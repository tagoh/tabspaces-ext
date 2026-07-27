;;; tabspaces-ext-magit.el --- Magit integration for tabspaces-ext -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira TAGOH

;; Author: Akira TAGOH <akira@tagoh.org>
;; URL: https://github.com/tagoh/tabspaces-ext
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (magit "3.0.0") (tabspaces "1.0") (tabspaces-ext "1.0"))
;; Keywords: vc, tools, convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Magit integration module for tabspaces-ext.
;;
;; This module implements the tabspaces-ext integration contract:
;;
;; - `tabspaces-ext-magit-register-buffer-kind' (autoloaded)
;;   No-op for magit since magit buffers don't need special session
;;   restoration. Provided for contract consistency.
;;
;; - `tabspaces-ext-magit-setup' (autoloaded)
;;   Called after magit loads. Sets up hooks and advice for git-aware
;;   tab management. Safe to require magit features here.
;;
;; - `tabspaces-ext-magit-teardown'
;;   Removes all hooks and advice installed by setup.
;;
;; Features:
;; - Git-aware tab naming (project@branch format)
;; - Automatic tabspace creation for git repositories
;; - Worktree-aware tab management
;; - Automatic tab switching when visiting worktrees
;; - Clean tab/buffer cleanup when deleting worktrees
;; - Detached HEAD support (during rebases)
;; - Project mapping persistence across sessions
;;
;; Integration Details:
;;
;; This module advises `tabspaces-generate-descriptive-tab-name' to use
;; "project@branch" format for git repositories instead of the default
;; directory-based naming.
;;
;; It also advises magit worktree commands:
;; - `magit-worktree-status' - switches/creates tabs when visiting worktrees
;; - `magit-worktree-delete' - cleans up tabs and buffers
;;
;; Session restoration uses window-state-plus for proper layout handling.
;;
;; Customization:
;;
;; Set `tabspaces-ext-magit-tab-name-function' to customize tab naming:
;;
;;   (setq tabspaces-ext-magit-tab-name-function #'my-custom-tab-name)
;;
;; Acknowledgments:
;;   This module was created with the assistance of Claude (Anthropic).

;;; Code:

(require 'tabspaces)
(require 'tabspaces-ext)
(require 'window-state-plus)

;; Don't require magit here - it will be loaded via with-eval-after-load

;;; Customization

(defcustom tabspaces-ext-magit-tab-name-function #'tabspaces-ext-magit-default-tab-name
  "Function to generate tab name for a git repository.
The function is called with optional WORKTREE-PATH argument and should
return a string to use as the tab name."
  :type 'function
  :group 'tabspaces-ext)

;;; Git helper functions

(defun tabspaces-ext-magit--get-git-project-name ()
  "Get project name from git remote URL or directory name.
Works for both regular repos and worktrees."
  (when (and (fboundp 'magit-get) (fboundp 'magit-gitdir))
    (or
     ;; Try to extract from remote URL
     (when-let ((remote-url (magit-get "remote.origin.url")))
       (cond
        ;; SSH: git@github.com:user/project.git
        ((string-match ":\\([^/]+\\)/\\([^/\\.]+\\)\\(\\.git\\)?$" remote-url)
         (match-string 2 remote-url))
        ;; HTTPS: https://github.com/user/project.git
        ((string-match "/\\([^/\\.]+\\)\\(\\.git\\)?$" remote-url)
         (match-string 1 remote-url))))
     ;; Fallback to repository directory name
     (when-let ((git-dir (magit-gitdir)))
       (file-name-nondirectory
        (directory-file-name
         (if (string-match "/\\.git/?$" git-dir)
             (replace-regexp-in-string "/\\.git/?$" "" git-dir)
           default-directory)))))))

(defun tabspaces-ext-magit--get-git-branch-name (&optional worktree-path)
  "Get current git branch name.
For detached HEAD (e.g., during rebase), tries to infer from git state
or WORKTREE-PATH directory name."
  (let ((default-directory (or worktree-path default-directory)))
    (or
     ;; Try normal branch name
     (and (fboundp 'magit-get-current-branch)
          (magit-get-current-branch))
     ;; If rebasing, get branch from rebase state
     (when (file-exists-p ".git/rebase-merge/head-name")
       (let ((head-name (with-temp-buffer
                          (insert-file-contents ".git/rebase-merge/head-name")
                          (string-trim (buffer-string)))))
         (when (string-match "refs/heads/\\(.+\\)" head-name)
           (match-string 1 head-name))))
     ;; Extract from worktree directory name: "main_branch-name" -> "branch-name"
     (when worktree-path
       (let ((dir-name (file-name-nondirectory
                        (directory-file-name worktree-path))))
         (when (string-match "^[^_]+_\\(.+\\)$" dir-name)
           (match-string 1 dir-name)))))))

(defun tabspaces-ext-magit-default-tab-name (&optional worktree-path)
  "Get tab name in 'project@branch' format.
Optional WORKTREE-PATH for worktree-specific branch detection."
  (let* ((default-directory (or worktree-path default-directory))
         (project-name (tabspaces-ext-magit--get-git-project-name))
         (branch-name (tabspaces-ext-magit--get-git-branch-name worktree-path)))
    (if (and project-name branch-name)
        (format "%s@%s" project-name branch-name)
      (or project-name
          (file-name-nondirectory
           (directory-file-name default-directory))))))

(defun tabspaces-ext-magit--kill-worktree-buffers (project-root)
  "Kill all buffers associated with PROJECT-ROOT worktree."
  (let ((worktree-name (file-name-nondirectory
                        (directory-file-name project-root))))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (let ((buf-file (buffer-file-name buf))
              (buf-dir (ignore-errors
                         (buffer-local-value 'default-directory buf)))
              (buf-name (buffer-name buf)))
          ;; Kill buffers from deleted worktree (but not system buffers)
          (when (and (not (tabspaces-ext--is-system-buffer-p buf-name))
                     (or (and buf-file
                              (string-prefix-p project-root
                                               (expand-file-name buf-file)))
                         (and buf-dir
                              (string-prefix-p project-root
                                               (expand-file-name buf-dir)))
                         (and (string-prefix-p " *Old buffer" buf-name)
                              (string-match-p (regexp-quote worktree-name)
                                              buf-name))))
            (kill-buffer buf)))))))

;;; Magit integration

(defun tabspaces-ext-magit--worktree-ensure-tabspace ()
  "Automatically switch to or create tabspace for git worktree in magit buffers."
  (when (and (derived-mode-p 'magit-mode)
             default-directory
             (not (get-buffer "*tabspaces--placeholder*")))
    (let* ((project-root (expand-file-name default-directory))
           (current-tab-name (tabspaces-ext--get-current-tab-name))
           (expected-tab-name (funcall tabspaces-ext-magit-tab-name-function))
           (magit-buffer (current-buffer)))
      (when (and expected-tab-name
                 (not (string= current-tab-name expected-tab-name)))
        ;; Switch to or create the appropriate tab
        (if (member expected-tab-name (tabspaces-ext--get-all-tab-names))
            ;; Switch to existing tab
            (progn
              (tab-bar-switch-to-tab expected-tab-name)
              (switch-to-buffer magit-buffer))
          ;; Create new tab
          (tab-bar-new-tab)
          (tab-bar-rename-tab expected-tab-name)
          (tabspaces-ext--add-project-tab-mapping project-root expected-tab-name))
        (setq-local project-current-directory project-root)))))

(defun tabspaces-ext-magit--worktree-status-advice (orig-fun worktree)
  "Switch to or create dedicated tabspace when visiting a worktree.
This is advice for `magit-worktree-status'."
  (let* ((worktree-list (if (consp worktree)
                            worktree
                          (cl-find-if (lambda (wt) (string= (car wt) worktree))
                                      (magit-list-worktrees))))
         (worktree-path (expand-file-name (car worktree-list)))
         (expected-tab-name
          (let ((default-directory worktree-path))
            (funcall tabspaces-ext-magit-tab-name-function worktree-path)))
         (current-tab-name (tabspaces-ext--get-current-tab-name)))

    ;; If already in correct tab or no expected name, just call original function
    (if (or (not expected-tab-name)
            (string= current-tab-name expected-tab-name))
        (funcall orig-fun worktree)
      ;; Handle tab switching/creation
      (if (member expected-tab-name (tabspaces-ext--get-all-tab-names))
          ;; Tab exists: switch and show status
          (progn
            (tab-bar-switch-to-tab expected-tab-name)
            (let ((default-directory worktree-path))
              (magit-status-setup-buffer)))
        ;; Tab doesn't exist: create and visit
        (tab-bar-new-tab)
        (tab-bar-rename-tab expected-tab-name)
        (tabspaces-ext--add-project-tab-mapping worktree-path expected-tab-name)
        (when (fboundp 'project-remember-project)
          (project-remember-project (project--find-in-directory worktree-path)))
        ;; Temporarily remove hook to prevent duplicate tab
        (remove-hook 'magit-post-display-buffer-hook
                     #'tabspaces-ext-magit--worktree-ensure-tabspace)
        (unwind-protect
            (funcall orig-fun worktree)
          (add-hook 'magit-post-display-buffer-hook
                    #'tabspaces-ext-magit--worktree-ensure-tabspace))))))

(defun tabspaces-ext-magit--worktree-delete-advice (orig-fun &rest args)
  "Delete worktree and clean up associated tab and buffers.
This is advice for `magit-worktree-delete'."
  (let* ((worktree-path (expand-file-name (car args)))
         (target-tab-name
          (let ((default-directory worktree-path))
            (funcall tabspaces-ext-magit-tab-name-function worktree-path))))

    (if (or (not target-tab-name)
            (string= target-tab-name "Default"))
        ;; No special tab handling needed
        (apply orig-fun args)

      ;; Delete worktree and manage tab
      (apply orig-fun args)

      ;; Clean up buffers associated with the deleted worktree
      (tabspaces-ext-magit--kill-worktree-buffers worktree-path)

      ;; Forget project
      (when (fboundp 'project-forget-project)
        (project-forget-project worktree-path))

      ;; Close the tab
      (when (and target-tab-name
                 (stringp target-tab-name)
                 (not (string-empty-p target-tab-name)))
        (let ((tab-bar-tab-prevent-close-functions nil))
          (tab-bar-close-tab-by-name target-tab-name)))

      ;; Refresh magit if still in a magit buffer
      (when (and (derived-mode-p 'magit-mode) (magit-gitdir))
        (magit-refresh)))))

;;; Tabspaces integration

(defun tabspaces-ext-magit--generate-descriptive-tab-name-advice (orig-fun project-path existing-tab-names)
  "Use 'project@branch' format for git repositories.
This is advice for `tabspaces-generate-descriptive-tab-name'."
  (if (get-buffer "*tabspaces--placeholder*")
      ;; Don't interfere during session restoration
      (funcall orig-fun project-path existing-tab-names)
    (let ((default-directory project-path))
      (if (and (fboundp 'magit-toplevel)
               (condition-case nil (magit-toplevel) (error nil)))
          ;; Git repository: use project@branch format
          (let ((tab-name (funcall tabspaces-ext-magit-tab-name-function)))
            (tabspaces-ext--add-project-tab-mapping project-path tab-name)
            tab-name)
        ;; Not a git repo: use default behavior
        (funcall orig-fun project-path existing-tab-names)))))

(defun tabspaces-ext-magit--cleanup-placeholder-tabs ()
  "Close placeholder tabs left after session restoration.
Fixes tabspaces bug where placeholder tabs aren't automatically cleaned up."
  (dolist (tab (funcall tab-bar-tabs-function))
    (let ((tab-name (alist-get 'name tab)))
      (when (and tab-name
                 (stringp tab-name)
                 (string-prefix-p "*tabspaces--" tab-name))
        (let ((tab-bar-tab-prevent-close-functions nil))
          (tab-bar-close-tab-by-name tab-name))))))

(defun tabspaces-ext-magit--save-session-advice (orig-fun &rest args)
  "Preserve window layout during tabspaces session save.
This uses window-state-plus to avoid layout corruption with side windows."
  (window-state-plus-advice-save-session orig-fun args))

(defun tabspaces-ext-magit--repair-project-tab-mappings ()
  "Re-populate missing `tabspaces-project-tab-map' entries for existing tabs.
The magit advice on `tabspaces-generate-descriptive-tab-name' is bypassed
during session restore (placeholder buffer check), so project@branch tabs
restored from the global session file lose their mapping.  Without a mapping,
`tabspaces-save-all-project-sessions' treats them as non-project tabs,
perpetuating the loss across restarts.
This function scans the known project list to find matching roots and
restores the missing entries."
  (when (and (boundp 'tabspaces-project-tab-map)
             (boundp 'project--list)
             (fboundp 'magit-toplevel))
    (let ((tab-names (tabspaces-ext--get-all-tab-names))
          (mapped-tabs (mapcar #'cdr tabspaces-project-tab-map)))
      (dolist (tab-name tab-names)
        (when (and (string-match-p "@" tab-name)
                   (not (member tab-name mapped-tabs)))
          (catch 'found
            (dolist (project-entry project--list)
              (let ((project-root (expand-file-name (car project-entry))))
                (when (file-directory-p project-root)
                  (let ((default-directory project-root))
                    (when (condition-case nil (magit-toplevel) (error nil))
                      (let ((expected (funcall tabspaces-ext-magit-tab-name-function)))
                        (when (string= expected tab-name)
                          (tabspaces-ext--add-project-tab-mapping
                           project-root tab-name)
                          (throw 'found t))))))))))))))

(defun tabspaces-ext-magit--restore-session-advice (&rest _)
  "Cleanup after tabspaces session restoration."
  (tabspaces-ext-magit--cleanup-placeholder-tabs)
  (when (boundp 'tabspaces-project-tab-map)
    (setq tabspaces-project-tab-map
          (delete-dups tabspaces-project-tab-map)))
  (tabspaces-ext-magit--repair-project-tab-mappings))

;;; Setup/teardown functions

(defvar tabspaces-ext-magit--active nil
  "Track whether magit integration is active.")

;;;###autoload
(defun tabspaces-ext-magit-register-buffer-kind ()
  "Register buffer kinds for magit integration.
Magit doesn't need custom buffer kinds, so this is a no-op for consistency."
  ;; No-op: magit buffers don't need special session restoration
  nil)

;;;###autoload
(defun tabspaces-ext-magit-setup ()
  "Set up Magit integration for tabspaces."
  (unless tabspaces-ext-magit--active
    ;; Require magit features (safe now since this runs after magit loads)
    (require 'magit-worktree)
    ;; Magit integration
    (add-hook 'magit-post-display-buffer-hook #'tabspaces-ext-magit--worktree-ensure-tabspace)
    (advice-add 'magit-worktree-status :around #'tabspaces-ext-magit--worktree-status-advice)
    (advice-add 'magit-worktree-delete :around #'tabspaces-ext-magit--worktree-delete-advice)
    ;; Tabspaces integration
    (advice-add 'tabspaces-generate-descriptive-tab-name :around
                #'tabspaces-ext-magit--generate-descriptive-tab-name-advice)
    (advice-add 'tabspaces-save-session :around
                #'tabspaces-ext-magit--save-session-advice)
    (advice-add 'tabspaces-restore-session :after
                #'tabspaces-ext-magit--restore-session-advice)
    (setq tabspaces-ext-magit--active t)
    ;; Repair mappings lost during session restore (the :after advice on
    ;; tabspaces-restore-session is not yet active when the initial startup
    ;; restore runs, because magit hasn't loaded yet at that point).
    (tabspaces-ext-magit--repair-project-tab-mappings)))

;;;###autoload
(defun tabspaces-ext-magit-teardown ()
  "Tear down Magit integration for tabspaces."
  (when tabspaces-ext-magit--active
    ;; Magit integration
    (remove-hook 'magit-post-display-buffer-hook #'tabspaces-ext-magit--worktree-ensure-tabspace)
    (advice-remove 'magit-worktree-status #'tabspaces-ext-magit--worktree-status-advice)
    (advice-remove 'magit-worktree-delete #'tabspaces-ext-magit--worktree-delete-advice)
    ;; Tabspaces integration
    (advice-remove 'tabspaces-generate-descriptive-tab-name
                   #'tabspaces-ext-magit--generate-descriptive-tab-name-advice)
    (advice-remove 'tabspaces-save-session
                   #'tabspaces-ext-magit--save-session-advice)
    (advice-remove 'tabspaces-restore-session
                   #'tabspaces-ext-magit--restore-session-advice)
    (setq tabspaces-ext-magit--active nil)))

(provide 'tabspaces-ext-magit)

;;; tabspaces-ext-magit.el ends here
