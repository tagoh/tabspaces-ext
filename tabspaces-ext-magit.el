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
     (when-let* ((remote-url (magit-get "remote.origin.url")))
       (cond
        ;; SSH: git@github.com:user/project.git
        ((string-match ":\\([^/]+\\)/\\([^/\\.]+\\)\\(\\.git\\)?$" remote-url)
         (match-string 2 remote-url))
        ;; HTTPS: https://github.com/user/project.git
        ((string-match "/\\([^/\\.]+\\)\\(\\.git\\)?$" remote-url)
         (match-string 1 remote-url))))
     ;; Fallback to repository directory name
     (when-let* ((git-dir (magit-gitdir)))
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

(defun tabspaces-ext-magit--tab-name-for-path (worktree-path)
  "Return the tab name for WORKTREE-PATH.
Binds `default-directory' to WORKTREE-PATH and also passes it explicitly,
matching the calling convention of `tabspaces-ext-magit-tab-name-function'."
  (let ((default-directory worktree-path))
    (funcall tabspaces-ext-magit-tab-name-function worktree-path)))

(defun tabspaces-ext-magit--kill-worktree-buffers (project-root)
  "Kill all buffers associated with PROJECT-ROOT worktree."
  (let* ((root (file-name-as-directory (expand-file-name project-root)))
         (worktree-name (file-name-nondirectory
                         (directory-file-name project-root))))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (let ((buf-file (buffer-file-name buf))
              (buf-dir (ignore-errors
                         (buffer-local-value 'default-directory buf)))
              (buf-name (buffer-name buf)))
          (when (and (not (tabspaces-ext--is-system-buffer-p buf-name))
                     (not (eq (buffer-local-value 'major-mode buf)
                              'treemacs-mode))
                     ;; Compare against ROOT with a trailing slash so a sibling
                     ;; worktree sharing a name prefix (e.g. "main_feature" vs
                     ;; "main_feature-2") is not matched.
                     (or (and buf-file
                              (string-prefix-p root (expand-file-name buf-file)))
                         (and buf-dir
                              (string-prefix-p root (expand-file-name buf-dir)))
                         (and (string-prefix-p " *Old buffer" buf-name)
                              (string-match-p (regexp-quote worktree-name)
                                              buf-name))))
            (kill-buffer buf)))))))

;;; Magit integration

(defun tabspaces-ext-magit--evict-buffer-from-current-tab (buffer)
  "Remove BUFFER from the current tab's windows and buffer lists.
Called before relocating a worktree magit buffer to its own tab.

tabspaces tracks tab membership via the frame `buffer-list' parameter,
and Emacs records a buffer there as soon as it is displayed in the tab.
When `magit-status-setup-buffer' shows a sibling worktree's status in the
current (wrong) tab, that buffer leaks into the tab's `buffer-list' and
window layout, and is then persisted into the tab's session.  Evicting it
here keeps a worktree's magit buffer out of every tab but its own."
  ;; Restore the previous buffer in any window of this tab showing BUFFER,
  ;; so it neither stays visible nor gets re-recorded on tab return.
  (dolist (win (get-buffer-window-list buffer nil nil))
    (switch-to-prev-buffer win 'bury))
  ;; Drop it from the tab's buffer lists.
  (set-frame-parameter nil 'buffer-list
                       (delq buffer (frame-parameter nil 'buffer-list)))
  (set-frame-parameter nil 'buried-buffer-list
                       (delq buffer (frame-parameter nil 'buried-buffer-list))))

(defun tabspaces-ext-magit--worktree-ensure-tabspace ()
  "Automatically switch to or create tabspace for git worktree in magit buffers."
  (when (and (derived-mode-p 'magit-mode)
             default-directory
             (not (get-buffer "*tabspaces--placeholder*")))
    (let* ((project-root (expand-file-name default-directory))
           (current-tab-name (tabspaces-ext--get-current-tab-name))
           (current-tab-project
            (car (rassoc current-tab-name tabspaces-project-tab-map)))
           (expected-tab-name (tabspaces-ext-magit--tab-name-for-path project-root))
           (magit-buffer (current-buffer)))
      (when (and expected-tab-name
                 (not (string= current-tab-name expected-tab-name))
                 (or (null current-tab-project)
                     (file-exists-p
                      (expand-file-name ".git" current-tab-project))))
        ;; The buffer was just displayed in the current (origin) tab.  Evict it
        ;; before relocating so it does not leak into that tab's buffer list and
        ;; window layout (which would otherwise be saved into the origin
        ;; session and resurrected on restore).
        (tabspaces-ext-magit--evict-buffer-from-current-tab magit-buffer)
        ;; Switch to or create the appropriate tab
        (if (member expected-tab-name (tabspaces-ext--get-all-tab-names))
            ;; Switch to existing tab
            (progn
              (tab-bar-switch-to-tab expected-tab-name)
              (switch-to-buffer magit-buffer))
          ;; Create new tab
          (tab-bar-new-tab)
          (tab-bar-rename-tab expected-tab-name)
          (tabspaces-ext--add-project-tab-mapping project-root expected-tab-name)
          ;; Show the magit buffer in the new tab.  Previously this relied on
          ;; `tab-bar-new-tab' cloning the current buffer; with
          ;; `tab-bar-new-tab-choice' set to "*scratch*" the new tab would
          ;; otherwise land on *scratch* instead of the worktree status.
          (switch-to-buffer magit-buffer))
        (setq-local project-current-directory project-root)))))

(defun tabspaces-ext-magit--worktree-status-advice (orig-fun worktree)
  "Switch to or create dedicated tabspace when visiting a worktree.
This is advice for `magit-worktree-status'."
  (let* ((worktree-list (if (consp worktree)
                            worktree
                          (cl-find-if (lambda (wt) (string= (car wt) worktree))
                                      (magit-list-worktrees))))
         ;; Fall back to WORKTREE itself when the lookup misses (e.g. path
         ;; normalization differences), so we never pass nil to
         ;; `expand-file-name'.
         (worktree-path (expand-file-name (or (car worktree-list)
                                              (and (stringp worktree) worktree)
                                              default-directory)))
         (expected-tab-name (tabspaces-ext-magit--tab-name-for-path worktree-path))
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

(defun tabspaces-ext-magit--branch-rename-advice (orig-fun old new &optional force)
  "Update tab name and project mapping after renaming a branch.
This is advice for `magit-branch-rename'."
  (let* ((project-name (tabspaces-ext-magit--get-git-project-name))
         (old-tab-name (when project-name (format "%s@%s" project-name old))))
    (funcall orig-fun old new force)
    (when (and old-tab-name
               (member old-tab-name (tabspaces-ext--get-all-tab-names)))
      (let ((new-tab-name (format "%s@%s" project-name new)))
        (let ((tab-index (tabspaces-ext--find-tab-index old-tab-name)))
          (when tab-index
            (tab-bar-rename-tab new-tab-name (1+ tab-index))))
        (let ((mapping (rassoc old-tab-name tabspaces-project-tab-map)))
          (when mapping
            (setcdr mapping new-tab-name)))))))

(defun tabspaces-ext-magit--worktree-delete-advice (orig-fun &rest args)
  "Delete worktree and clean up associated tab and buffers.
This is advice for `magit-worktree-delete'.
Cleanup is deferred so that pending file-notify events are dispatched
with valid callbacks before buffers are killed."
  (let* ((worktree-path (expand-file-name (car args)))
         (target-tab-name (tabspaces-ext-magit--tab-name-for-path worktree-path)))

    (if (or (not target-tab-name)
            (string= target-tab-name "Default"))
        (apply orig-fun args)

      ;; Delete worktree (synchronous directory removal + async git prune)
      (apply orig-fun args)

      ;; Defer cleanup to let pending file-notify events be processed
      ;; with their original valid callbacks before we kill buffers.
      (run-at-time 0 nil
                   #'tabspaces-ext-magit--worktree-post-delete-cleanup
                   worktree-path target-tab-name))))

(defun tabspaces-ext-magit--worktree-post-delete-cleanup (worktree-path target-tab-name)
  "Clean up buffers, tab, and treemacs state after worktree deletion."
  (condition-case err
      (progn
        (tabspaces-ext-magit--kill-worktree-buffers worktree-path)

        (when (fboundp 'project-forget-project)
          (project-forget-project worktree-path))

        (when (and target-tab-name
                   (stringp target-tab-name)
                   (not (string-empty-p target-tab-name))
                   (member target-tab-name (tabspaces-ext--get-all-tab-names)))
          (let ((tab-bar-tab-prevent-close-functions nil))
            (tab-bar-close-tab-by-name target-tab-name)))

        (when (and (featurep 'treemacs)
                   (fboundp 'tabspaces-ext-treemacs--sync-with-tabspaces))
          (tabspaces-ext-treemacs--sync-with-tabspaces))

        (when (and (derived-mode-p 'magit-mode)
                   (ignore-errors (magit-gitdir)))
          (ignore-errors (magit-refresh))))
    (error (message "tabspaces-ext: worktree cleanup error: %S" err))))

;;; Tabspaces integration

(defun tabspaces-ext-magit--generate-descriptive-tab-name-advice (orig-fun project-path existing-tab-names)
  "Use 'project@branch' format for git repositories.
This is advice for `tabspaces-generate-descriptive-tab-name'."
  (if (get-buffer "*tabspaces--placeholder*")
      ;; Don't interfere during session restoration
      (funcall orig-fun project-path existing-tab-names)
    (let ((default-directory project-path))
      (if (and (file-exists-p (expand-file-name ".git" project-path))
               (fboundp 'magit-toplevel)
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
  "Clean stale mappings and preserve window layout during session save."
  (tabspaces-ext-magit--clean-non-git-mappings)
  (apply #'window-state-plus-advice-save-session orig-fun args))

(defun tabspaces-ext-magit--save-project-session-advice (&rest _)
  "Clean stale mappings before per-project session save."
  (tabspaces-ext-magit--clean-non-git-mappings))

(defun tabspaces-ext-magit--clean-non-git-mappings ()
  "Remove project@branch entries from `tabspaces-project-tab-map' for non-git dirs."
  (when (boundp 'tabspaces-project-tab-map)
    (setq tabspaces-project-tab-map
          (cl-remove-if
           (lambda (entry)
             (and (string-match-p "@" (cdr entry))
                  (not (file-exists-p
                        (expand-file-name
                         ".git" (expand-file-name (car entry)))))))
           tabspaces-project-tab-map))))

(defun tabspaces-ext-magit--repair-project-tab-mappings ()
  "Repair `tabspaces-project-tab-map' after session restore.
Two passes:
1. Remove stale project@branch mappings whose project root has no .git.
2. Re-populate missing mappings for existing project@branch tabs.
The magit advice on `tabspaces-generate-descriptive-tab-name' is bypassed
during session restore (placeholder buffer check), so project@branch tabs
restored from the global session file lose their mapping.  Without a mapping,
`tabspaces-save-all-project-sessions' treats them as non-project tabs,
perpetuating the loss across restarts."
  (when (and (boundp 'tabspaces-project-tab-map)
             (fboundp 'magit-toplevel))
    (tabspaces-ext-magit--clean-non-git-mappings)
    ;; `project--list' is the sentinel symbol `unset' until project.el has
    ;; read the saved project list -- which may not have happened yet during
    ;; early startup session restore.  Iterating it in that state signals
    ;; (wrong-type-argument listp unset), which aborts this `:after' advice
    ;; and trips tabspaces' "session restore failed" handler.  Force a read
    ;; when possible, then guard defensively so a non-list value is a no-op.
    (when (fboundp 'project--ensure-read-project-list)
      (ignore-errors (project--ensure-read-project-list)))
    (when (and (boundp 'project--list) (listp project--list))
      ;; Add missing mappings for git project tabs.  Iterate the project list
      ;; once (computing each project's expected tab name at most once, since
      ;; that shells out to git) rather than re-scanning all projects per tab,
      ;; and stop early once every unmapped tab has been resolved.
      (let* ((mapped-tabs (mapcar #'cdr tabspaces-project-tab-map))
             (unmapped (cl-remove-if-not
                        (lambda (name)
                          (and (string-match-p "@" name)
                               (not (member name mapped-tabs))))
                        (tabspaces-ext--get-all-tab-names))))
        (when unmapped
          (catch 'done
            (dolist (project-entry project--list)
              (let ((project-root (expand-file-name (car project-entry))))
                (when (and (file-directory-p project-root)
                           (file-exists-p (expand-file-name ".git" project-root)))
                  (let ((default-directory project-root))
                    (when (condition-case nil (magit-toplevel) (error nil))
                      (let ((expected (tabspaces-ext-magit--tab-name-for-path
                                       project-root)))
                        (when (member expected unmapped)
                          (tabspaces-ext--add-project-tab-mapping
                           project-root expected)
                          (setq unmapped (delete expected unmapped))
                          (unless unmapped (throw 'done t)))))))))))))))

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
  "Register the magit-status buffer kind for session restoration.
This function is called early during tabspaces-ext initialization,
before magit is loaded, to support session restoration.

Without a handler, magit-status buffers (no file, no restore-fn) are
dropped from the session on save, leaving an unrestorable window on
restore.  Persist their repository directory and rebuild the status
buffer from it via `magit-status-setup-buffer'."
  (tabspaces-register-buffer-kind
   'magit-status
   (lambda (b)
     (when (eq (buffer-local-value 'major-mode b) 'magit-status-mode)
       (list :kind 'magit-status
             :dir (buffer-local-value 'default-directory b))))
   (lambda (rec)
     (when-let* ((dir (plist-get rec :dir)))
       (when (and (featurep 'magit)
                  (fboundp 'magit-status-setup-buffer)
                  (file-directory-p dir))
         ;; Rebuild without stealing the window; tabspaces restores the
         ;; layout afterwards via `window-state-put'.
         (save-window-excursion
           (let ((default-directory dir))
             (ignore-errors (magit-status-setup-buffer dir)))))))))

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
    (advice-add 'magit-branch-rename :around #'tabspaces-ext-magit--branch-rename-advice)
    ;; Tabspaces integration
    (advice-add 'tabspaces-generate-descriptive-tab-name :around
                #'tabspaces-ext-magit--generate-descriptive-tab-name-advice)
    (advice-add 'tabspaces-save-session :around
                #'tabspaces-ext-magit--save-session-advice)
    (advice-add 'tabspaces-save-all-project-sessions :before
                #'tabspaces-ext-magit--save-project-session-advice)
    (advice-add 'tabspaces-save-current-project-session :before
                #'tabspaces-ext-magit--save-project-session-advice)
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
    (advice-remove 'magit-branch-rename #'tabspaces-ext-magit--branch-rename-advice)
    ;; Tabspaces integration
    (advice-remove 'tabspaces-generate-descriptive-tab-name
                   #'tabspaces-ext-magit--generate-descriptive-tab-name-advice)
    (advice-remove 'tabspaces-save-session
                   #'tabspaces-ext-magit--save-session-advice)
    (advice-remove 'tabspaces-save-all-project-sessions
                   #'tabspaces-ext-magit--save-project-session-advice)
    (advice-remove 'tabspaces-save-current-project-session
                   #'tabspaces-ext-magit--save-project-session-advice)
    (advice-remove 'tabspaces-restore-session
                   #'tabspaces-ext-magit--restore-session-advice)
    (setq tabspaces-ext-magit--active nil)))

(provide 'tabspaces-ext-magit)

;;; tabspaces-ext-magit.el ends here
