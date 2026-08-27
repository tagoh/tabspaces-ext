;;; tabspaces-ext.el --- Enhanced tabspaces integrations -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira TAGOH

;; Author: Akira TAGOH <akira@tagoh.org>
;; URL: https://github.com/tagoh/tabspaces-ext
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (tabspaces "1.0"))
;; Keywords: convenience, frames

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

;; Enhanced integrations for tabspaces with modular, optional support for
;; various packages. Uses a generic, data-driven integration loader for
;; easy extensibility.
;;
;; Core Features:
;; - Generic integration loader - data-driven architecture
;; - Automatic buffer cleanup - kills tab-unique buffers when closing tabs
;; - Early buffer kind registration - supports session restoration
;; - Enhanced window state handling - via window-state-plus
;; - Common helper functions - shared utilities for tab management
;;
;; Optional Integrations:
;; Each integration is completely optional and loads only when enabled:
;;
;; - Magit (`tabspaces-ext-magit'):
;;   Git-aware tab naming (project@branch), worktree management,
;;   automatic tab switching, clean cleanup on worktree deletion
;;
;; - Treemacs (`tabspaces-ext-treemacs'):
;;   Per-tab workspace sync, automatic sync on tab switch,
;;   session restoration support
;;
;; - Popterm (`tabspaces-ext-popterm'):
;;   Separate terminal instances per tab, tab-specific buffer naming,
;;   automatic window sync, layout fixes
;;
;; Architecture:
;;
;; The package uses a generic, data-driven integration loader. Integration
;; modules are discovered from `tabspaces-ext--integration-alist', eliminating
;; hardcoded integration names from the core.
;;
;; Each integration module must provide:
;;
;; 1. `<module>-register-buffer-kind' (autoloaded, optional)
;;    - Called early during initialization
;;    - No dependencies on the target package
;;    - Registers buffer kinds for session restoration
;;
;; 2. `<module>-setup' (autoloaded, required)
;;    - Called after the target package loads
;;    - Can safely require package features
;;    - Sets up hooks, advice, and keybindings
;;
;; 3. `<module>-teardown' (required)
;;    - Removes all hooks and advice
;;    - Called when integration is disabled
;;
;; Installation:
;;
;; IMPORTANT: tabspaces-ext must load AFTER tabspaces.
;;
;;   (use-package tabspaces-ext
;;     :after tabspaces  ; Required!
;;     :custom
;;     (tabspaces-ext-magit t)
;;     (tabspaces-ext-treemacs t)
;;     (tabspaces-ext-popterm t)
;;     :config
;;     (tabspaces-ext-mode 1))
;;
;; Adding New Integrations:
;;
;; 1. Create module file: `tabspaces-ext-newpackage.el'
;; 2. Follow the module contract (register-buffer-kind, setup, teardown)
;; 3. Update `tabspaces-ext--integration-alist' in this file
;; 4. Add a customization variable (`tabspaces-ext-newpackage')
;;
;; The generic loader handles the rest automatically.
;;
;; Acknowledgments:
;;   This package was created with the assistance of Claude (Anthropic).

;;; Code:

(require 'cl-lib)
(require 'tabspaces)
(require 'window-state-plus)

;;; Customization

(defgroup tabspaces-ext nil
  "Enhanced tabspaces integrations."
  :group 'tabspaces
  :group 'convenience)

(defcustom tabspaces-ext-magit nil
  "Enable Magit integration for tabspaces.
When non-nil, enables git-aware tab naming and worktree management."
  :type 'boolean
  :group 'tabspaces-ext)

(defcustom tabspaces-ext-treemacs nil
  "Enable Treemacs integration for tabspaces.
When non-nil, syncs treemacs workspace to current tab's project."
  :type 'boolean
  :group 'tabspaces-ext)

(defcustom tabspaces-ext-popterm nil
  "Enable Popterm integration for tabspaces.
When non-nil, creates separate popterm instances per tab."
  :type 'boolean
  :group 'tabspaces-ext)

(defcustom tabspaces-ext-shared-buffers nil
  "Matchers for buffers that must never be saved into any session.
Some buffers are global by nature -- e.g. a central org journal reached
from any project -- and, because they live outside every project root,
the cross-workspace leak filter cannot recognize them as foreign.  Such
buffers otherwise leak into whatever tab happens to be current when they
are visited, and are then persisted into that tab's session file.

Each element is a matcher applied to a session buffer record (a file
path string or a plist with `:dir'/`:name'):

  - a string   -- matched as a regexp against the record's file path
                  and against its directory;
  - a function -- called with the record, non-nil means \"shared\".

Records matching any element are dropped from every session save,
regardless of which tab is being saved."
  :type '(repeat (choice (regexp :tag "Path regexp")
                         (function :tag "Predicate")))
  :group 'tabspaces-ext)

(defcustom tabspaces-ext-project-directory-commands nil
  "Commands whose `default-directory' is pinned to the tab's project root.
Direction-sensitive commands resolve `default-directory' from whatever
buffer happens to be current.  When a global buffer -- e.g. a central org
journal reached from any project -- is current in a project tab, such a
command would run against that buffer's directory instead of the tab's
project.  Each command listed here is advised so that, while it runs in a
project tab, `default-directory' is bound to that tab's mapped project
root instead.  On the `Default' tab or any tab with no mapped project the
command sees the ambient `default-directory' unchanged.

Takes effect when `tabspaces-ext-mode' is enabled; changing it while the
mode is on requires toggling the mode to re-install the advice."
  :type '(repeat function)
  :group 'tabspaces-ext)

;;; Helper functions

(defun tabspaces-ext--add-project-tab-mapping (project-root tab-name)
  "Add PROJECT-ROOT to TAB-NAME mapping without creating duplicates."
  (let ((existing (assoc project-root tabspaces-project-tab-map)))
    (if existing
        ;; Update existing mapping if different
        (unless (string= (cdr existing) tab-name)
          (message "Updating tab mapping: %s -> %s (was %s)" project-root tab-name (cdr existing))
          (setf (cdr existing) tab-name))
      ;; Remove stale entries that map a different root to the same tab name
      (setq tabspaces-project-tab-map
            (cl-remove-if (lambda (entry)
                            (and (string= (cdr entry) tab-name)
                                 (not (string= (car entry) project-root))))
                          tabspaces-project-tab-map))
      ;; Add new mapping
      (message "Adding new tab mapping: %s -> %s" project-root tab-name)
      (push (cons project-root tab-name) tabspaces-project-tab-map))))

(defun tabspaces-ext--get-all-tab-names ()
  "Get list of all tab names."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

(defun tabspaces-ext--get-current-tab-name ()
  "Get current tab name."
  (alist-get 'name (tab-bar--current-tab)))

(defun tabspaces-ext--find-tab-index (tab-name)
  "Find the index of tab with TAB-NAME."
  (cl-position-if
   (lambda (tab) (string= (alist-get 'name tab) tab-name))
   (funcall tab-bar-tabs-function)))

(defun tabspaces-ext--is-system-buffer-p (buffer-name)
  "Return t if BUFFER-NAME is a system buffer."
  (or (member buffer-name '("*scratch*" "*Messages*"))
      (string-prefix-p " *Minibuf" buffer-name)))

(defun tabspaces-ext--switch-or-create-tab (tab-name project-root)
  "Switch to existing tab TAB-NAME or create new one for PROJECT-ROOT."
  (if (member tab-name (tabspaces-ext--get-all-tab-names))
      (tab-bar-switch-to-tab tab-name)
    (tab-bar-new-tab)
    (tab-bar-rename-tab tab-name)
    (tabspaces-ext--add-project-tab-mapping project-root tab-name)))

;;; Cross-workspace buffer leak filter

;; Tabspace-unaware commands (e.g. `switch-to-buffer'/`find-file' issued by
;; packages like org-capture based tools) open their buffers into whatever
;; tab is current, which adds them to that tab's `buffer-list'.  When a
;; project tab's session is then saved, those foreign buffers are persisted
;; into it -- and, since restore re-materializes every record back into the
;; tab, the leak is self-perpetuating across restarts.  These helpers drop,
;; at save time, any record that demonstrably belongs to *another* mapped
;; project so a project tab's session only keeps its own buffers.

(defun tabspaces-ext--tab-project-root (tab-name)
  "Return the project root mapped to TAB-NAME, or nil.
Mirrors `tabspaces--get-project-for-tab' numbered-suffix handling so a
tab like \"proj<2>\" resolves to the same root as \"proj\"."
  (when (and tab-name (boundp 'tabspaces-project-tab-map))
    (or (car (rassoc tab-name tabspaces-project-tab-map))
        (when (string-match "\\`\\(.+\\)<[0-9]+>\\'" tab-name)
          (car (rassoc (match-string 1 tab-name) tabspaces-project-tab-map))))))

(defun tabspaces-ext--record-directory (rec)
  "Return the directory associated with session buffer record REC, or nil.
REC is either a file path string (legacy format) or a plist with `:dir'."
  (cond
   ((stringp rec) (file-name-directory rec))
   ((consp rec) (plist-get rec :dir))))

(defun tabspaces-ext--record-file (rec)
  "Return the file path of session buffer record REC, or nil.
REC is either a file path string (legacy format) or a plist; only string
records carry a concrete file path."
  (when (stringp rec) rec))

(defun tabspaces-ext--shared-record-p (rec)
  "Return non-nil if REC matches any `tabspaces-ext-shared-buffers' matcher.
String matchers are treated as regexps tested against the record's file
path and its directory; function matchers are called with REC directly."
  (let ((file (tabspaces-ext--record-file rec))
        (dir (tabspaces-ext--record-directory rec)))
    (cl-some
     (lambda (matcher)
       (cond
        ((functionp matcher) (funcall matcher rec))
        ((stringp matcher)
         (or (and file (string-match-p matcher file))
             (and dir (string-match-p matcher dir))))))
     tabspaces-ext-shared-buffers)))

(defun tabspaces-ext--foreign-record-p (rec tab-root)
  "Return non-nil if REC belongs to a project other than TAB-ROOT.
A record is foreign when its directory lies under some project root in
`tabspaces-project-tab-map' that is not TAB-ROOT.  Records with no
directory, records under TAB-ROOT, and records under no known project
\(e.g. a loose file outside every workspace) are kept."
  (when tab-root
    (let ((dir (tabspaces-ext--record-directory rec)))
      (when (stringp dir)
        (let ((edir (expand-file-name dir))
              (eroot (file-name-as-directory (expand-file-name tab-root))))
          (and (not (string-prefix-p eroot edir))
               (cl-some
                (lambda (entry)
                  (let ((r (file-name-as-directory (expand-file-name (car entry)))))
                    (and (not (string= r eroot))
                         (string-prefix-p r edir))))
                tabspaces-project-tab-map)))))))

(defun tabspaces-ext--filter-foreign-buffers (records)
  "Remove RECORDS that must not be persisted into a session.
Advice (`:filter-return') for `tabspaces--store-buffers'.  Runs two
passes:

  1. Drop shared buffers (`tabspaces-ext-shared-buffers') unconditionally,
     so a global buffer like a central org journal never lands in any
     session file, whatever tab is being saved.
  2. Drop foreign buffers -- records under another mapped project root --
     but only when the tab being saved maps to a project.  The current
     tab during a save is the tab being saved, so its mapped root
     determines what counts as foreign.  Non-project tabs (no mapped
     root) keep every remaining record."
  (let ((records (if tabspaces-ext-shared-buffers
                     (cl-remove-if #'tabspaces-ext--shared-record-p records)
                   records))
        (tab-root (tabspaces-ext--tab-project-root
                   (tabspaces-ext--get-current-tab-name))))
    (if tab-root
        (cl-remove-if (lambda (rec)
                        (tabspaces-ext--foreign-record-p rec tab-root))
                      records)
      records)))

;;; Project-directory pinning for direction-sensitive commands

(defun tabspaces-ext-current-project-root ()
  "Return the project root mapped to the current tab, or nil.
Nil for the `Default' tab or any tab with no mapped project."
  (tabspaces-ext--tab-project-root (tabspaces-ext--get-current-tab-name)))

(defun tabspaces-ext--with-project-directory (orig-fn &rest args)
  "Call ORIG-FN with `default-directory' pinned to the tab's project root.
Advice (`:around') for the commands in
`tabspaces-ext-project-directory-commands'.  When the current tab maps to
a project, ORIG-FN runs with `default-directory' bound to that root so it
ignores whichever buffer -- e.g. a global org journal -- happens to be
current.  Off a project tab the ambient `default-directory' is used."
  (let* ((root (tabspaces-ext-current-project-root))
         (default-directory (if root
                                (file-name-as-directory (expand-file-name root))
                              default-directory)))
    (apply orig-fn args)))

(defun tabspaces-ext--install-project-directory-advice ()
  "Advise every command in `tabspaces-ext-project-directory-commands'."
  (dolist (cmd tabspaces-ext-project-directory-commands)
    (advice-add cmd :around #'tabspaces-ext--with-project-directory)))

(defun tabspaces-ext--remove-project-directory-advice ()
  "Remove project-directory advice from all commands that carry it.
Iterates the configured list; a stale list still cannot leave advice on a
command that was removed from it, but toggling the mode is the supported
way to pick up list changes."
  (dolist (cmd tabspaces-ext-project-directory-commands)
    (advice-remove cmd #'tabspaces-ext--with-project-directory)))

;;; Buffer cleanup on tab close

(defun tabspaces-ext--kill-buffers-before-close (tab)
  "Kill buffers unique to a tab when closing it.
This is a hook function for `tab-bar-tab-prevent-close-functions'."
  (let ((name (cdr (assq 'name tab))))
    (when (and name
               (stringp name)
               (not (string= name "Default"))
               (not (get-buffer "*tabspaces--placeholder*")))
      (let ((tab-index (tabspaces-ext--find-tab-index name)))
        (when tab-index
          ;; `tabspaces--buffer-list' takes a 0-based tab index (same as
          ;; `tab-bar--tab-index-by-name'), so pass TAB-INDEX/IDX directly.
          (let* ((tabs (funcall tab-bar-tabs-function))
                 (buffers (tabspaces--buffer-list nil tab-index))
                 (protected (make-hash-table :test 'eq)))
            ;; A buffer shared with any other tab must survive this close.
            (cl-loop for idx from 0 below (length tabs)
                     unless (= idx tab-index)
                     do (dolist (buf (tabspaces--buffer-list nil idx))
                          (puthash buf t protected)))
            (dolist (buf buffers)
              (when (buffer-live-p buf)
                (let ((buf-name (buffer-name buf)))
                  (unless (or (tabspaces-ext--is-system-buffer-p buf-name)
                              (gethash buf protected))
                    (kill-buffer buf)))))))))))

(defun tabspaces-ext--tab-close-handler (tab arg)
  "Handler for tab-bar-tab-prevent-close-functions.
Kills buffers before closing TAB. ARG is ignored."
  (tabspaces-ext--kill-buffers-before-close tab)
  nil) ; Return nil to allow tab to close

;;; Generic integration loader

(defconst tabspaces-ext--integration-alist
  '((magit . ((package . magit)
              (module . tabspaces-ext-magit)
              (variable . tabspaces-ext-magit)))
    (treemacs . ((package . treemacs)
                 (module . tabspaces-ext-treemacs)
                 (variable . tabspaces-ext-treemacs)))
    (popterm . ((package . popterm)
                (module . tabspaces-ext-popterm)
                (variable . tabspaces-ext-popterm))))
  "Alist of available integrations and their configuration.
Each entry is (NAME . PLIST) where PLIST contains:
  :package - the package to load after
  :module - the module to require
  :variable - the customization variable")

(defvar tabspaces-ext--integrations-loaded nil
  "List of integrations that have been loaded.")

(defun tabspaces-ext--load-integration (name config)
  "Load integration NAME with CONFIG.
Calls the integration's register-buffer-kind function if it exists,
then sets up full integration after the package loads."
  (let* ((package (alist-get 'package config))
         (module (alist-get 'module config))
         (register-fn (intern (format "%s-register-buffer-kind" module)))
         (setup-fn (intern (format "%s-setup" module)))
         (teardown-fn (intern (format "%s-teardown" module))))

    ;; Require the module early to make register-buffer-kind available
    ;; This is needed for session restoration support
    (require module nil t)

    ;; Call register-buffer-kind function if it exists (for session restoration)
    (when (fboundp register-fn)
      (funcall register-fn))

    ;; Set up full integration when package loads
    (eval `(with-eval-after-load ',package
             (when (fboundp ',setup-fn)
               (,setup-fn))))

    ;; Track that we've loaded this integration
    (push (cons name teardown-fn) tabspaces-ext--integrations-loaded)))

(defun tabspaces-ext--load-integrations ()
  "Load all enabled integrations based on customization variables."
  (dolist (entry tabspaces-ext--integration-alist)
    (let* ((name (car entry))
           (config (cdr entry))
           (var (alist-get 'variable config)))
      ;; Check if this integration is enabled and not already loaded
      (when (and (symbol-value var)
                 (not (assq name tabspaces-ext--integrations-loaded)))
        (tabspaces-ext--load-integration name config)))))

(defun tabspaces-ext--unload-integrations ()
  "Unload all loaded integrations."
  (dolist (entry tabspaces-ext--integrations-loaded)
    (let ((teardown-fn (cdr entry)))
      (when (fboundp teardown-fn)
        (funcall teardown-fn))))
  (setq tabspaces-ext--integrations-loaded nil))

;;; Session cleanup

(defun tabspaces-ext-cleanup-sessions ()
  "Delete stale per-project session files.
Scans the session store directory for session files and deletes those
whose project directory no longer exists, or whose session data contains
stale tab names (e.g. project@branch for a non-git directory)."
  (interactive)
  (let ((store (and (boundp 'tabspaces-session-project-session-store)
                    tabspaces-session-project-session-store)))
    (unless (and (stringp store) (file-directory-p store))
      (user-error "No session store directory configured"))
    (let ((files (directory-files store t "\\`\\..*-tabspaces-session\\.el\\'"))
          (removed 0)
          (kept 0))
      (dolist (file files)
        (if (tabspaces-ext--session-file-stale-p file)
            (progn
              (delete-file file)
              (cl-incf removed))
          (cl-incf kept)))
      (message "Session cleanup: removed %d stale file(s), kept %d" removed kept))))

(defun tabspaces-ext--session-file-stale-p (file)
  "Return non-nil if session FILE is stale and should be removed.
A session file is stale if its project root directory no longer exists,
or if the session tab name contains @ but the project root has no .git."
  (let ((data (tabspaces-ext--session-file-data file)))
    (or (null data)
        (not (file-directory-p (car data)))
        (and (string-match-p "@" (cdr data))
             (not (file-exists-p
                   (expand-file-name ".git" (expand-file-name (car data)))))))))

(defun tabspaces-ext--session-file-data (file)
  "Extract project root and tab name from a tabspaces session FILE.
Returns (ROOT . TAB-NAME) or nil on failure."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let (map session-list)
          (while (not (eobp))
            (let ((form (condition-case nil
                            (read (current-buffer))
                          (end-of-file nil))))
              (when form
                (pcase form
                  (`(setq tabspaces-project-tab-map (quote ,val))
                   (setq map val))
                  (`(setq tabspaces--session-list (quote ,val))
                   (setq session-list val))))))
          (when session-list
            (let* ((tab-name (cadr (car session-list)))
                   (root (or (car (rassoc tab-name map))
                             (caar map))))
              (when root
                (cons root tab-name))))))
    (error nil)))

;;; Minor mode

;;;###autoload
(define-minor-mode tabspaces-ext-mode
  "Toggle enhanced tabspaces integrations.
When enabled, provides:
- Automatic buffer cleanup when closing tabs
- Optional integrations (magit, treemacs, popterm) based on customization"
  :global t
  :group 'tabspaces-ext
  (if tabspaces-ext-mode
      (progn
        ;; Core functionality
        (add-hook 'tab-bar-tab-prevent-close-functions #'tabspaces-ext--tab-close-handler)
        ;; Keep a project tab's saved session free of buffers that leaked in
        ;; from other workspaces.
        (advice-add 'tabspaces--store-buffers :filter-return
                    #'tabspaces-ext--filter-foreign-buffers)
        ;; Pin direction-sensitive commands to the tab's project root.
        (tabspaces-ext--install-project-directory-advice)
        ;; Load enabled integrations
        (tabspaces-ext--load-integrations))
    ;; Disable
    (remove-hook 'tab-bar-tab-prevent-close-functions #'tabspaces-ext--tab-close-handler)
    (advice-remove 'tabspaces--store-buffers #'tabspaces-ext--filter-foreign-buffers)
    (tabspaces-ext--remove-project-directory-advice)
    (tabspaces-ext--unload-integrations)))

(provide 'tabspaces-ext)

;;; tabspaces-ext.el ends here
