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

;;; Helper functions

(defun tabspaces-ext--add-project-tab-mapping (project-root tab-name)
  "Add PROJECT-ROOT to TAB-NAME mapping without creating duplicates."
  (let ((existing (assoc project-root tabspaces-project-tab-map)))
    (if existing
        ;; Update existing mapping if different
        (unless (string= (cdr existing) tab-name)
          (message "Updating tab mapping: %s -> %s (was %s)" project-root tab-name (cdr existing))
          (setf (cdr existing) tab-name))
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
          (let* ((tabs (funcall tab-bar-tabs-function))
                 (buffers (tabspaces--buffer-list nil (1+ tab-index)))
                 (other-tabs-buffers
                  (cl-loop for idx from 0 below (length tabs)
                           unless (= idx tab-index)
                           append (tabspaces--buffer-list nil (1+ idx)))))
            (dolist (buf buffers)
              (when (buffer-live-p buf)
                (let ((buf-name (buffer-name buf)))
                  (unless (or (tabspaces-ext--is-system-buffer-p buf-name)
                              (member buf other-tabs-buffers))
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
        ;; Load enabled integrations
        (tabspaces-ext--load-integrations))
    ;; Disable
    (remove-hook 'tab-bar-tab-prevent-close-functions #'tabspaces-ext--tab-close-handler)
    (tabspaces-ext--unload-integrations)))

(provide 'tabspaces-ext)

;;; tabspaces-ext.el ends here
