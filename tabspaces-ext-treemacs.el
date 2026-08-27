;;; tabspaces-ext-treemacs.el --- Treemacs integration for tabspaces-ext -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira TAGOH

;; Author: Akira TAGOH <akira@tagoh.org>
;; URL: https://github.com/tagoh/tabspaces-ext
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (treemacs "3.0") (tabspaces "1.0") (tabspaces-ext "1.0"))
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

;; Treemacs integration module for tabspaces-ext.
;;
;; This module implements the tabspaces-ext integration contract:
;;
;; - `tabspaces-ext-treemacs-register-buffer-kind' (autoloaded)
;;   Registers treemacs buffer kind for session restoration.
;;   Called early before treemacs loads. Uses `featurep' checks
;;   to avoid requiring treemacs at registration time.
;;
;; - `tabspaces-ext-treemacs-setup' (autoloaded)
;;   Called after treemacs loads. Sets up hooks for automatic
;;   workspace sync. Safe to require treemacs features here.
;;
;; - `tabspaces-ext-treemacs-teardown'
;;   Removes all hooks and advice installed by setup.
;;
;; Features:
;; - Per-tab workspace sync - shows only current tab's project
;; - Automatic sync on tab switch
;; - Session restoration - restores treemacs state with sessions
;; - Debug command - `tabspaces-ext-treemacs-sync-debug' for troubleshooting
;;
;; Integration Details:
;;
;; This module hooks into `tab-bar-tab-post-select-functions' to sync
;; treemacs workspace when switching tabs. The sync ensures treemacs
;; shows exactly one project: the current tab's project root.
;;
;; Session restoration works by registering treemacs buffers early,
;; before treemacs is loaded. The buffer kind registration uses
;; `featurep' checks to restore treemacs buffers only when treemacs
;; is available.
;;
;; Troubleshooting:
;;
;; If treemacs isn't syncing correctly, run:
;;   M-x tabspaces-ext-treemacs-sync-debug
;;
;; This shows diagnostic information about tab, project mappings,
;; and treemacs workspace state.
;;
;; Acknowledgments:
;;   This module was created with the assistance of Claude (Anthropic).

;;; Code:

(require 'tabspaces)
(require 'tabspaces-ext)

;; Don't require treemacs here - it will be loaded via with-eval-after-load

;;; Session restoration

;;;###autoload
(defun tabspaces-ext-treemacs-register-buffer-kind ()
  "Register treemacs buffer kind for session restoration.
This function is called early during tabspaces-ext initialization,
before treemacs is loaded, to support session restoration."
  (tabspaces-register-buffer-kind
   'treemacs
   (lambda (b)
     (when (eq (buffer-local-value 'major-mode b) 'treemacs-mode)
       (list :kind 'treemacs :dir (buffer-local-value 'default-directory b))))
   (lambda (rec)
     (when-let* ((dir (plist-get rec :dir)))
       (or (tabspaces-reuse-existing-buffer " *Treemacs-Buffer-No Tab")
           (when (featurep 'treemacs)
             (save-window-excursion
               (let ((default-directory dir))
                 (ignore-errors (treemacs) (current-buffer))))))))))

;;; Treemacs sync functions

(defun tabspaces-ext-treemacs--get-project-root-for-tab ()
  "Get project root for current tabspaces tab.
Falls back to `project-current' when the tab map has no entry."
  (let ((tab-name (tabspaces-ext--get-current-tab-name)))
    (when tab-name
      (let ((from-map (and (boundp 'tabspaces-project-tab-map)
                           (car (rassoc tab-name tabspaces-project-tab-map)))))
        (if (and from-map (file-directory-p from-map))
            (expand-file-name from-map)
          (when-let* ((proj (project-current)))
            (expand-file-name (project-root proj))))))))

(defun tabspaces-ext-treemacs--sync-with-tabspaces ()
  "Sync treemacs to show the current project for the active tabspaces tab."
  (condition-case err
      (when-let* ((root0 (tabspaces-ext-treemacs--get-project-root-for-tab))
                  ;; Match treemacs' own path canonicalization (truename +
                  ;; trailing slash stripped); otherwise `root' never compares
                  ;; equal to a stored project path and every sync needlessly
                  ;; removes and re-adds the project (visible flicker).
                  (root (if (fboundp 'treemacs-canonical-path)
                            (treemacs-canonical-path (file-truename root0))
                          root0))
                  (workspace (treemacs-current-workspace)))
        (let* ((projects (treemacs-workspace->projects workspace))
               (paths (mapcar #'treemacs-project->path projects)))
          ;; Only sync if not already showing the correct single project
          (unless (and (= 1 (length projects)) (string= root (car paths)))
            ;; Remove mismatched projects
            (dolist (project projects)
              (unless (string= root (treemacs-project->path project))
                (treemacs-do-remove-project-from-workspace project t)))
            ;; Add current project if missing
            (unless (member root paths)
              (treemacs-do-add-project-to-workspace
               root (file-name-nondirectory (directory-file-name root))))
            ;; Pulse notification if treemacs window is visible
            (when-let* ((window (treemacs-get-local-window))
                        (buffer (window-buffer window)))
              (run-with-timer 0.1 nil
                              (lambda (w b p)
                                (when (and (window-live-p w) (buffer-live-p b))
                                  (with-selected-window w
                                    (goto-char (point-min))
                                    (treemacs-pulse-on-success "Synced to %s" p))))
                              window buffer (file-name-nondirectory (directory-file-name root)))))))
    (error (message "Treemacs sync error: %S" err))))

(let ((last-synced-tab nil)
      (pending-timer nil))
  (defun tabspaces-ext-treemacs--handle-tab-switch (&rest _)
    "Handle treemacs updates when switching tabspaces tabs."
    (let ((current-tab (tabspaces-ext--get-current-tab-name)))
      (unless (equal current-tab last-synced-tab)
        (setq last-synced-tab current-tab)
        (when (timerp pending-timer)
          (cancel-timer pending-timer))
        (setq pending-timer
              (run-with-idle-timer 0.3 nil #'tabspaces-ext-treemacs--sync-with-tabspaces)))))

  (defun tabspaces-ext-treemacs--treemacs-opened (&rest _)
    "Sync treemacs when it's opened."
    (when (timerp pending-timer)
      (cancel-timer pending-timer))
    (setq pending-timer
          (run-with-idle-timer 0.3 nil #'tabspaces-ext-treemacs--sync-with-tabspaces)))

  (defun tabspaces-ext-treemacs--handle-magit-buffer (&rest _)
    "Sync treemacs when a magit status buffer is displayed.
This handles worktree switches that may not trigger tab-bar hooks."
    (when (derived-mode-p 'magit-status-mode)
      (when (timerp pending-timer)
        (cancel-timer pending-timer))
      (setq pending-timer
            (run-with-idle-timer 0.5 nil #'tabspaces-ext-treemacs--sync-with-tabspaces))))

  (defun tabspaces-ext-treemacs--reset-state ()
    "Reset internal state for clean teardown/re-enable."
    (when (timerp pending-timer)
      (cancel-timer pending-timer))
    (setq pending-timer nil
          last-synced-tab nil)))

;;; Debug function

;;;###autoload
(defun tabspaces-ext-treemacs-sync-debug ()
  "Manually sync treemacs and show diagnostic info."
  (interactive)
  (let* ((tab (tabspaces-ext--get-current-tab-name))
         (root-map (tabspaces-ext-treemacs--get-project-root-for-tab))
         (root-api (when-let* ((p (project-current))) (expand-file-name (project-root p))))
         (workspace (treemacs-current-workspace))
         (projects (when workspace
                     (mapcar #'treemacs-project->path (treemacs-workspace->projects workspace)))))
    (message "Tab: %s | Map: %s | API: %s | Projects: %s"
             tab root-map root-api projects)
    (tabspaces-ext-treemacs--sync-with-tabspaces)))

;;; Setup/teardown functions

(defvar tabspaces-ext-treemacs--active nil
  "Track whether treemacs integration is active.")

;;;###autoload
(defun tabspaces-ext-treemacs-setup ()
  "Set up Treemacs integration for tabspaces."
  (unless tabspaces-ext-treemacs--active
    ;; Require treemacs features (safe now since this runs after treemacs loads)
    (require 'treemacs-scope)
    ;; Install hooks
    (add-hook 'tab-bar-tab-post-select-functions #'tabspaces-ext-treemacs--handle-tab-switch)
    (advice-add 'treemacs :after #'tabspaces-ext-treemacs--treemacs-opened)
    ;; Sync treemacs when magit displays a status buffer (worktree switches)
    (with-eval-after-load 'magit
      (add-hook 'magit-post-display-buffer-hook #'tabspaces-ext-treemacs--handle-magit-buffer))
    ;; Note: buffer kind registration is done early by register-buffer-kind function
    (setq tabspaces-ext-treemacs--active t)))

;;;###autoload
(defun tabspaces-ext-treemacs-teardown ()
  "Tear down Treemacs integration for tabspaces."
  (when tabspaces-ext-treemacs--active
    (remove-hook 'tab-bar-tab-post-select-functions #'tabspaces-ext-treemacs--handle-tab-switch)
    (advice-remove 'treemacs #'tabspaces-ext-treemacs--treemacs-opened)
    (remove-hook 'magit-post-display-buffer-hook #'tabspaces-ext-treemacs--handle-magit-buffer)
    (tabspaces-ext-treemacs--reset-state)
    (setq tabspaces-ext-treemacs--active nil)))

(provide 'tabspaces-ext-treemacs)

;;; tabspaces-ext-treemacs.el ends here
