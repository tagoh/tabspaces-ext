;;; tabspaces-ext-popterm.el --- Popterm integration for tabspaces-ext -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira TAGOH

;; Author: Akira TAGOH <akira@tagoh.org>
;; URL: https://github.com/tagoh/tabspaces-ext
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (popterm "0.1") (tabspaces "1.0") (tabspaces-ext "1.0"))
;; Keywords: convenience, terminals

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

;; Popterm integration module for tabspaces-ext.
;;
;; This module implements the tabspaces-ext integration contract:
;;
;; - `tabspaces-ext-popterm-register-buffer-kind' (autoloaded)
;;   Registers popterm buffer kind for session restoration.
;;   Called early before popterm loads. Uses `featurep' checks
;;   to avoid requiring popterm at registration time.
;;
;; - `tabspaces-ext-popterm-setup' (autoloaded)
;;   Called after popterm loads. Sets up advice and hooks for
;;   per-tab terminal isolation. Safe to require popterm features here.
;;
;; - `tabspaces-ext-popterm-teardown'
;;   Removes all hooks and advice installed by setup.
;;
;; Features:
;; - Separate terminal instances per tab
;; - Tab-specific buffer naming (*popterm-backend[project@branch]*)
;; - Automatic window sync on tab switch
;; - Session restoration support
;; - Layout fixes for side windows
;; - Per-tab toggle command (`tabspaces-ext-popterm-toggle')
;;
;; Integration Details:
;;
;; This module advises popterm's buffer naming and buffer listing
;; functions to create isolated terminal instances per tab. Each
;; tab gets its own popterm buffer named with the tab name.
;;
;; The module also handles window state synchronization to ensure
;; popterm windows are correctly associated with their tabs after
;; tab switches or session restoration.
;;
;; Session restoration registers popterm buffer kinds early and
;; restores them with the correct directory and backend settings.
;;
;; Usage:
;;
;; Bind the per-tab toggle command to a key:
;;
;;   (with-eval-after-load 'popterm
;;     (global-set-key [f9] #'tabspaces-ext-popterm-toggle))
;;
;; This toggles the popterm terminal for the current tab, automatically
;; using the tab's project directory.
;;
;; Acknowledgments:
;;   This module was created with the assistance of Claude (Anthropic).

;;; Code:

(require 'tabspaces)
(require 'tabspaces-ext)

;; Don't require popterm here - it will be loaded via with-eval-after-load

;;; Session restoration

(defconst tabspaces-ext-popterm--backend-tags
  '((vterm . "vterm") (ghostel . "ghostel") (eat . "eat")
    (shell . "shell") (eshell . "eshell"))
  "Mapping of popterm backends to their tag strings.")

;;;###autoload
(defun tabspaces-ext-popterm-register-buffer-kind ()
  "Register popterm buffer kind for session restoration.
This function is called early during tabspaces-ext initialization,
before popterm is loaded, to support session restoration."
  (tabspaces-register-buffer-kind
   'popterm
   (lambda (b)
     (when (and (boundp 'popterm-mode)
                (buffer-local-value 'popterm-mode b))
       (list :kind 'popterm
             :backend (when (boundp 'popterm-backend)
                        (buffer-local-value 'popterm-backend b))
             :dir (buffer-local-value 'default-directory b))))
   (lambda (rec)
     (when (and (featurep 'popterm)
                (plist-get rec :dir)
                (file-directory-p (plist-get rec :dir)))
       (let* ((dir (plist-get rec :dir))
              (tab-name (tabspaces-ext--get-current-tab-name))
              (backend (or (plist-get rec :backend) 'ghostel))
              (buf-name (format "*popterm-%s[%s]*"
                              (or (cdr (assq backend tabspaces-ext-popterm--backend-tags)) "ghostel")
                              tab-name)))
         (or (tabspaces-reuse-existing-buffer buf-name)
             (let ((default-directory dir))
               (ignore-errors
                 (when (fboundp 'popterm--get-or-create)
                   (popterm--get-or-create tab-name backend)
                   (when-let* ((buf (get-buffer buf-name)))
                     (with-current-buffer buf
                       (setq default-directory dir)
                       (when-let* ((proc (get-buffer-process buf)))
                         (process-send-string proc (format "cd %s\n" (shell-quote-argument dir)))))
                     buf))))))))))

;;; Helper functions

(defun tabspaces-ext-popterm--restoring-session-p ()
  "Check if tabspaces is currently restoring a session."
  (get-buffer "*tabspaces--placeholder*"))

(defun tabspaces-ext-popterm--buffer-name (backend tab-name)
  "Generate popterm buffer name for BACKEND and TAB-NAME."
  (format "*popterm-%s[%s]*"
          (or (cdr (assq backend tabspaces-ext-popterm--backend-tags)) "ghostel")
          tab-name))

(defun tabspaces-ext-popterm--get-project-dir (tab-name)
  "Find project directory for TAB-NAME from multiple sources."
  (or (car (rassoc tab-name tabspaces-project-tab-map))
      (cl-loop for buf in (mapcar #'window-buffer (window-list))
               for proj = (with-current-buffer buf (project-current nil))
               when proj return (project-root proj))
      (when-let* ((project (project-current))) (project-root project))))

(defun tabspaces-ext-popterm--set-directory (buf dir)
  "Set BUF's directory to DIR and send cd command to terminal."
  (with-current-buffer buf
    (setq default-directory dir)
    (when-let* ((proc (get-buffer-process buf)))
      (process-send-string proc (format "cd %s\n" (shell-quote-argument dir))))))

;;; Buffer isolation

(defun tabspaces-ext-popterm--buffer-name-with-tab (orig-fun &optional name backend)
  "Inject current tab name into popterm buffer name for isolation per tab."
  (funcall orig-fun
           (if (and (not name)
                    (not (tabspaces-ext-popterm--restoring-session-p))
                    (tabspaces--current-tab-name))
               (tabspaces--current-tab-name)
             name)
           backend))

(defun tabspaces-ext-popterm--filter-buffers-by-tab (orig-fun &optional backend)
  "Filter popterm buffer list to only current tab's buffers."
  (let ((all-bufs (funcall orig-fun backend)))
    (if (tabspaces-ext-popterm--restoring-session-p)
        all-bufs
      (let ((tab-name (tabspaces--current-tab-name)))
        (if (not tab-name)
            all-bufs
          (cl-remove-if-not
           (lambda (buf)
             (let ((inst (buffer-local-value 'popterm--buffer-instance-name buf)))
               (and inst (or (string= inst tab-name)
                             (string-prefix-p (concat tab-name ":") inst)))))
           all-bufs))))))

;;; Window sync

(defun tabspaces-ext-popterm--sync-window-for-tab ()
  "Sync popterm--window to match the current tab's popterm window."
  (unless (tabspaces-ext-popterm--restoring-session-p)
    (when-let* ((tab-name (tabspaces--current-tab-name))
                (backend (or popterm-backend 'ghostel))
                (tag (pcase backend
                       ('vterm "vterm") ('ghostel "ghostel") ('eat "eat")
                       ('shell "shell") ('eshell "eshell")))
                (buf-name (format "*popterm-%s[%s]*" tag tab-name)))
      (let ((found-window
             (cl-find-if (lambda (w) (string= (buffer-name (window-buffer w)) buf-name))
                         (window-list))))
        ;; Update if we found current tab's window, or if current popterm--window
        ;; is invalid or belongs to a different tab
        (when (or found-window
                  (not (and (boundp 'popterm--window)
                            popterm--window
                            (window-live-p popterm--window)
                            (string= (buffer-name (window-buffer popterm--window)) buf-name))))
          (setq popterm--window found-window))))))

(defun tabspaces-ext-popterm--sync-window-state (orig-fun &rest args)
  "Sync popterm--window to actual visible window before toggle."
  (tabspaces-ext-popterm--sync-window-for-tab)
  (apply orig-fun args))

(defun tabspaces-ext-popterm--handle-tab-switch (&rest _)
  "Sync popterm window state when switching tabs."
  (tabspaces-ext-popterm--sync-window-for-tab))

;;; Window display

(defun tabspaces-ext-popterm--window-show-at-bottom (orig-fun buffer)
  "Show popterm BUFFER in a full-width bottom side window.
popterm's own `popterm--window-show' splits `frame-root-window', which
only covers the main window area when a left/right side window (e.g. the
Treemacs sidebar) is present -- leaving popterm in the bottom of the main
area (\"right-bottom\") rather than spanning the whole frame.  A bottom
side window spans the full width and pushes the vertical side windows up.

Falls back to ORIG-FUN when the side window cannot be created."
  (let* ((ratio (if (boundp 'popterm-window-height-ratio)
                    popterm-window-height-ratio
                  0.3))
         (win (display-buffer-in-side-window
               buffer `((side . bottom)
                        (slot . 0)
                        (window-height . ,ratio)
                        (preserve-size . (nil . t))))))
    (if (not (window-live-p win))
        (funcall orig-fun buffer)
      (setq popterm--active-display-method 'window)
      (setq popterm--window win)
      (select-window win)
      (popterm--reset-cursor-point buffer)
      win)))

;;; Layout fixes

(defun tabspaces-ext-popterm--fix-layout (&rest _)
  "Reposition an already-visible popterm after restoration/tab switching.
Does nothing when popterm is not currently displayed: the buffer often
outlives its window, so keying off buffer existence alone would re-open a
popterm the user has deliberately closed on every tab switch (including
the tab cycling done by session auto-save).

Session restore needs no help here: popterm is a bottom side window (see
`tabspaces-ext-popterm--window-show-at-bottom'), so tabspaces restores it
via `window-state-put' -- but only when it was actually displayed at save
time, which is exactly the desired behaviour."
  (when-let* ((tab-name (tabspaces-ext--get-current-tab-name))
              (project-dir (tabspaces-ext-popterm--get-project-dir tab-name))
              (popterm-buf (get-buffer (tabspaces-ext-popterm--buffer-name popterm-backend tab-name)))
              (windows (get-buffer-window-list popterm-buf nil t)))
    ;; Delete all popterm windows
    (dolist (win windows)
      (unless (eq win (frame-root-window))
        (ignore-errors (delete-window win))))
    ;; Recreate at bottom with correct directory
    (let ((default-directory project-dir)
          (popterm-display-method 'window))
      (ignore-errors (popterm-toggle tab-name popterm-backend)))
    (tabspaces-ext-popterm--set-directory popterm-buf project-dir)))

;;; Per-tab toggle command

;;;###autoload
(defun tabspaces-ext-popterm-toggle ()
  "Toggle popterm in window with current tab name as instance."
  (interactive)
  (when-let* ((tab-name (tabspaces-ext--get-current-tab-name)))
    (let* ((project-dir (tabspaces-ext-popterm--get-project-dir tab-name))
           (default-directory (or project-dir default-directory))
           (popterm-display-method 'window))
      (popterm-toggle tab-name popterm-backend)
      (when project-dir
        (when-let* ((buf (get-buffer (tabspaces-ext-popterm--buffer-name popterm-backend tab-name))))
          (tabspaces-ext-popterm--set-directory buf project-dir))))))

;;; Setup/teardown functions

(defvar tabspaces-ext-popterm--active nil
  "Track whether popterm integration is active.")

;;;###autoload
(defun tabspaces-ext-popterm-setup ()
  "Set up Popterm integration for tabspaces."
  (unless tabspaces-ext-popterm--active
    ;; Buffer isolation
    (advice-add 'popterm--buffer-name :around #'tabspaces-ext-popterm--buffer-name-with-tab)
    (advice-add 'popterm--buffer-list :around #'tabspaces-ext-popterm--filter-buffers-by-tab)
    ;; Window display: full-width bottom side window instead of a main-area split
    (advice-add 'popterm--window-show :around #'tabspaces-ext-popterm--window-show-at-bottom)
    ;; Window sync
    (advice-add 'popterm-window-toggle :around #'tabspaces-ext-popterm--sync-window-state)
    (add-hook 'tab-bar-tab-post-select-functions #'tabspaces-ext-popterm--handle-tab-switch)
    ;; Layout fixes: only reposition an already-visible popterm, never reopen
    ;; a closed one.  Session restore is handled by tabspaces' own
    ;; `window-state-put' now that popterm is a bottom side window.
    (advice-add 'tabspaces-restore-session :after #'tabspaces-ext-popterm--fix-layout)
    (advice-add 'tab-bar-select-tab :after #'tabspaces-ext-popterm--fix-layout)
    (advice-add 'tab-bar-select-tab-by-name :after #'tabspaces-ext-popterm--fix-layout)
    ;; Note: buffer kind registration is done early by register-buffer-kind function
    (setq tabspaces-ext-popterm--active t)))

;;;###autoload
(defun tabspaces-ext-popterm-teardown ()
  "Tear down Popterm integration for tabspaces."
  (when tabspaces-ext-popterm--active
    ;; Buffer isolation
    (advice-remove 'popterm--buffer-name #'tabspaces-ext-popterm--buffer-name-with-tab)
    (advice-remove 'popterm--buffer-list #'tabspaces-ext-popterm--filter-buffers-by-tab)
    ;; Window display
    (advice-remove 'popterm--window-show #'tabspaces-ext-popterm--window-show-at-bottom)
    ;; Window sync
    (advice-remove 'popterm-window-toggle #'tabspaces-ext-popterm--sync-window-state)
    (remove-hook 'tab-bar-tab-post-select-functions #'tabspaces-ext-popterm--handle-tab-switch)
    ;; Layout fixes
    (advice-remove 'tabspaces-restore-session #'tabspaces-ext-popterm--fix-layout)
    (advice-remove 'tab-bar-select-tab #'tabspaces-ext-popterm--fix-layout)
    (advice-remove 'tab-bar-select-tab-by-name #'tabspaces-ext-popterm--fix-layout)
    (setq tabspaces-ext-popterm--active nil)))

(provide 'tabspaces-ext-popterm)

;;; tabspaces-ext-popterm.el ends here
