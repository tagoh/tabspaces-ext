;;; window-state-plus.el --- Enhanced window state save/restore for side windows -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira TAGOH

;; Author: Akira TAGOH <akira@tagoh.org>
;; URL: https://github.com/tagoh/tabspaces-ext
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: frames, convenience

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

;; Enhanced window state save/restore functions that properly handle side
;; windows (popterm, treemacs, etc.).
;;
;; Problem:
;;
;; The built-in `window-state-get' and `window-state-put' functions can
;; cause layout corruption when dealing with side windows, particularly
;; during session save/restore operations. Side windows created by packages
;; like popterm and treemacs may:
;;
;; - Disappear during restoration
;; - Appear in wrong positions
;; - Have incorrect sizes
;; - Lose their side window properties
;;
;; This corruption happens because the built-in functions don't properly
;; preserve side window parameters (window-side, window-slot, etc.) and
;; don't handle the interaction between normal and side windows correctly.
;;
;; Solution:
;;
;; This package provides enhanced functions that:
;;
;; - Properly save side window configurations (buffer, side, slot, size)
;; - Delete existing side windows before restoring main layout
;; - Correctly restore side windows with proper parameters
;; - Preserve window dedication and size constraints
;;
;; The functions work as drop-in replacements for `window-state-get' and
;; `window-state-put', or can be used via advice wrappers for session
;; save functions.
;;
;; Usage:
;;
;; As drop-in replacements:
;;
;;   (require 'window-state-plus)
;;
;;   ;; Get state
;;   (let ((state (window-state-plus-get (frame-root-window) t)))
;;     ;; ... do something ...
;;     ;; Restore state
;;     (window-state-plus-put state (frame-root-window)))
;;
;; As advice for session save functions:
;;
;;   (advice-add 'tabspaces-save-session :around
;;               #'window-state-plus-advice-save-session)
;;
;; This is the recommended approach for tabspaces-ext, as it automatically
;; wraps the save operation to preserve layout during the save process.
;;
;; Customization:
;;
;; - `window-state-plus-preserve-side-windows' - enable/disable side window
;;   handling (default: t)
;;
;; - `window-state-plus-side-window-buffers' - list of buffer name prefixes
;;   that indicate side windows (default: "*Treemacs-" "*popterm-")
;;
;; Independence:
;;
;; This package can be used independently of tabspaces-ext for any session
;; management or window layout save/restore needs involving side windows.
;;
;; Acknowledgments:
;;   This package was created with the assistance of Claude (Anthropic).

;;; Code:

(require 'cl-lib)
(require 'window)

;;; Customization

(defgroup window-state-plus nil
  "Enhanced window state save/restore for side windows."
  :group 'windows
  :group 'convenience)

(defcustom window-state-plus-preserve-side-windows t
  "When non-nil, preserve side window configurations during save/restore.
This prevents layout corruption with packages like popterm and treemacs."
  :type 'boolean
  :group 'window-state-plus)

(defcustom window-state-plus-side-window-buffers
  '("*Treemacs-" "*popterm-")
  "List of buffer name prefixes that indicate side windows.
Buffers matching these prefixes will be handled specially during
window state save/restore operations."
  :type '(repeat string)
  :group 'window-state-plus)

;;; Helper functions

(defun window-state-plus--is-side-window-buffer-p (buffer)
  "Return t if BUFFER is a side window buffer."
  (when buffer
    (let ((buf-name (if (bufferp buffer)
                        (buffer-name buffer)
                      buffer)))
      (cl-some (lambda (prefix)
                 (string-prefix-p prefix buf-name))
               window-state-plus-side-window-buffers))))

(defun window-state-plus--get-side-window-state ()
  "Get state of all current side windows.
Returns a list of (buffer window-parameters) for each side window."
  (let ((side-windows '()))
    (walk-windows
     (lambda (win)
       (when (window-parameter win 'window-side)
         (let ((buffer (window-buffer win)))
           (when (window-state-plus--is-side-window-buffer-p buffer)
             (push (list :buffer buffer
                        :side (window-parameter win 'window-side)
                        :slot (window-parameter win 'window-slot)
                        :width (window-width win)
                        :height (window-height win)
                        :dedicated (window-dedicated-p win))
                   side-windows)))))
     ;; Current frame only, to stay symmetric with `--delete-side-windows'
     ;; and `window-state-plus-put', which operate on the selected frame.
     nil nil)
    (nreverse side-windows)))

(defun window-state-plus--delete-side-windows ()
  "Delete the current frame's side windows managed by window-state-plus."
  (dolist (win (window-list))
    (when (and (window-parameter win 'window-side)
               (window-state-plus--is-side-window-buffer-p (window-buffer win)))
      (ignore-errors (delete-window win)))))

(defun window-state-plus--restore-side-window (state)
  "Restore a single side window from STATE."
  (let ((buffer (plist-get state :buffer))
        (side (plist-get state :side))
        (slot (plist-get state :slot))
        (width (plist-get state :width))
        (height (plist-get state :height)))
    (when (buffer-live-p buffer)
      (let ((win (display-buffer-in-side-window
                  buffer
                  `((side . ,side)
                    (slot . ,slot)
                    (window-width . ,width)
                    (window-height . ,height)
                    (preserve-size . (t . t))))))
        (when win
          (set-window-dedicated-p win t)
          win)))))

;;; Public API

(defconst window-state-plus--wrapper-tag 'window-state-plus--v1
  "Head symbol marking a state wrapped by `window-state-plus-get'.
Distinguishes a (TAG :main STATE :side-windows LIST) wrapper from a raw
`window-state-get' value, so `window-state-plus-put' can accept both
without misparsing the non-plist window-state structure.")

;;;###autoload
(defun window-state-plus-get (&optional window writable)
  "Get window state of WINDOW, properly handling side windows.
This is an enhanced version of `window-state-get' that correctly
handles side windows like popterm and treemacs.

WINDOW defaults to the selected window.
WRITABLE is passed to the underlying `window-state-get'."
  (let* ((window (or window (selected-window)))
         (side-window-states (when window-state-plus-preserve-side-windows
                               (window-state-plus--get-side-window-state)))
         (main-state (window-state-get window writable)))
    ;; A `window-state-get' value is not a plist (with WRITABLE its car is
    ;; an alist of size constraints), so side window info cannot be stored
    ;; inside it via `plist-put' -- that signals `wrong-type-argument
    ;; plistp' on Emacs 28+.  Wrap both pieces in a tagged container.
    (if side-window-states
        (list window-state-plus--wrapper-tag
              :main main-state
              :side-windows side-window-states)
      main-state)))

;;;###autoload
(defun window-state-plus-put (state &optional window ignore)
  "Restore window STATE into WINDOW, properly handling side windows.
This is an enhanced version of `window-state-put' that correctly
restores side windows like popterm and treemacs.

WINDOW defaults to the selected window.
IGNORE is passed to the underlying `window-state-put'."
  (let* ((window (or window (selected-window)))
         (wrapped (and (consp state)
                       (eq (car state) window-state-plus--wrapper-tag)))
         (main-state (if wrapped (plist-get (cdr state) :main) state))
         (side-window-states (and wrapped (plist-get (cdr state) :side-windows))))

    ;; Delete existing side windows first to avoid conflicts
    (when window-state-plus-preserve-side-windows
      (window-state-plus--delete-side-windows))

    ;; Restore main window state
    (ignore-errors
      (window-state-put main-state window ignore))

    ;; Restore side windows
    (when (and window-state-plus-preserve-side-windows side-window-states)
      (dolist (side-state side-window-states)
        (ignore-errors
          (window-state-plus--restore-side-window side-state))))))

;;;###autoload
(defun window-state-plus-wrap-save-function (save-fn &rest args)
  "Wrap SAVE-FN to preserve window layout during save operation.
This is useful for session save functions that may corrupt the layout
during the save process.

Example:
  (advice-add 'tabspaces-save-session :around
              #'window-state-plus-wrap-save-function)"
  (let ((saved-state (window-state-plus-get (frame-root-window) t)))
    (prog1 (apply save-fn args)
      (ignore-errors
        (window-state-plus-put saved-state (frame-root-window))))))

;;;###autoload
(defun window-state-plus-advice-save-session (orig-fun &rest args)
  "Advice function to preserve window layout during session save.
Add this as :around advice to session save functions.

Example:
  (advice-add 'tabspaces-save-session :around
              #'window-state-plus-advice-save-session)"
  (let ((saved-state (window-state-plus-get (frame-root-window) t)))
    (prog1 (apply orig-fun args)
      (ignore-errors
        (window-state-plus-put saved-state (frame-root-window))))))

(provide 'window-state-plus)

;;; window-state-plus.el ends here
