;;; window-state-plus-test.el --- Tests for window-state-plus -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for window-state-plus: the tagged-wrapper state handling
;; (regression for the `plist-put'/`plist-get' misuse that signalled
;; `wrong-type-argument plistp' on Emacs 28+ whenever a side window was
;; present), the side-window round-trip, and the save-session advice.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'window-state-plus)

;;; Helpers

(defun window-state-plus-test--side-windows ()
  "Return the live side windows in the selected frame."
  (cl-remove-if-not (lambda (w) (window-parameter w 'window-side))
                    (window-list)))

(defun window-state-plus-test--clear-side-windows ()
  "Delete every side window in the selected frame."
  (dolist (w (window-list))
    (when (window-parameter w 'window-side)
      (ignore-errors (delete-window w)))))

(defmacro window-state-plus-test--with-layout (&rest body)
  "Build a main window plus a treemacs-style side window, run BODY, clean up."
  (declare (indent 0))
  `(let ((window-state-plus-side-window-buffers '("*Treemacs-test"))
         (main (get-buffer-create "wsp-main"))
         (side (get-buffer-create "*Treemacs-test*")))
     (unwind-protect
         (progn
           (switch-to-buffer main)
           (window-state-plus-test--clear-side-windows)
           (delete-other-windows)
           (set-window-dedicated-p
            (display-buffer-in-side-window side '((side . left) (slot . 0))) t)
           ,@body)
       (window-state-plus-test--clear-side-windows)
       (delete-other-windows)
       (when (buffer-live-p main) (kill-buffer main))
       (when (buffer-live-p side) (kill-buffer side)))))

;;; Tests for window-state-plus-get

(ert-deftest window-state-plus-test/get-does-not-signal-with-side-window ()
  "Regression: `plist-put' on a non-plist window-state signalled `plistp'.
`window-state-plus-get' must succeed when a side window is present."
  (window-state-plus-test--with-layout
    (should (window-state-plus-get (frame-root-window) t))))

(ert-deftest window-state-plus-test/get-wraps-when-side-window-present ()
  "With a side window, get returns a tagged wrapper carrying both pieces."
  (window-state-plus-test--with-layout
    (let ((st (window-state-plus-get (frame-root-window) t)))
      (should (eq (car st) window-state-plus--wrapper-tag))
      (should (plist-get (cdr st) :main))
      (should (plist-get (cdr st) :side-windows)))))

(ert-deftest window-state-plus-test/get-raw-without-side-window ()
  "Without a side window, get returns a raw window-state (drop-in)."
  (let ((main (get-buffer-create "wsp-main")))
    (unwind-protect
        (progn
          (switch-to-buffer main)
          (window-state-plus-test--clear-side-windows)
          (delete-other-windows)
          (let ((st (window-state-plus-get (frame-root-window) t)))
            (should-not (and (consp st)
                             (eq (car st) window-state-plus--wrapper-tag)))))
      (when (buffer-live-p main) (kill-buffer main)))))

;;; Tests for window-state-plus-put

(ert-deftest window-state-plus-test/put-restores-side-window ()
  "A get/scramble/put cycle restores the side window with its parameters."
  (window-state-plus-test--with-layout
    (let ((st (window-state-plus-get (frame-root-window) t)))
      (window-state-plus-test--clear-side-windows)
      (delete-other-windows)
      (should-not (window-state-plus-test--side-windows))
      (window-state-plus-put st (frame-root-window))
      (should (cl-some
               (lambda (w)
                 (and (eq (window-parameter w 'window-side) 'left)
                      (string= (buffer-name (window-buffer w)) "*Treemacs-test*")))
               (window-list))))))

(ert-deftest window-state-plus-test/put-accepts-raw-state ()
  "put must accept a raw `window-state-get' value without error."
  (let ((main (get-buffer-create "wsp-main")))
    (unwind-protect
        (progn
          (switch-to-buffer main)
          (window-state-plus-test--clear-side-windows)
          (delete-other-windows)
          (let ((raw (window-state-get (frame-root-window) t)))
            (should-not (and (consp raw)
                             (eq (car raw) window-state-plus--wrapper-tag)))
            (window-state-plus-put raw (frame-root-window))
            (should (get-buffer-window main))))
      (when (buffer-live-p main) (kill-buffer main)))))

;;; Tests for window-state-plus-advice-save-session

(ert-deftest window-state-plus-test/advice-save-session-zero-arg-fn ()
  "Regression: the advice must accept a zero-argument save function and
return its value.  The magit save-session advice relies on this via
`apply', which previously appended a spurious nil argument."
  (window-state-plus-test--with-layout
    (let ((ran nil))
      (should (eq 'result
                  (window-state-plus-advice-save-session
                   (lambda () (setq ran t) 'result))))
      (should ran))))

(provide 'window-state-plus-test)

;;; window-state-plus-test.el ends here
