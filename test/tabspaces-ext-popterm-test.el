;;; tabspaces-ext-popterm-test.el --- Tests for tabspaces-ext-popterm -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for the popterm layout integration.
;;
;; Two regressions are covered here, both of which only reproduce with a
;; realistic multi-window layout -- a left side window (like the Treemacs
;; sidebar) plus a main window:
;;
;; 1. Placement: popterm must open as a *full-width* bottom side window.
;;    popterm's own `popterm--window-show' splits `frame-root-window',
;;    which only covers the main area when a vertical side window is
;;    present, leaving popterm in the bottom-*right* corner.  A single-
;;    window test cannot catch this because there is no side window to be
;;    pushed past, so these tests build the sidebar and assert geometry
;;    (window-side, and the window's left edge reaching the frame edge).
;;
;; 2. Reopen guard: the layout fixer keyed off buffer existence alone, so
;;    it re-opened a popterm the user had closed on every tab switch.  It
;;    must only reposition a popterm that is actually displayed.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Stub out dependencies that aren't available in batch mode.
(unless (featurep 'tabspaces)
  (defun tabspaces--current-tab-name () "T1")
  (defun tabspaces-register-buffer-kind (&rest _) nil)
  (defun tabspaces-reuse-existing-buffer (&rest _) nil)
  (defvar tabspaces-project-tab-map nil)
  (provide 'tabspaces))

(unless (featurep 'tabspaces-ext)
  (defun tabspaces-ext--get-current-tab-name () "T1")
  (provide 'tabspaces-ext))

;; popterm symbols referenced by the module under test.
(defvar popterm-backend 'ghostel)
(defvar popterm-window-height-ratio 0.3)
(defvar popterm--window nil)
(defvar popterm--active-display-method nil)
(unless (fboundp 'popterm-toggle)
  (defun popterm-toggle (&rest _) nil))
(unless (fboundp 'popterm--reset-cursor-point)
  (defun popterm--reset-cursor-point (&rest _) nil))

(require 'tabspaces-ext-popterm)

;;; Helpers

(defun tabspaces-ext-popterm-test--delete-side-windows ()
  "Delete every side window in the selected frame."
  (dolist (w (window-list))
    (when (window-parameter w 'window-side)
      (ignore-errors (delete-window w)))))

(defun tabspaces-ext-popterm-test--popterm-window ()
  "Return the live window showing the test popterm buffer, or nil."
  (get-buffer-window "*popterm-ghostel[T1]*"))

(defun tabspaces-ext-popterm-test--side-window (side)
  "Return the live side window on SIDE, or nil."
  (cl-find-if (lambda (w) (eq (window-parameter w 'window-side) side))
              (window-list)))

(defmacro tabspaces-ext-popterm-test--with-side-layout (&rest body)
  "Build a Treemacs-like left side window plus a main window, run BODY, clean up.

`popterm-toggle' is stubbed to actually show/hide the popterm buffer via
the real `tabspaces-ext-popterm--window-show-at-bottom', so tests can
assert the resulting window geometry instead of a bare boolean.  Binds
BUF to the tab's popterm buffer."
  (declare (indent 0))
  `(let ((side (get-buffer-create "*tepop-side*"))
         (main (get-buffer-create "*tepop-main*"))
         (buf (get-buffer-create "*popterm-ghostel[T1]*")))
     (unwind-protect
         (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
                    (lambda () "T1"))
                   ((symbol-function 'tabspaces-ext-popterm--get-project-dir)
                    (lambda (_tab) "/tmp/"))
                   ((symbol-function 'tabspaces-ext-popterm--set-directory)
                    (lambda (&rest _) nil))
                   ;; Realistic toggle: hide when visible, otherwise show via
                   ;; the real full-width-bottom placement.
                   ((symbol-function 'popterm-toggle)
                    (lambda (&rest _)
                      (if-let* ((w (get-buffer-window buf)))
                          (delete-window w)
                        (tabspaces-ext-popterm--window-show-at-bottom
                         #'ignore buf)))))
           (switch-to-buffer main)
           (delete-other-windows)
           (tabspaces-ext-popterm-test--delete-side-windows)
           ;; Treemacs-style full-height left sidebar.
           (set-window-dedicated-p
            (display-buffer-in-side-window
             side '((side . left) (slot . 0) (window-width . 20)))
            t)
           ,@body)
       (tabspaces-ext-popterm-test--delete-side-windows)
       (delete-other-windows)
       (dolist (b (list side main buf))
         (when (buffer-live-p b) (kill-buffer b))))))

(defun tabspaces-ext-popterm-test--assert-full-width-bottom (win)
  "Assert WIN is a full-width bottom side window sitting below the sidebar.
This is the check a single-window test cannot make: the old split-based
placement produced a non-side window whose left edge started at the
sidebar's right edge (the \"right-bottom\" bug)."
  (should (window-live-p win))
  ;; It is a real bottom side window, not a plain split.
  (should (eq (window-parameter win 'window-side) 'bottom))
  ;; It reaches the frame's left edge -- i.e. it spans *under* the left
  ;; sidebar rather than starting to its right.
  (should (= (nth 0 (window-edges win)) 0))
  ;; The sidebar was pushed up: its bottom edge meets popterm's top edge.
  (let ((sidebar (tabspaces-ext-popterm-test--side-window 'left)))
    (should sidebar)
    (should (= (nth 3 (window-edges sidebar)) (nth 1 (window-edges win))))
    (should (< (nth 3 (window-edges sidebar)) (nth 3 (window-edges win))))))

;;; Tests -- placement

(ert-deftest tabspaces-ext-popterm-test/window-show-spans-full-width ()
  "With a left sidebar present, popterm opens as a full-width bottom side
window, not a main-area split parked in the bottom-right corner."
  (tabspaces-ext-popterm-test--with-side-layout
    (let ((win (tabspaces-ext-popterm--window-show-at-bottom #'ignore buf)))
      (tabspaces-ext-popterm-test--assert-full-width-bottom win))))

(ert-deftest tabspaces-ext-popterm-test/window-show-falls-back-when-no-side-window ()
  "When the side window cannot be created, fall back to ORIG-FUN and forward
its return value; no popterm bottom side window is left behind."
  (tabspaces-ext-popterm-test--with-side-layout
    (let* ((orig-buf nil)
           (orig (lambda (b) (setq orig-buf b) 'fallback))
           (ret (cl-letf (((symbol-function 'display-buffer-in-side-window)
                           (lambda (&rest _) nil)))
                  (tabspaces-ext-popterm--window-show-at-bottom orig buf))))
      (should (eq orig-buf buf))
      (should (eq ret 'fallback))
      (should-not (tabspaces-ext-popterm-test--popterm-window)))))

;;; Tests -- reopen guard / restore, verifying real placement

(ert-deftest tabspaces-ext-popterm-test/fix-layout-does-not-reopen-closed ()
  "When the popterm buffer exists but is not displayed, --fix-layout must
not reopen it."
  (tabspaces-ext-popterm-test--with-side-layout
    (should-not (tabspaces-ext-popterm-test--popterm-window))
    (tabspaces-ext-popterm--fix-layout)
    (should-not (tabspaces-ext-popterm-test--popterm-window))))

(ert-deftest tabspaces-ext-popterm-test/fix-layout-repositions-visible ()
  "When popterm is displayed, --fix-layout repositions it and keeps it a
full-width bottom side window."
  (tabspaces-ext-popterm-test--with-side-layout
    (tabspaces-ext-popterm--window-show-at-bottom #'ignore buf)
    (should (tabspaces-ext-popterm-test--popterm-window))
    (tabspaces-ext-popterm--fix-layout)
    (tabspaces-ext-popterm-test--assert-full-width-bottom
     (tabspaces-ext-popterm-test--popterm-window))))

(provide 'tabspaces-ext-popterm-test)

;;; tabspaces-ext-popterm-test.el ends here
