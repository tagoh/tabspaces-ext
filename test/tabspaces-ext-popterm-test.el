;;; tabspaces-ext-popterm-test.el --- Tests for tabspaces-ext-popterm -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for `tabspaces-ext-popterm--fix-layout'.  Regression: the
;; layout fixer keyed off buffer existence alone, so it re-opened a
;; popterm the user had closed on every tab switch -- including the tab
;; cycling done by the session auto-save.  It must now only reposition a
;; popterm that is actually displayed.

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
(unless (fboundp 'popterm-toggle)
  (defun popterm-toggle (&rest _) nil))

(require 'tabspaces-ext-popterm)

;;; Helper

(defmacro tabspaces-ext-popterm-test--with-buf (&rest body)
  "Create the current tab's popterm buffer, run BODY, then clean up."
  (declare (indent 0))
  `(let ((buf (get-buffer-create "*popterm-ghostel[T1]*"))
         (toggled nil))
     (unwind-protect
         (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
                    (lambda () "T1"))
                   ((symbol-function 'tabspaces-ext-popterm--get-project-dir)
                    (lambda (_tab) "/tmp/"))
                   ((symbol-function 'popterm-toggle)
                    (lambda (&rest _) (setq toggled t)))
                   ((symbol-function 'tabspaces-ext-popterm--set-directory)
                    (lambda (&rest _) nil)))
           ,@body)
       (delete-other-windows)
       (when (buffer-live-p buf) (kill-buffer buf)))))

;;; Tests

(ert-deftest tabspaces-ext-popterm-test/fix-layout-does-not-reopen-closed ()
  "When the popterm buffer exists but is not displayed, --fix-layout must
not reopen it."
  (tabspaces-ext-popterm-test--with-buf
    ;; Buffer exists but is shown in no window.
    (switch-to-buffer "*scratch*")
    (delete-other-windows)
    (should-not (get-buffer-window-list buf nil t))
    (tabspaces-ext-popterm--fix-layout)
    (should-not toggled)))

(ert-deftest tabspaces-ext-popterm-test/fix-layout-repositions-visible ()
  "When popterm is displayed, --fix-layout repositions it (toggles)."
  (tabspaces-ext-popterm-test--with-buf
    (switch-to-buffer "*scratch*")
    (delete-other-windows)
    (set-window-buffer (split-window-below) buf)
    (should (get-buffer-window-list buf nil t))
    (tabspaces-ext-popterm--fix-layout)
    (should toggled)))

(provide 'tabspaces-ext-popterm-test)

;;; tabspaces-ext-popterm-test.el ends here
