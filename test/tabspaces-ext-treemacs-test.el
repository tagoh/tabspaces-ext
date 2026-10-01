;;; tabspaces-ext-treemacs-test.el --- Tests for tabspaces-ext-treemacs -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for the treemacs project-root resolution.  These cover the
;; hardening that stops an unmapped project@branch tab from borrowing the
;; current buffer's project (which caused treemacs to sync to the wrong
;; project when a foreign buffer leaked into the tab).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'project)

;; Stub out dependencies that aren't available in batch mode
(unless (featurep 'tabspaces)
  (defvar tabspaces-project-tab-map nil)
  (defvar tab-bar-tabs-function #'tab-bar-tabs)
  (defun tabspaces-generate-descriptive-tab-name (_path _names) nil)
  (defun tabspaces-register-buffer-kind (&rest _args) nil)
  (provide 'tabspaces))

(unless (featurep 'window-state-plus)
  (defun window-state-plus-advice-save-session (_fn _args) nil)
  (provide 'window-state-plus))

;; Load the real tabspaces-ext so `tabspaces-ext--tab-project-root' and
;; `tabspaces-ext--get-current-tab-name' are the genuine implementations.
(load "tabspaces-ext" nil t)

;; The treemacs module does not require treemacs at load time (buffer-kind
;; registration and root resolution use `featurep'/stubs), so it loads fine
;; with only tabspaces/tabspaces-ext present.
(require 'tabspaces-ext-treemacs)

;;; Helper

(defmacro with-temp-project-dir (var &rest body)
  "Create a temporary directory, bind it to VAR, execute BODY, then clean up."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory
                (make-temp-file "tabspaces-ext-test-" t))))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

;;; Tests for --get-project-root-for-tab

(ert-deftest tabspaces-ext-treemacs-test/root-from-mapping ()
  "A mapped tab resolves to its mapped, existing project root."
  (with-temp-project-dir dir
    (let ((tabspaces-project-tab-map (list (cons dir "proj@main"))))
      (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
                 (lambda () "proj@main")))
        (should (string= (tabspaces-ext-treemacs--get-project-root-for-tab)
                         (expand-file-name dir)))))))

(ert-deftest tabspaces-ext-treemacs-test/root-nil-for-unmapped-at-tab ()
  "An unmapped project@branch tab returns nil and never calls
`project-current' -- otherwise a leaked foreign buffer would misdirect
the treemacs sync."
  (let ((tabspaces-project-tab-map nil))
    (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
               (lambda () "proj@feature"))
              ((symbol-function 'project-current)
               (lambda (&rest _) (error "project-current must not be called"))))
      (should-not (tabspaces-ext-treemacs--get-project-root-for-tab)))))

(ert-deftest tabspaces-ext-treemacs-test/root-falls-back-for-non-at-tab ()
  "A non-project-shaped tab (no @) may still fall back to `project-current'."
  (with-temp-project-dir dir
    (let ((tabspaces-project-tab-map nil))
      (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
                 (lambda () "scratchpad"))
                ((symbol-function 'project-current)
                 (lambda (&rest _) (list 'transient dir)))
                ((symbol-function 'project-root)
                 (lambda (_proj) dir)))
        (should (string= (tabspaces-ext-treemacs--get-project-root-for-tab)
                         (expand-file-name dir)))))))

;;; Tests for --cancel-pending-annotation-timers

(ert-deftest tabspaces-ext-treemacs-test/cancel-only-annotation-timers ()
  "Cancelling drops pending `treemacs--apply-annotations-deferred' timers and
leaves unrelated timers running.  This is what stops treemacs' deferred timer
from firing on a button our sync is about to delete (which crashes with
\"number-or-marker-p nil\")."
  (let* ((ran (list nil nil))
         ;; A deferred-annotation timer (must be cancelled) and an unrelated one
         ;; (must survive).
         (ann-timer (run-with-timer
                     100 nil #'treemacs--apply-annotations-deferred
                     nil nil nil nil))
         (other-timer (run-with-timer 100 nil #'ignore)))
    (unwind-protect
        (progn
          (should (memq ann-timer timer-list))
          (should (memq other-timer timer-list))
          (tabspaces-ext-treemacs--cancel-pending-annotation-timers)
          (should-not (memq ann-timer timer-list))
          (should (memq other-timer timer-list)))
      (ignore ran)
      (cancel-timer ann-timer)
      (cancel-timer other-timer))))

(ert-deftest tabspaces-ext-treemacs-test/cancel-with-no-timers ()
  "Cancelling is a no-op when no deferred-annotation timers are pending."
  (let ((before (length timer-list)))
    (tabspaces-ext-treemacs--cancel-pending-annotation-timers)
    (should (= before (length timer-list)))))

(ert-deftest tabspaces-ext-treemacs-test/cancel-tolerates-non-timer-entries ()
  "A nil (or non-timer) entry in `timer-list' must not crash the sweep.
`timer--function' is `aref'-based, so calling it on nil signals
\"(wrong-type-argument arrayp nil)\" -- the recurring \"Treemacs sync error\"
seen on tab switches.  The `timerp' guard skips such entries while still
cancelling a genuine deferred-annotation timer sitting beside the nil.

Uses `timer-create' (not `run-with-timer') so the timer is never registered
in the global `timer-list', keeping the test's binding fully isolated."
  (let* ((ann-timer (timer-create)))
    (timer-set-function ann-timer #'treemacs--apply-annotations-deferred)
    (let ((timer-list (list nil ann-timer)))
      (should (memq ann-timer timer-list))
      ;; Must not signal despite the leading nil entry.
      (tabspaces-ext-treemacs--cancel-pending-annotation-timers)
      (should-not (memq ann-timer timer-list)))))

(provide 'tabspaces-ext-treemacs-test)

;;; tabspaces-ext-treemacs-test.el ends here
