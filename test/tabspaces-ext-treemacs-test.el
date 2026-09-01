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

(provide 'tabspaces-ext-treemacs-test)

;;; tabspaces-ext-treemacs-test.el ends here
