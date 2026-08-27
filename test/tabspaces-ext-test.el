;;; tabspaces-ext-test.el --- Tests for tabspaces-ext core -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for the cross-workspace buffer leak filter in tabspaces-ext.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Stub out dependencies that aren't available in batch mode so the module
;; under test can be loaded standalone.
(unless (featurep 'tabspaces)
  (defvar tabspaces-project-tab-map nil)
  (defvar tab-bar-tabs-function #'ignore)
  (provide 'tabspaces))

(unless (featurep 'window-state-plus)
  (provide 'window-state-plus))

;; The real `tabspaces--store-buffers' lives in tabspaces; stub it so the
;; mode can attach its `:filter-return' advice in batch.  Returns whatever
;; `tabspaces-ext-test--raw' holds, standing in for the serialized records.
(defvar tabspaces-ext-test--raw nil)
(unless (fboundp 'tabspaces--store-buffers)
  (defun tabspaces--store-buffers (&rest _) tabspaces-ext-test--raw))

(load "tabspaces-ext" nil t)

;;; Tests for --filter-foreign-buffers

(ert-deftest tabspaces-ext-test/filter-drops-foreign-project-buffers ()
  "A project tab's save keeps its own buffers and loose files, drops others."
  (let ((tabspaces-project-tab-map
         '(("/p/alpha/" . "alpha@main")
           ("/p/beta/" . "beta@main"))))
    (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
               (lambda () "alpha@main")))
      (let* ((records
              (list "/p/alpha/src/foo.c"                 ; own project file
                    '(:kind dired :dir "/p/alpha/src/")   ; own project dired
                    '(:kind popterm :dir "/p/alpha/")     ; own project kind
                    "/p/beta/report.el"                   ; other project file
                    '(:kind dired :dir "/p/beta/")        ; other project dired
                    '(:kind treemacs :dir "/p/beta/")     ; foreign treemacs
                    "/tmp/loose/notes.org"))              ; loose, no project
             (out (tabspaces-ext--filter-foreign-buffers records)))
        (should (member "/p/alpha/src/foo.c" out))
        (should (member '(:kind dired :dir "/p/alpha/src/") out))
        (should (member '(:kind popterm :dir "/p/alpha/") out))
        (should (member "/tmp/loose/notes.org" out))
        (should-not (member "/p/beta/report.el" out))
        (should-not (member '(:kind dired :dir "/p/beta/") out))
        (should-not (member '(:kind treemacs :dir "/p/beta/") out))
        (should (= 4 (length out)))))))

(ert-deftest tabspaces-ext-test/filter-keeps-all-for-nonproject-tab ()
  "A non-project tab (no mapped root) keeps every record."
  (let ((tabspaces-project-tab-map '(("/p/beta/" . "beta@main"))))
    (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
               (lambda () "*scratch*")))
      (let ((records (list "/p/beta/x.el" "/tmp/loose/y.txt")))
        (should (equal records (tabspaces-ext--filter-foreign-buffers records)))))))

(ert-deftest tabspaces-ext-test/filter-resolves-numbered-tab-suffix ()
  "A numbered tab like \"proj<2>\" resolves to the base project's root."
  (let ((tabspaces-project-tab-map '(("/p/alpha/" . "alpha@main"))))
    (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
               (lambda () "alpha@main<2>")))
      (let* ((records (list "/p/alpha/a.c" "/p/other/b.c"))
             (out (tabspaces-ext--filter-foreign-buffers records)))
        ;; /p/other is not mapped, so it stays; the point is the base tab
        ;; still resolves so /p/alpha/a.c is recognized as own.
        (should (member "/p/alpha/a.c" out))))))

(ert-deftest tabspaces-ext-test/foreign-record-p-keeps-record-without-dir ()
  "A record with no directory is never treated as foreign."
  (let ((tabspaces-project-tab-map '(("/p/beta/" . "beta@main"))))
    (should-not (tabspaces-ext--foreign-record-p '(:kind popterm) "/p/alpha/"))
    (should-not (tabspaces-ext--foreign-record-p '(:kind treemacs :dir nil) "/p/alpha/"))))

;;; Integration: the mode wires the filter onto `tabspaces--store-buffers'

(ert-deftest tabspaces-ext-test/mode-installs-and-removes-store-buffers-advice ()
  "Enabling `tabspaces-ext-mode' must filter saves; disabling must stop.
The unit tests above call `tabspaces-ext--filter-foreign-buffers' directly,
so they still pass even if the mode forgot to install the advice (or a later
edit removed it).  This guards the hookup itself: with the mode on, every
`tabspaces--store-buffers' call -- which is how tabspaces serializes a tab's
buffers on save -- must have the foreign-buffer filter applied, and turning
the mode off must fully restore the unfiltered behaviour."
  (let ((tabspaces-project-tab-map '(("/p/alpha/" . "alpha@main")
                                     ("/p/beta/" . "beta@main")))
        (tabspaces-ext-test--raw (list "/p/alpha/keep.c" "/p/beta/foreign.el")))
    (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
               (lambda () "alpha@main")))
      (unwind-protect
          (progn
            (tabspaces-ext-mode 1)
            (should (advice-member-p #'tabspaces-ext--filter-foreign-buffers
                                     'tabspaces--store-buffers))
            (let ((out (tabspaces--store-buffers nil)))
              (should (member "/p/alpha/keep.c" out))
              (should-not (member "/p/beta/foreign.el" out)))
            (tabspaces-ext-mode -1)
            (should-not (advice-member-p #'tabspaces-ext--filter-foreign-buffers
                                         'tabspaces--store-buffers))
            (should (equal tabspaces-ext-test--raw (tabspaces--store-buffers nil))))
        (tabspaces-ext-mode -1)))))

(provide 'tabspaces-ext-test)
;;; tabspaces-ext-test.el ends here
