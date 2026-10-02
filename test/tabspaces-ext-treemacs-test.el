;;; tabspaces-ext-treemacs-test.el --- Tests for tabspaces-ext-treemacs -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for the slimmed treemacs integration: the per-tab project
;; resolver registered in `treemacs--find-user-project-functions', and the
;; restore/cleanup `--reconcile' that resets a mapped tab's workspace to its
;; single project.  treemacs-tab-bar now provides per-tab workspaces and
;; tab-switch sync natively, so this module no longer does that itself.

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
;; registration and resolution use `featurep'/stubs), so it loads fine with
;; only tabspaces/tabspaces-ext present.
(require 'tabspaces-ext-treemacs)

;; Minimal stand-ins for treemacs' structs so `--reconcile' can be exercised in
;; batch (treemacs is not loaded here).  `cl-defstruct' must be top-level for its
;; slot `setf' expanders to register, so these are unconditional -- harmless
;; because the tests only ever run in batch without the real treemacs.
(cl-defstruct (treemacs-workspace
               (:constructor tabspaces-ext-test--make-ws)
               (:conc-name treemacs-workspace->))
  name projects)
(cl-defstruct (treemacs-project
               (:constructor treemacs-project->create!)
               (:conc-name treemacs-project->))
  name path path-status)

;;; Helper

(defmacro with-temp-project-dir (var &rest body)
  "Create a temporary directory, bind it to VAR, execute BODY, then clean up."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory
                (make-temp-file "tabspaces-ext-test-" t))))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

;;; Tests for --user-project-function (treemacs-tab-bar workspace resolution)

(ert-deftest tabspaces-ext-treemacs-test/user-project-from-mapping ()
  "A mapped tab resolves to its existing root, so treemacs-tab-bar creates the
tab's workspace with the real project instead of copying the fallback."
  (with-temp-project-dir dir
    (let ((tabspaces-project-tab-map (list (cons dir "proj@main"))))
      (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
                 (lambda () "proj@main")))
        ;; Without treemacs loaded (batch), the resolver falls back to
        ;; `expand-file-name', which keeps the trailing slash.  With treemacs it
        ;; would be `treemacs-canonical-path' (slash stripped); the point of the
        ;; test is that the mapped root -- not a fallback -- is returned.
        (should (string= (tabspaces-ext-treemacs--user-project-function)
                         (expand-file-name dir)))))))

(ert-deftest tabspaces-ext-treemacs-test/user-project-nil-when-unmapped ()
  "An unmapped tab yields nil so treemacs' built-in resolvers still apply."
  (let ((tabspaces-project-tab-map nil))
    (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
               (lambda () "Default")))
      (should-not (tabspaces-ext-treemacs--user-project-function)))))

(ert-deftest tabspaces-ext-treemacs-test/user-project-nil-when-root-missing ()
  "A mapped root that no longer exists yields nil (never a stale path)."
  (let ((tabspaces-project-tab-map
         (list (cons "/tabspaces-ext/does/not/exist/" "proj@gone"))))
    (cl-letf (((symbol-function 'tabspaces-ext--get-current-tab-name)
               (lambda () "proj@gone")))
      (should-not (tabspaces-ext-treemacs--user-project-function)))))

;;; Tests for --needs-reconcile-p (pure)

(ert-deftest tabspaces-ext-treemacs-test/needs-reconcile-p ()
  "Reconcile is needed unless the workspace holds exactly the one mapped root."
  (should (tabspaces-ext-treemacs--needs-reconcile-p nil "/a"))
  (should (tabspaces-ext-treemacs--needs-reconcile-p '("/a" "/b") "/a"))
  (should (tabspaces-ext-treemacs--needs-reconcile-p '("/b") "/a"))
  (should-not (tabspaces-ext-treemacs--needs-reconcile-p '("/a") "/a")))

;;; Tests for --reconcile

(defmacro tabspaces-ext-treemacs-test--with-treemacs-stubs (ws consolidated &rest body)
  "Run BODY with treemacs functions stubbed; WS is the only workspace.
CONSOLIDATED is a variable set to t when `treemacs--consolidate-projects' runs."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'featurep) (lambda (f) (or (eq f 'treemacs) nil)))
             ((symbol-function 'treemacs-canonical-path)
              (lambda (p) (directory-file-name p)))
             ((symbol-function 'treemacs--get-path-status)
              (lambda (_) 'local-readable))
             ((symbol-function 'treemacs--find-workspace-by-name)
              (lambda (name) (and (string= name (treemacs-workspace->name ,ws)) ,ws)))
             ((symbol-function 'treemacs--consolidate-projects)
              (lambda () (setq ,consolidated t)))
             ((symbol-function 'treemacs--persist) (lambda () nil)))
     ,@body))

(ert-deftest tabspaces-ext-treemacs-test/reconcile-resets-stale-workspace ()
  "A tab workspace holding the wrong project is reset to the mapped one and the
buffers are redrawn via `treemacs--consolidate-projects'."
  (with-temp-project-dir dir
    (let* ((croot (directory-file-name (file-truename dir)))
           (ws (tabspaces-ext-test--make-ws
                :name "Tab proj@main"
                :projects (list (treemacs-project->create!
                                 :name "stale" :path "/some/other/repo"
                                 :path-status 'local-readable))))
           (tabspaces-project-tab-map (list (cons dir "proj@main")))
           (consolidated nil))
      (tabspaces-ext-treemacs-test--with-treemacs-stubs ws consolidated
        (should (tabspaces-ext-treemacs--reconcile))
        (should consolidated)
        (should (equal (mapcar #'treemacs-project->path
                               (treemacs-workspace->projects ws))
                       (list croot)))))))

(ert-deftest tabspaces-ext-treemacs-test/reconcile-skips-correct-workspace ()
  "A tab workspace already showing exactly its one project is left untouched."
  (with-temp-project-dir dir
    (let* ((croot (directory-file-name (file-truename dir)))
           (ws (tabspaces-ext-test--make-ws
                :name "Tab proj@main"
                :projects (list (treemacs-project->create!
                                 :name "ok" :path croot
                                 :path-status 'local-readable))))
           (tabspaces-project-tab-map (list (cons dir "proj@main")))
           (consolidated nil))
      (tabspaces-ext-treemacs-test--with-treemacs-stubs ws consolidated
        (should-not (tabspaces-ext-treemacs--reconcile))
        (should-not consolidated)))))

(ert-deftest tabspaces-ext-treemacs-test/reconcile-ignores-missing-root ()
  "A mapping whose root no longer exists is skipped (no reset, no redraw)."
  (let* ((ws (tabspaces-ext-test--make-ws
              :name "Tab proj@gone"
              :projects (list (treemacs-project->create!
                               :name "stale" :path "/some/other/repo"
                               :path-status 'local-readable))))
         (tabspaces-project-tab-map
          (list (cons "/tabspaces-ext/does/not/exist/" "proj@gone")))
         (consolidated nil))
    (tabspaces-ext-treemacs-test--with-treemacs-stubs ws consolidated
      (should-not (tabspaces-ext-treemacs--reconcile))
      (should-not consolidated))))

(provide 'tabspaces-ext-treemacs-test)

;;; tabspaces-ext-treemacs-test.el ends here
