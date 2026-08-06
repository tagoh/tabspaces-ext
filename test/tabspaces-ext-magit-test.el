;;; tabspaces-ext-magit-test.el --- Tests for tabspaces-ext-magit -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for the non-git directory skip logic in tabspaces-ext-magit.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Stub out dependencies that aren't available in batch mode
(unless (featurep 'tabspaces)
  (defvar tabspaces-project-tab-map nil)
  (defvar tab-bar-tabs-function #'tab-bar-tabs)
  (defun tabspaces-generate-descriptive-tab-name (_path _names) nil)
  (provide 'tabspaces))

(unless (featurep 'tabspaces-ext)
  (defun tabspaces-ext--add-project-tab-mapping (root name)
    (push (cons root name) tabspaces-project-tab-map))
  (defun tabspaces-ext--get-all-tab-names () nil)
  (defun tabspaces-ext--get-current-tab-name () "Default")
  (defvar tabspaces-session-project-session-store nil)
  (provide 'tabspaces-ext))

;; Load the real tabspaces-ext for cleanup-sessions (re-defines functions
;; but that's fine since the stubs above are compatible defaults).
(load "tabspaces-ext" nil t)

(unless (featurep 'window-state-plus)
  (defun window-state-plus-advice-save-session (_fn _args) nil)
  (provide 'window-state-plus))

;; project--list must be special so `let' creates a dynamic binding
;; visible to `boundp' inside the functions under test.
(defvar project--list nil)

;; Provide magit stubs before loading the module under test
(unless (featurep 'magit-worktree)
  (provide 'magit-worktree))
(unless (fboundp 'magit-toplevel)
  (defun magit-toplevel () nil))
(unless (fboundp 'magit-get)
  (defun magit-get (&rest _args) nil))
(unless (fboundp 'magit-gitdir)
  (defun magit-gitdir () nil))
(unless (fboundp 'magit-get-current-branch)
  (defun magit-get-current-branch () nil))

(require 'tabspaces-ext-magit)

;;; Helper

(defmacro with-temp-project-dir (var &rest body)
  "Create a temporary directory, bind it to VAR, execute BODY, then clean up."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory
                (make-temp-file "tabspaces-ext-test-" t))))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

;;; Tests for --generate-descriptive-tab-name-advice

(ert-deftest tabspaces-ext-magit-test/tab-name-advice-skips-non-git-dir ()
  "Advice should call orig-fun for a directory without .git."
  (with-temp-project-dir dir
    (let ((orig-called nil))
      (tabspaces-ext-magit--generate-descriptive-tab-name-advice
       (lambda (path _names)
         (setq orig-called t)
         (file-name-nondirectory (directory-file-name path)))
       dir '())
      (should orig-called))))

(ert-deftest tabspaces-ext-magit-test/tab-name-advice-returns-default-name-for-non-git ()
  "Advice should return the default name (no @branch) for non-git directories."
  (with-temp-project-dir dir
    (let ((result
           (tabspaces-ext-magit--generate-descriptive-tab-name-advice
            (lambda (path _names)
              (file-name-nondirectory (directory-file-name path)))
            dir '())))
      (should-not (string-match-p "@" result)))))

(ert-deftest tabspaces-ext-magit-test/tab-name-advice-uses-git-format-for-git-dir ()
  "Advice should return project@branch for a directory with .git."
  (with-temp-project-dir dir
    (make-directory (expand-file-name ".git" dir))
    (cl-letf (((symbol-function 'magit-toplevel) (lambda () dir))
              ((symbol-value 'tabspaces-ext-magit-tab-name-function)
               (lambda (&optional _wt) "myproject@main"))
              ((symbol-value 'tabspaces-project-tab-map) nil))
      (let ((result
             (tabspaces-ext-magit--generate-descriptive-tab-name-advice
              (lambda (_path _names) "should-not-be-used")
              dir '())))
        (should (string= result "myproject@main"))))))

(ert-deftest tabspaces-ext-magit-test/tab-name-advice-no-magit-call-for-non-git ()
  "Advice should never call magit-toplevel for non-git directories."
  (with-temp-project-dir dir
    (let ((magit-called nil))
      (cl-letf (((symbol-function 'magit-toplevel)
                 (lambda () (setq magit-called t) dir)))
        (tabspaces-ext-magit--generate-descriptive-tab-name-advice
         (lambda (path _names)
           (file-name-nondirectory (directory-file-name path)))
         dir '()))
      (should-not magit-called))))

;;; Tests for --repair-project-tab-mappings

(ert-deftest tabspaces-ext-magit-test/repair-skips-non-git-project ()
  "Repair should not add mappings for projects without .git."
  (with-temp-project-dir dir
    (let ((tabspaces-project-tab-map nil)
          (project--list (list (list dir)))
          (magit-called nil))
      (cl-letf (((symbol-function 'tabspaces-ext--get-all-tab-names)
                 (lambda () '("somerepo@main")))
                ((symbol-function 'magit-toplevel)
                 (lambda () (setq magit-called t) dir)))
        (tabspaces-ext-magit--repair-project-tab-mappings)
        (should-not magit-called)
        (should (null tabspaces-project-tab-map))))))

(ert-deftest tabspaces-ext-magit-test/repair-removes-stale-mapping-for-non-git ()
  "Repair should remove @-mappings whose project root has no .git."
  (with-temp-project-dir dir
    (let ((tabspaces-project-tab-map
           (list (cons dir "myproject@main")))
          (project--list (list (list dir))))
      (cl-letf (((symbol-function 'tabspaces-ext--get-all-tab-names)
                 (lambda () nil)))
        (tabspaces-ext-magit--repair-project-tab-mappings)
        (should (null tabspaces-project-tab-map))))))

(ert-deftest tabspaces-ext-magit-test/repair-keeps-mapping-for-git-project ()
  "Repair should not remove @-mappings whose project root has .git."
  (with-temp-project-dir dir
    (make-directory (expand-file-name ".git" dir))
    (let ((tabspaces-project-tab-map
           (list (cons dir "myproject@main")))
          (project--list (list (list dir))))
      (cl-letf (((symbol-function 'tabspaces-ext--get-all-tab-names)
                 (lambda () '("myproject@main"))))
        (tabspaces-ext-magit--repair-project-tab-mappings)
        (should (= 1 (length tabspaces-project-tab-map)))
        (should (string= (cdar tabspaces-project-tab-map) "myproject@main"))))))

(ert-deftest tabspaces-ext-magit-test/repair-adds-mapping-for-git-project ()
  "Repair should add mapping for projects with .git."
  (with-temp-project-dir dir
    (make-directory (expand-file-name ".git" dir))
    (let ((tabspaces-project-tab-map nil)
          (project--list (list (list dir))))
      (cl-letf (((symbol-function 'tabspaces-ext--get-all-tab-names)
                 (lambda () '("myrepo@develop")))
                ((symbol-function 'magit-toplevel)
                 (lambda () dir))
                ((symbol-value 'tabspaces-ext-magit-tab-name-function)
                 (lambda (&optional _wt) "myrepo@develop")))
        (tabspaces-ext-magit--repair-project-tab-mappings)
        (should (= 1 (length tabspaces-project-tab-map)))
        (should (string= (cdar tabspaces-project-tab-map) "myrepo@develop"))))))

;;; Tests for --worktree-ensure-tabspace

(ert-deftest tabspaces-ext-magit-test/ensure-tabspace-skips-non-git-project-tab ()
  "Should not create a second tab when current tab is for a non-git project."
  (with-temp-project-dir dir
    (let ((tabspaces-project-tab-map (list (cons dir "myproject")))
          (new-tab-created nil))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t))
                ((symbol-function 'tabspaces-ext--get-current-tab-name)
                 (lambda () "myproject"))
                ((symbol-value 'tabspaces-ext-magit-tab-name-function)
                 (lambda (&optional _wt) "myproject@main"))
                ((symbol-function 'tabspaces-ext--get-all-tab-names)
                 (lambda () '("myproject")))
                ((symbol-function 'tab-bar-new-tab)
                 (lambda (&rest _) (setq new-tab-created t)))
                ((symbol-function 'tab-bar-rename-tab)
                 (lambda (&rest _) nil)))
        (let ((default-directory dir))
          (tabspaces-ext-magit--worktree-ensure-tabspace))
        (should-not new-tab-created)))))

(ert-deftest tabspaces-ext-magit-test/ensure-tabspace-proceeds-for-git-project-tab ()
  "Should create/switch tab when current tab is for a git project."
  (with-temp-project-dir dir
    (make-directory (expand-file-name ".git" dir))
    (let ((tabspaces-project-tab-map (list (cons dir "myproject@main")))
          (new-tab-created nil))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t))
                ((symbol-function 'tabspaces-ext--get-current-tab-name)
                 (lambda () "myproject@main"))
                ((symbol-value 'tabspaces-ext-magit-tab-name-function)
                 (lambda (&optional _wt) "myproject@feature"))
                ((symbol-function 'tabspaces-ext--get-all-tab-names)
                 (lambda () '("myproject@main")))
                ((symbol-function 'tab-bar-new-tab)
                 (lambda (&rest _) (setq new-tab-created t)))
                ((symbol-function 'tab-bar-rename-tab)
                 (lambda (&rest _) nil)))
        (let ((default-directory dir))
          (tabspaces-ext-magit--worktree-ensure-tabspace))
        (should new-tab-created)))))

(ert-deftest tabspaces-ext-magit-test/ensure-tabspace-proceeds-for-unmapped-tab ()
  "Should proceed when current tab has no project mapping."
  (with-temp-project-dir dir
    (let ((tabspaces-project-tab-map nil)
          (new-tab-created nil))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t))
                ((symbol-function 'tabspaces-ext--get-current-tab-name)
                 (lambda () "Default"))
                ((symbol-value 'tabspaces-ext-magit-tab-name-function)
                 (lambda (&optional _wt) "myproject@main"))
                ((symbol-function 'tabspaces-ext--get-all-tab-names)
                 (lambda () '("Default")))
                ((symbol-function 'tab-bar-new-tab)
                 (lambda (&rest _) (setq new-tab-created t)))
                ((symbol-function 'tab-bar-rename-tab)
                 (lambda (&rest _) nil)))
        (let ((default-directory dir))
          (tabspaces-ext-magit--worktree-ensure-tabspace))
        (should new-tab-created)))))

;;; Tests for --clean-non-git-mappings

(ert-deftest tabspaces-ext-magit-test/clean-removes-non-git-at-mappings ()
  "Should remove @-mappings for directories without .git."
  (with-temp-project-dir dir
    (let ((tabspaces-project-tab-map
           (list (cons dir "myproject@main"))))
      (tabspaces-ext-magit--clean-non-git-mappings)
      (should (null tabspaces-project-tab-map)))))

(ert-deftest tabspaces-ext-magit-test/clean-keeps-git-at-mappings ()
  "Should keep @-mappings for directories with .git."
  (with-temp-project-dir dir
    (make-directory (expand-file-name ".git" dir))
    (let ((tabspaces-project-tab-map
           (list (cons dir "myproject@main"))))
      (tabspaces-ext-magit--clean-non-git-mappings)
      (should (= 1 (length tabspaces-project-tab-map))))))

(ert-deftest tabspaces-ext-magit-test/clean-keeps-non-at-mappings ()
  "Should keep mappings without @ regardless of .git."
  (with-temp-project-dir dir
    (let ((tabspaces-project-tab-map
           (list (cons dir "myproject"))))
      (tabspaces-ext-magit--clean-non-git-mappings)
      (should (= 1 (length tabspaces-project-tab-map))))))

;;; Tests for tabspaces-ext-cleanup-sessions

(ert-deftest tabspaces-ext-magit-test/cleanup-sessions-removes-missing-dir ()
  "Should delete session files whose project directory does not exist."
  (with-temp-project-dir store
    (let ((tabspaces-session-project-session-store store)
          (session-file (expand-file-name ".gone-project-tabspaces-session.el" store)))
      (with-temp-file session-file
        (insert "(setq tabspaces-project-tab-map '((\"/no/such/dir/\" . \"gone-project@main\")))\n")
        (insert "(setq tabspaces--session-list '(((()) \"gone-project@main\" nil)))\n"))
      (tabspaces-ext-cleanup-sessions)
      (should-not (file-exists-p session-file)))))

(ert-deftest tabspaces-ext-magit-test/cleanup-sessions-removes-stale-at-name ()
  "Should delete session files with @branch name for non-git project."
  (with-temp-project-dir store
    (with-temp-project-dir project-dir
      (let ((tabspaces-session-project-session-store store)
            (session-file (expand-file-name ".stale-project-tabspaces-session.el" store)))
        (with-temp-file session-file
          (insert (format "(setq tabspaces-project-tab-map '((%S . \"myproject@main\")))\n" project-dir))
          (insert "(setq tabspaces--session-list '(((()) \"myproject@main\" nil)))\n"))
        (tabspaces-ext-cleanup-sessions)
        (should-not (file-exists-p session-file))))))

(ert-deftest tabspaces-ext-magit-test/cleanup-sessions-keeps-valid-files ()
  "Should keep session files whose project directory exists and has no stale data."
  (with-temp-project-dir store
    (with-temp-project-dir project-dir
      (let ((tabspaces-session-project-session-store store)
            (session-file (expand-file-name ".valid-project-tabspaces-session.el" store)))
        (with-temp-file session-file
          (insert (format "(setq tabspaces-project-tab-map '((%S . \"valid-project\")))\n" project-dir))
          (insert "(setq tabspaces--session-list '(((()) \"valid-project\" nil)))\n"))
        (tabspaces-ext-cleanup-sessions)
        (should (file-exists-p session-file))))))

(ert-deftest tabspaces-ext-magit-test/cleanup-sessions-keeps-git-at-files ()
  "Should keep session files with @branch name when project has .git."
  (with-temp-project-dir store
    (with-temp-project-dir project-dir
      (make-directory (expand-file-name ".git" project-dir))
      (let ((tabspaces-session-project-session-store store)
            (session-file (expand-file-name ".git-project-tabspaces-session.el" store)))
        (with-temp-file session-file
          (insert (format "(setq tabspaces-project-tab-map '((%S . \"myrepo@main\")))\n" project-dir))
          (insert "(setq tabspaces--session-list '(((()) \"myrepo@main\" nil)))\n"))
        (tabspaces-ext-cleanup-sessions)
        (should (file-exists-p session-file))))))

(provide 'tabspaces-ext-magit-test)

;;; tabspaces-ext-magit-test.el ends here
