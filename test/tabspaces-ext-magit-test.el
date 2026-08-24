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

(ert-deftest tabspaces-ext-magit-test/ensure-tabspace-selects-magit-buffer-in-new-tab ()
  "Creating a new worktree tab should select the magit buffer.
Guards against the regression where `tab-bar-new-tab-choice' set to
\"*scratch*\" leaves the new tab on *scratch* instead of the worktree
status buffer."
  (with-temp-project-dir dir
    (with-temp-buffer
      (let ((magit-buffer (current-buffer))
            (selected-buffer nil)
            (tabspaces-project-tab-map nil)
            (default-directory dir))
        (cl-letf (((symbol-function 'derived-mode-p)
                   (lambda (&rest _modes) t))
                  ((symbol-function 'tabspaces-ext--get-current-tab-name)
                   (lambda () "Default"))
                  ((symbol-value 'tabspaces-ext-magit-tab-name-function)
                   (lambda (&optional _wt) "myproject@main"))
                  ((symbol-function 'tabspaces-ext--get-all-tab-names)
                   (lambda () '("Default")))
                  ((symbol-function 'tab-bar-new-tab)
                   (lambda (&rest _) nil))
                  ((symbol-function 'tab-bar-rename-tab)
                   (lambda (&rest _) nil))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buf &rest _) (setq selected-buffer buf))))
          (tabspaces-ext-magit--worktree-ensure-tabspace)
          (should (eq selected-buffer magit-buffer)))))))

(ert-deftest tabspaces-ext-magit-test/ensure-tabspace-non-vcs-target ()
  "Creating a tab for a non-VCS target uses the plain directory name.
Exercises the real name-resolution fallback (no stubbed tab-name
function): no remote, no branch, no .git, so the tab name has no
@branch and the magit buffer is still selected."
  (with-temp-project-dir dir
    (with-temp-buffer
      (let ((magit-buffer (current-buffer))
            (selected-buffer nil)
            (renamed-name nil)
            (tabspaces-project-tab-map nil)
            (default-directory dir))
        (cl-letf (((symbol-function 'derived-mode-p)
                   (lambda (&rest _modes) t))
                  ((symbol-function 'tabspaces-ext--get-current-tab-name)
                   (lambda () "Default"))
                  ((symbol-function 'tabspaces-ext--get-all-tab-names)
                   (lambda () '("Default")))
                  ;; Real magit lookups all fail for a non-VCS dir.
                  ((symbol-function 'magit-get) (lambda (&rest _) nil))
                  ((symbol-function 'magit-gitdir) (lambda () nil))
                  ((symbol-function 'magit-get-current-branch) (lambda () nil))
                  ((symbol-function 'tab-bar-new-tab) (lambda (&rest _) nil))
                  ((symbol-function 'tab-bar-rename-tab)
                   (lambda (name &rest _) (setq renamed-name name)))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buf &rest _) (setq selected-buffer buf))))
          (tabspaces-ext-magit--worktree-ensure-tabspace)
          ;; Tab named after the directory, with no @branch suffix.
          (should (string= renamed-name
                           (file-name-nondirectory (directory-file-name dir))))
          (should-not (string-match-p "@" renamed-name))
          ;; Mapping recorded and magit buffer selected despite no VCS.
          (should (= 1 (length tabspaces-project-tab-map)))
          (should (eq selected-buffer magit-buffer)))))))

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

;;; Tests for --branch-rename-advice

(ert-deftest tabspaces-ext-magit-test/branch-rename-updates-tab-and-mapping ()
  "Renaming a branch should rename the tab and update the mapping."
  (let* ((tabspaces-project-tab-map
          (list (cons "/path/to/repo/" "myproject@old-feature")))
         (renamed-to nil))
    (cl-letf (((symbol-function 'tabspaces-ext-magit--get-git-project-name)
               (lambda () "myproject"))
              ((symbol-function 'tabspaces-ext--get-all-tab-names)
               (lambda () '("myproject@old-feature")))
              ((symbol-function 'tabspaces-ext--find-tab-index)
               (lambda (_name) 0))
              ((symbol-function 'tab-bar-rename-tab)
               (lambda (name &rest _) (setq renamed-to name))))
      (tabspaces-ext-magit--branch-rename-advice
       (lambda (_old _new &optional _force) nil)
       "old-feature" "new-feature")
      (should (string= renamed-to "myproject@new-feature"))
      (should (string= (cdar tabspaces-project-tab-map)
                       "myproject@new-feature")))))

(ert-deftest tabspaces-ext-magit-test/branch-rename-no-op-when-no-matching-tab ()
  "Renaming a branch with no matching tab should not rename anything."
  (let* ((tabspaces-project-tab-map
          (list (cons "/path/to/repo/" "myproject@main")))
         (renamed nil))
    (cl-letf (((symbol-function 'tabspaces-ext-magit--get-git-project-name)
               (lambda () "myproject"))
              ((symbol-function 'tabspaces-ext--get-all-tab-names)
               (lambda () '("myproject@main")))
              ((symbol-function 'tab-bar-rename-tab)
               (lambda (&rest _) (setq renamed t))))
      (tabspaces-ext-magit--branch-rename-advice
       (lambda (_old _new &optional _force) nil)
       "other-branch" "renamed-branch")
      (should-not renamed)
      (should (string= (cdar tabspaces-project-tab-map)
                       "myproject@main")))))

(ert-deftest tabspaces-ext-magit-test/branch-rename-no-op-without-project-name ()
  "Should not fail when project name cannot be determined."
  (let* ((tabspaces-project-tab-map nil)
         (renamed nil))
    (cl-letf (((symbol-function 'tabspaces-ext-magit--get-git-project-name)
               (lambda () nil))
              ((symbol-function 'tab-bar-rename-tab)
               (lambda (&rest _) (setq renamed t))))
      (tabspaces-ext-magit--branch-rename-advice
       (lambda (_old _new &optional _force) nil)
       "old" "new")
      (should-not renamed))))

(ert-deftest tabspaces-ext-magit-test/branch-rename-calls-orig-fun ()
  "Should always call the original function."
  (let* ((orig-called nil)
         (orig-args nil))
    (cl-letf (((symbol-function 'tabspaces-ext-magit--get-git-project-name)
               (lambda () "myproject"))
              ((symbol-function 'tabspaces-ext--get-all-tab-names)
               (lambda () nil)))
      (tabspaces-ext-magit--branch-rename-advice
       (lambda (old new &optional force)
         (setq orig-called t
               orig-args (list old new force)))
       "old-branch" "new-branch" t)
      (should orig-called)
      (should (equal orig-args '("old-branch" "new-branch" t))))))

;;; Tests for --save-session-advice

(ert-deftest tabspaces-ext-magit-test/save-session-advice-forwards-args ()
  "Advice must forward orig-fun to the layout wrapper via `apply', not pass
the `&rest' list as a single argument.  Regression: the buggy form called
the zero-arg `tabspaces-save-session' with a spurious nil, signalling
`wrong-number-of-arguments'."
  (let ((received 'unset))
    (cl-letf (((symbol-function 'window-state-plus-advice-save-session)
               (lambda (fn &rest args) (setq received (cons fn args))))
              ((symbol-function 'tabspaces-ext-magit--clean-non-git-mappings)
               (lambda () nil)))
      (let ((orig (lambda () 'ok)))
        (tabspaces-ext-magit--save-session-advice orig)
        ;; orig-fun forwarded as the first arg, with NO extra args appended.
        (should (eq (car received) orig))
        (should (null (cdr received)))))))

;;; Tests for tabspaces-ext--is-system-buffer-p

(ert-deftest tabspaces-ext-magit-test/is-system-buffer-scratch ()
  "*scratch* is treated as a system buffer."
  (should (tabspaces-ext--is-system-buffer-p "*scratch*")))

(ert-deftest tabspaces-ext-magit-test/is-system-buffer-messages ()
  "*Messages* is treated as a system buffer."
  (should (tabspaces-ext--is-system-buffer-p "*Messages*")))

(ert-deftest tabspaces-ext-magit-test/is-system-buffer-minibuffer ()
  "Minibuffer buffers are treated as system buffers."
  (should (tabspaces-ext--is-system-buffer-p " *Minibuf-0*")))

(ert-deftest tabspaces-ext-magit-test/is-system-buffer-regular-file ()
  "Regular file buffers are not system buffers."
  (should-not (tabspaces-ext--is-system-buffer-p "foo.el")))

(ert-deftest tabspaces-ext-magit-test/is-system-buffer-other-starred ()
  "Non-listed starred buffers are not system buffers."
  (should-not (tabspaces-ext--is-system-buffer-p "*Compile-Log*")))

;;; Tests for tabspaces-ext--add-project-tab-mapping

(ert-deftest tabspaces-ext-magit-test/add-mapping-adds-new ()
  "Adding to an empty map creates the mapping."
  (let ((tabspaces-project-tab-map nil))
    (tabspaces-ext--add-project-tab-mapping "/a/" "proj@main")
    (should (equal tabspaces-project-tab-map '(("/a/" . "proj@main"))))))

(ert-deftest tabspaces-ext-magit-test/add-mapping-no-duplicate-same-name ()
  "Re-adding the same root and name does not duplicate."
  (let ((tabspaces-project-tab-map (list (cons "/a/" "proj@main"))))
    (tabspaces-ext--add-project-tab-mapping "/a/" "proj@main")
    (should (= 1 (length tabspaces-project-tab-map)))
    (should (string= (cdar tabspaces-project-tab-map) "proj@main"))))

(ert-deftest tabspaces-ext-magit-test/add-mapping-updates-existing-root ()
  "Adding an existing root with a new name updates it in place."
  (let ((tabspaces-project-tab-map (list (cons "/a/" "proj@main"))))
    (tabspaces-ext--add-project-tab-mapping "/a/" "proj@feature")
    (should (= 1 (length tabspaces-project-tab-map)))
    (should (string= (cdr (assoc "/a/" tabspaces-project-tab-map))
                     "proj@feature"))))

(ert-deftest tabspaces-ext-magit-test/add-mapping-removes-stale-reverse-dup ()
  "Adding a new root reusing an existing tab name removes the old root."
  (let ((tabspaces-project-tab-map
         (list (cons "/a/" "shared") (cons "/b/" "other"))))
    (tabspaces-ext--add-project-tab-mapping "/c/" "shared")
    (should (= 2 (length tabspaces-project-tab-map)))
    (should (string= (cdr (assoc "/c/" tabspaces-project-tab-map)) "shared"))
    (should-not (assoc "/a/" tabspaces-project-tab-map))
    (should (assoc "/b/" tabspaces-project-tab-map))))

;;; Tests for --get-git-project-name

(ert-deftest tabspaces-ext-magit-test/git-project-name-from-ssh-url ()
  "Extracts project name from an SSH remote URL with .git suffix."
  (cl-letf (((symbol-function 'magit-get)
             (lambda (&rest _) "git@github.com:user/project.git")))
    (should (string= (tabspaces-ext-magit--get-git-project-name) "project"))))

(ert-deftest tabspaces-ext-magit-test/git-project-name-from-ssh-url-no-suffix ()
  "Extracts project name from an SSH remote URL without .git suffix."
  (cl-letf (((symbol-function 'magit-get)
             (lambda (&rest _) "git@github.com:user/project")))
    (should (string= (tabspaces-ext-magit--get-git-project-name) "project"))))

(ert-deftest tabspaces-ext-magit-test/git-project-name-from-https-url ()
  "Extracts project name from an HTTPS remote URL."
  (cl-letf (((symbol-function 'magit-get)
             (lambda (&rest _) "https://github.com/user/project.git")))
    (should (string= (tabspaces-ext-magit--get-git-project-name) "project"))))

(ert-deftest tabspaces-ext-magit-test/git-project-name-falls-back-to-gitdir ()
  "Falls back to the repository directory name when there is no remote."
  (cl-letf (((symbol-function 'magit-get) (lambda (&rest _) nil))
            ((symbol-function 'magit-gitdir) (lambda () "/tmp/myrepo/.git/")))
    (should (string= (tabspaces-ext-magit--get-git-project-name) "myrepo"))))

;;; Tests for --get-git-branch-name

(ert-deftest tabspaces-ext-magit-test/git-branch-name-normal ()
  "Returns the current branch name when magit reports one."
  (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () "main")))
    (should (string= (tabspaces-ext-magit--get-git-branch-name) "main"))))

(ert-deftest tabspaces-ext-magit-test/git-branch-name-from-worktree-dir ()
  "Infers the branch from a worktree directory name when detached."
  (with-temp-project-dir dir
    (let ((wt (file-name-as-directory (expand-file-name "main_feature-x" dir))))
      (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () nil)))
        (should (string= (tabspaces-ext-magit--get-git-branch-name wt)
                         "feature-x"))))))

;;; Tests for tabspaces-ext-magit-default-tab-name

(ert-deftest tabspaces-ext-magit-test/default-tab-name-project-and-branch ()
  "Combines project and branch into project@branch."
  (cl-letf (((symbol-function 'tabspaces-ext-magit--get-git-project-name)
             (lambda () "proj"))
            ((symbol-function 'tabspaces-ext-magit--get-git-branch-name)
             (lambda (&optional _wt) "main")))
    (should (string= (tabspaces-ext-magit-default-tab-name) "proj@main"))))

(ert-deftest tabspaces-ext-magit-test/default-tab-name-project-only ()
  "Falls back to the project name when no branch is available."
  (cl-letf (((symbol-function 'tabspaces-ext-magit--get-git-project-name)
             (lambda () "proj"))
            ((symbol-function 'tabspaces-ext-magit--get-git-branch-name)
             (lambda (&optional _wt) nil)))
    (should (string= (tabspaces-ext-magit-default-tab-name) "proj"))))

(ert-deftest tabspaces-ext-magit-test/default-tab-name-falls-back-to-dir ()
  "Falls back to the directory name when neither project nor branch is known."
  (with-temp-project-dir dir
    (cl-letf (((symbol-function 'tabspaces-ext-magit--get-git-project-name)
               (lambda () nil))
              ((symbol-function 'tabspaces-ext-magit--get-git-branch-name)
               (lambda (&optional _wt) nil)))
      (let ((default-directory dir))
        (should (string= (tabspaces-ext-magit-default-tab-name)
                         (file-name-nondirectory
                          (directory-file-name dir))))))))

(provide 'tabspaces-ext-magit-test)

;;; tabspaces-ext-magit-test.el ends here
