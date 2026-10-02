;;; tabspaces-ext-treemacs.el --- Treemacs integration for tabspaces-ext -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira TAGOH

;; Author: Akira TAGOH <akira@tagoh.org>
;; URL: https://github.com/tagoh/tabspaces-ext
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (treemacs "3.0") (tabspaces "1.0") (tabspaces-ext "1.0"))
;; Keywords: convenience, frames

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

;; Treemacs integration module for tabspaces-ext.
;;
;; Modern treemacs provides per-tab workspaces natively via `treemacs-tab-bar'
;; (the `Tabs' scope): it creates and selects a treemacs workspace for each tab
;; on every tab switch and new tab.  This module no longer re-implements that.
;; It only fills the gaps treemacs-tab-bar leaves:
;;
;; - `tabspaces-ext-treemacs--user-project-function'
;;   Registered in `treemacs--find-user-project-functions' so that when
;;   treemacs-tab-bar creates a tab's workspace it resolves the tab's *real*
;;   project (from `tabspaces-project-tab-map') instead of copying the shared
;;   fallback workspace.  This is what keeps a new tab showing exactly its own
;;   project rather than a pile of unrelated ones.
;;
;; - `tabspaces-ext-treemacs-register-buffer-kind' (autoloaded)
;;   Bridges treemacs into tabspaces' session save/restore (treemacs' own
;;   persistence is separate from tabspaces sessions).  Called early, before
;;   treemacs loads, with `featurep' guards.
;;
;; - `tabspaces-ext-treemacs--reconcile'
;;   A restore/cleanup corrective.  At session restore the mapping may not be
;;   ready when treemacs-tab-bar first creates a tab's workspace, so it copies
;;   the fallback.  Once mappings are repaired (the magit integration calls this
;;   afterward), reconcile resets each mapped tab's workspace to its single
;;   project and lets treemacs redraw via `treemacs--consolidate-projects'.
;;
;; Setup/teardown just register and unregister the resolver.
;;
;; Troubleshooting:
;;
;;   M-x tabspaces-ext-treemacs-sync-debug
;;
;; reports the current tab, its mapped root, and its treemacs workspace
;; projects, then runs a reconcile.
;;
;; Acknowledgments:
;;   This module was created with the assistance of Claude (Anthropic).

;;; Code:

(require 'tabspaces)
(require 'tabspaces-ext)
(require 'cl-lib)                        ; for `cl-struct-slot-offset'

;; Don't require treemacs at load time - it is loaded via `with-eval-after-load'.
;; Pull it in only when byte-compiling so its functions are known to the compiler.
(eval-when-compile (require 'treemacs nil t))

;; Treemacs' list of per-buffer project resolvers.  `treemacs-tab-bar' consults
;; it (via `treemacs--find-current-user-project') when creating a tab's
;; workspace; we prepend our mapping-based resolver so a new tab gets its real
;; project instead of a copy of the shared fallback workspace.  Declared special
;; here so setup can `add-to-list' before treemacs has necessarily defined it.
(defvar treemacs--find-user-project-functions)

;;; Session restoration

;;;###autoload
(defun tabspaces-ext-treemacs-register-buffer-kind ()
  "Register treemacs buffer kind for session restoration.
This function is called early during tabspaces-ext initialization,
before treemacs is loaded, to support session restoration."
  (tabspaces-register-buffer-kind
   'treemacs
   (lambda (b)
     (when (eq (buffer-local-value 'major-mode b) 'treemacs-mode)
       (list :kind 'treemacs :dir (buffer-local-value 'default-directory b))))
   (lambda (rec)
     (when-let* ((dir (plist-get rec :dir)))
       (or (tabspaces-reuse-existing-buffer " *Treemacs-Buffer-No Tab")
           (when (featurep 'treemacs)
             (save-window-excursion
               (let ((default-directory dir))
                 (ignore-errors (treemacs) (current-buffer))))))))))

;;; Per-tab project resolution

(defun tabspaces-ext-treemacs--user-project-function ()
  "Resolve the current tab's mapped project root for treemacs, or nil.
Registered in `treemacs--find-user-project-functions' so that -- under Tabs
scope (`treemacs-tab-bar') -- creating a new tab's workspace resolves to the
tab's real project via `tabspaces-project-tab-map', instead of copying the
shared fallback workspace.  That copy is what otherwise leaves a new tab showing
unrelated projects, or -- when the fallback is empty -- makes `treemacs--init'
prompt \"Project root:\".  Returns nil for tabs with no mapped, existing root so
treemacs' built-in resolvers still apply (e.g. the `Default' tab)."
  (when-let* ((tab-name (tabspaces-ext--get-current-tab-name))
              (root (tabspaces-ext--tab-project-root tab-name)))
    (when (file-directory-p root)
      (if (fboundp 'treemacs-canonical-path)
          (treemacs-canonical-path (file-truename root))
        (expand-file-name root)))))

;;; Restore/cleanup reconcile

(defun tabspaces-ext-treemacs--needs-reconcile-p (current-paths canonical-root)
  "Return non-nil unless CURRENT-PATHS is exactly (CANONICAL-ROOT).
CURRENT-PATHS is the list of project paths in a tab's treemacs workspace;
CANONICAL-ROOT is the tab's mapped root, canonicalized."
  (not (equal current-paths (list canonical-root))))

(defun tabspaces-ext-treemacs--reconcile (&rest _)
  "Reset each mapped tab's treemacs workspace to show its single mapped project.
`treemacs-tab-bar' creates one workspace per tab, but at session restore a
`tabspaces-project-tab-map' entry may not exist yet when the workspace is first
created -- treemacs then copies its fallback workspace, so the tab shows the
wrong (or no) project.  For every mapping whose \"Tab <name>\" workspace already
exists and whose root still exists on disk, replace that workspace's project
list with a single project for the mapped root, then let treemacs redraw all
buffers via `treemacs--consolidate-projects'.  Return non-nil if anything
changed.

Editing the workspace's project list and delegating the redraw is deliberate:
`treemacs--consolidate-projects' re-renders each buffer from its *own*
workspace, so it never dereferences a project that is absent from a buffer's
buffer-local dom -- the failure mode that made the old per-tab add/remove sync
crash with `(wrong-type-argument arrayp nil)'."
  (when (and (featurep 'treemacs)
             (fboundp 'treemacs--find-workspace-by-name)
             (bound-and-true-p tabspaces-project-tab-map))
    (let ((changed nil))
      (dolist (entry tabspaces-project-tab-map)
        (let* ((root (car entry))
               (tab-name (cdr entry))
               (ws (treemacs--find-workspace-by-name (format "Tab %s" tab-name))))
          (when (and ws (stringp root) (file-directory-p root))
            (let* ((croot (treemacs-canonical-path (file-truename root)))
                   (current (mapcar #'treemacs-project->path
                                    (treemacs-workspace->projects ws))))
              (when (tabspaces-ext-treemacs--needs-reconcile-p current croot)
                ;; Set the workspace's `projects' slot by offset rather than with
                ;; `setf': this file is loaded before treemacs, and `load'
                ;; eager-macroexpands defun bodies, so a `setf' on the struct
                ;; accessor would bake a broken `(setf treemacs-workspace->...)'
                ;; call before treemacs has defined the generalized variable.
                ;; `aset' + `cl-struct-slot-offset' are plain runtime calls.
                (aset ws (cl-struct-slot-offset 'treemacs-workspace 'projects)
                      (list (treemacs-project->create!
                             :name (file-name-nondirectory (directory-file-name croot))
                             :path croot
                             :path-status (treemacs--get-path-status croot))))
                (setq changed t))))))
      (when changed
        (when (fboundp 'treemacs--consolidate-projects)
          (treemacs--consolidate-projects))
        (when (fboundp 'treemacs--persist)
          (treemacs--persist)))
      changed)))

;;; Debug function

;;;###autoload
(defun tabspaces-ext-treemacs-sync-debug ()
  "Show treemacs/tabspaces diagnostic info, then reconcile mapped workspaces."
  (interactive)
  (let* ((tab (tabspaces-ext--get-current-tab-name))
         (root (and tab (tabspaces-ext--tab-project-root tab)))
         (ws (and tab (fboundp 'treemacs--find-workspace-by-name)
                  (treemacs--find-workspace-by-name (format "Tab %s" tab))))
         (projects (and ws (mapcar #'treemacs-project->path
                                   (treemacs-workspace->projects ws)))))
    (message "Tab: %s | Mapped root: %s | Workspace projects: %s"
             tab root projects)
    (tabspaces-ext-treemacs--reconcile)))

;;; Setup/teardown functions

(defvar tabspaces-ext-treemacs--active nil
  "Track whether treemacs integration is active.")

;;;###autoload
(defun tabspaces-ext-treemacs-setup ()
  "Set up Treemacs integration for tabspaces.
Registers `tabspaces-ext-treemacs--user-project-function' so `treemacs-tab-bar'
creates each tab's workspace with the tab's mapped project.  Per-tab workspace
switching is handled natively by treemacs-tab-bar; session restore is handled by
`tabspaces-ext-treemacs-register-buffer-kind' and
`tabspaces-ext-treemacs--reconcile' (the latter called by the magit integration
after mapping repair)."
  (unless tabspaces-ext-treemacs--active
    (when (boundp 'treemacs--find-user-project-functions)
      (add-to-list 'treemacs--find-user-project-functions
                   #'tabspaces-ext-treemacs--user-project-function))
    (setq tabspaces-ext-treemacs--active t)))

;;;###autoload
(defun tabspaces-ext-treemacs-teardown ()
  "Tear down Treemacs integration for tabspaces."
  (when tabspaces-ext-treemacs--active
    (when (boundp 'treemacs--find-user-project-functions)
      (setq treemacs--find-user-project-functions
            (delq #'tabspaces-ext-treemacs--user-project-function
                  treemacs--find-user-project-functions)))
    (setq tabspaces-ext-treemacs--active nil)))

(provide 'tabspaces-ext-treemacs)

;;; tabspaces-ext-treemacs.el ends here
