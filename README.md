# tabspaces-ext

Enhanced integrations for [tabspaces](https://github.com/mclear-tools/tabspaces) with modular support for Magit, Treemacs, and Popterm.

## Features

### Core Features
- **Generic integration loader** - Data-driven architecture for easy extensibility
- **Automatic buffer cleanup** - Kills buffers unique to a tab when closing it
- **Early buffer kind registration** - Supports session restoration even when packages aren't loaded yet
- **Enhanced window state handling** - Proper side window save/restore during sessions (via window-state-plus)
- **Common helper functions** - Shared utilities for tab management

### Optional Integrations

Each integration is completely optional and loads only when its customization variable is enabled.

#### Magit Integration (`tabspaces-ext-magit`)
- **Git-aware tab naming** - Tabs automatically named `project@branch`
- **Automatic tabspace creation** - Creates or switches to dedicated tabs for git repositories
- **Worktree-aware** - Each git worktree gets its own tab
- **Smart tab switching** - Visiting worktrees automatically switches to the correct tab
- **Clean cleanup** - Deleting worktrees or closing tabs automatically cleans up buffers
- **Detached HEAD support** - Works during rebases and other detached HEAD states
- **Project mapping persistence** - Maintains project-to-tab mappings across sessions

#### Treemacs Integration (`tabspaces-ext-treemacs`)
Modern treemacs provides per-tab workspaces and tab-switch syncing natively via
`treemacs-tab-bar` (the `Tabs` scope). This module fills the remaining gaps:
- **Correct per-tab project** - Registers a resolver so `treemacs-tab-bar` creates each tab's workspace with the tab's real project (from `tabspaces-project-tab-map`) instead of copying the shared fallback workspace
- **Session restoration** - Restores treemacs buffers when loading saved tabspaces sessions
- **Restore-time reconcile** - After mappings are repaired at restore, resets each mapped tab's workspace to its one project
- **Debug command** - `tabspaces-ext-treemacs-sync-debug` for troubleshooting

#### Popterm Integration (`tabspaces-ext-popterm`)
- **Separate terminal instances per tab** - Each tab has its own popterm buffer
- **Tab-specific buffer naming** - Buffers named `*popterm-backend[project@branch]*`
- **Automatic window sync** - Keeps popterm window state in sync with current tab
- **Layout fixes** - Properly restores popterm position during session restoration
- **Per-tab toggle command** - `tabspaces-ext-popterm-toggle` to toggle terminal for current tab

## Installation

**Important**: `tabspaces-ext` must load **after** `tabspaces`. All installation methods below include `:after tabspaces` to ensure this.

### Using `use-package` with `vc` (Emacs 29+)

```elisp
(use-package tabspaces-ext
  :vc (:url "https://github.com/tagoh/tabspaces-ext.git" :branch "main")
  :after tabspaces
  :custom
  (tabspaces-ext-magit t)      ; Enable Magit integration
  (tabspaces-ext-treemacs t)   ; Enable Treemacs integration
  (tabspaces-ext-popterm t)    ; Enable Popterm integration
  :config
  (tabspaces-ext-mode 1))
```

### Using `use-package` with `straight.el`

```elisp
(use-package tabspaces-ext
  :straight (:host github :repo "tagoh/tabspaces-ext")
  :after tabspaces
  :custom
  (tabspaces-ext-magit t)
  (tabspaces-ext-treemacs t)
  (tabspaces-ext-popterm t)
  :config
  (tabspaces-ext-mode 1))
```

### Using `use-package` with local path

```elisp
(use-package tabspaces-ext
  :load-path "~/path/to/tabspaces-ext"
  :after tabspaces
  :custom
  (tabspaces-ext-magit t)
  (tabspaces-ext-treemacs t)
  (tabspaces-ext-popterm t)
  :config
  (tabspaces-ext-mode 1))
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/tagoh/tabspaces-ext.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/tabspaces-ext")
   
   ;; Load after tabspaces
   (with-eval-after-load 'tabspaces
     (require 'tabspaces-ext)
     (setq tabspaces-ext-magit t
           tabspaces-ext-treemacs t
           tabspaces-ext-popterm t)
     (tabspaces-ext-mode 1))
   ```

## Architecture

### Modular Design

The package uses a **generic, data-driven integration loader**. Integration modules are discovered from `tabspaces-ext--integration-alist`, eliminating hardcoded integration names from the core.

#### File Structure

- **`tabspaces-ext.el`** - Core package with generic loader and common utilities
- **`tabspaces-ext-magit.el`** - Magit/git integration module
- **`tabspaces-ext-treemacs.el`** - Treemacs integration module
- **`tabspaces-ext-popterm.el`** - Popterm integration module
- **`window-state-plus.el`** - Enhanced window state functions

#### Integration Module Contract

Each integration module must provide:

1. **`<module>-register-buffer-kind`** (autoloaded, optional)
   - Called early during initialization
   - No dependencies on the target package
   - Registers buffer kinds for session restoration
   - Example: `tabspaces-ext-treemacs-register-buffer-kind`

2. **`<module>-setup`** (autoloaded, required)
   - Called after the target package loads
   - Can safely require package features
   - Sets up hooks, advice, and keybindings

3. **`<module>-teardown`** (required)
   - Removes all hooks and advice
   - Called when integration is disabled

### How It Works

When `tabspaces-ext-mode` is enabled:

1. **Core functionality activates**:
   - Buffer cleanup hook is installed
   - Common helper functions become available

2. **For each enabled integration**:
   - Calls `<module>-register-buffer-kind` if it exists (for session restoration)
   - Sets up `with-eval-after-load` hook for the target package
   - When package loads → calls `<module>-setup`

This design ensures:
- ✅ Session restoration works even if packages aren't loaded yet
- ✅ No errors from missing packages
- ✅ Integrations activate only when their packages are available
- ✅ Easy to add new integrations (just update the alist)

## Customization

### Magit Integration

#### Custom Tab Naming

Customize how git repository tabs are named:

```elisp
(setq tabspaces-ext-magit-tab-name-function #'my-custom-tab-name)

(defun my-custom-tab-name (&optional worktree-path)
  "Generate custom tab name for git repository."
  (let ((default-directory (or worktree-path default-directory)))
    ;; Your custom naming logic here
    (format "Git: %s" (file-name-nondirectory default-directory))))
```

The default naming function creates names in the format `project@branch`, where:
- `project` is extracted from the git remote URL or directory name
- `branch` is the current git branch name

### Popterm Integration

#### Per-Tab Toggle Keybinding

Set a keybinding for the per-tab popterm toggle:

```elisp
(with-eval-after-load 'popterm
  (global-set-key [f9] #'tabspaces-ext-popterm-toggle))
```

This command toggles the popterm terminal for the current tab, automatically using the tab's project directory.

### Treemacs Integration

#### Debug Sync Issues

If a tab's treemacs workspace shows the wrong project:

```elisp
M-x tabspaces-ext-treemacs-sync-debug
```

This reports the current tab, its mapped project root, and its treemacs
workspace's projects, then runs a reconcile to reset mapped tabs' workspaces to
their one project.

## Using window-state-plus independently

The `window-state-plus` component can be used separately for any session management:

```elisp
(require 'window-state-plus)

;; Use as drop-in replacements for built-in functions
(let ((state (window-state-plus-get (frame-root-window) t)))
  ;; ... do something ...
  (window-state-plus-put state (frame-root-window)))

;; Or add as advice to session save functions
(advice-add 'my-save-session-function :around
            #'window-state-plus-advice-save-session)
```

## Adding New Integrations

To add a new integration:

1. **Create the module file**: `tabspaces-ext-newpackage.el`

2. **Follow the module contract**:
   ```elisp
   ;;;###autoload
   (defun tabspaces-ext-newpackage-register-buffer-kind ()
     "Register buffer kind for session restoration (optional)."
     ;; No dependencies on newpackage
     (tabspaces-register-buffer-kind 'newpackage ...))
   
   ;;;###autoload
   (defun tabspaces-ext-newpackage-setup ()
     "Set up newpackage integration (required)."
     ;; Can require newpackage features here
     (require 'newpackage-subfeature)
     ;; Set up hooks/advice
     ...)
   
   (defun tabspaces-ext-newpackage-teardown ()
     "Tear down newpackage integration (required)."
     ;; Remove hooks/advice
     ...)
   ```

3. **Update the integration alist** in `tabspaces-ext.el`:
   ```elisp
   (defconst tabspaces-ext--integration-alist
     '(...
       (newpackage . ((package . newpackage)
                      (module . tabspaces-ext-newpackage)
                      (variable . tabspaces-ext-newpackage)))))
   ```

4. **Add a customization variable**:
   ```elisp
   (defcustom tabspaces-ext-newpackage nil
     "Enable newpackage integration."
     :type 'boolean
     :group 'tabspaces-ext)
   ```

That's it! The generic loader handles the rest.

## Requirements

- Emacs 27.1 or later
- [tabspaces](https://github.com/mclear-tools/tabspaces) 1.0 or later

### Optional Requirements (per integration)
- [Magit](https://magit.vc/) 3.0.0 or later (for `tabspaces-ext-magit`)
- [Treemacs](https://github.com/Alexander-Miller/treemacs) 3.0 or later (for `tabspaces-ext-treemacs`)
- [Popterm](https://github.com/CarlQLange/popterm) 0.1 or later (for `tabspaces-ext-popterm`)

## Troubleshooting

### "Cannot open load file: tabspaces"

Make sure `tabspaces-ext` loads **after** `tabspaces`:

```elisp
(use-package tabspaces-ext
  :after tabspaces  ; <-- This is required!
  ...)
```

### "unknown buffer kinds in session"

This means buffer kind registration didn't happen before session restoration. Ensure:
1. You're using the latest version of tabspaces-ext
2. The integration module has a `register-buffer-kind` function
3. That function is being called (check with `M-x describe-function`)

### Session restoration creates buffers but wrong directory

This is expected during the transition. The buffer kind registration restores buffers, but full integration (directory sync, etc.) only happens after the package loads. This is normal and temporary.

## License

GPL-3.0-or-later

## Author

Akira TAGOH <akira@tagoh.org>

## Acknowledgments

This package was created with the assistance of Claude (Anthropic).
