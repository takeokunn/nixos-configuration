---
name: emacs-ecosystem
description: "Use when working with Emacs Lisp, 'init.el', 'use-package', '.el files', 'emacs config', 'magit', 'eglot', or 'lsp-mode'. Provides Emacs ecosystem patterns for elisp fundamentals, configuration management, package systems, Magit, and LSP integration. For org-mode, use org-ecosystem skill."
---

Comprehensive patterns for Emacs Lisp development, configuration management, package systems (package.el, straight.el, elpaca), Magit, LSP integration, and modern completion frameworks. For org-mode patterns, see the org-ecosystem skill.

## Elisp Fundamentals

### Data Types and Syntax

S-expressions with prefix notation. Core types: symbol, cons cell, list, vector, hash-table, string, number.

```elisp
;; Function definition
(defun my-function (arg1 arg2)
  "Docstring describing the function."
  (+ arg1 arg2))

;; Local bindings
(let ((x 1) (y 2))
  (+ x y))

;; Conditionals: if, when, unless, cond, pcase
(pcase value
  ('symbol (handle-symbol))
  ((pred stringp) (handle-string))
  (_ (handle-default)))
```

### Iteration

```elisp
(dolist (item list) (process item))
(dotimes (i 10) (process i))
(cl-loop for item in list collect (transform item))
(seq-map #'transform sequence)
```

### Macros

```elisp
(defmacro with-temp-message (msg &rest body)
  "Execute BODY with MSG displayed temporarily."
  `(let ((message-log-max nil))
     (message "%s" ,msg)
     (unwind-protect
         (progn ,@body)
       (message nil))))
```

## Configuration Patterns

### init.el Structure

```elisp
;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(provide 'init)
;;; init.el ends here
```

### use-package

Use for declarative, lazy-loaded package configuration:

```elisp
(use-package company
  :ensure t
  :defer t
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  :config
  (setq company-backends '(company-capf)))
```

Key keywords: `:ensure` (install), `:defer` (lazy load), `:hook`, `:bind`, `:custom` (defcustom vars), `:init` (before load), `:config` (after load), `:commands` (autoload), `:after`, `:if`/`:when`/`:unless`.

### Keybindings

```elisp
(global-set-key (kbd "C-c l") #'org-store-link)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'eval-last-sexp)

;; Keymap prefix
(defvar my-prefix-map (make-sparse-keymap))
(global-set-key (kbd "C-c m") my-prefix-map)
(define-key my-prefix-map (kbd "f") #'find-file)
```

### Hooks and Advice

```elisp
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Advice: modify existing functions
(defun my-after-save-message (orig-fun &rest args)
  (apply orig-fun args)
  (message "Buffer saved at %s" (current-time-string)))
(advice-add 'save-buffer :around #'my-after-save-message)
```

### Custom Variables

```elisp
(defgroup my-package nil
  "My package customization."
  :group 'convenience
  :prefix "my-package-")

(defcustom my-package-option t
  "Enable my-package option."
  :type 'boolean
  :group 'my-package)
```

## Package Managers

- **package.el**: Built-in, simple (`package-install`, `package-refresh-contents`)
- **straight.el**: Functional, Git-based, reproducible (`straight-use-package 'magit`)
- **elpaca**: Modern async package manager

## Magit (Git Porcelain)

```elisp
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch)))

(setq magit-save-repository-buffers 'dontask)
(setq magit-display-buffer-function
      #'magit-display-buffer-same-window-except-diff-v1)
```

Status buffer keys: `s` (stage), `u` (unstage), `c c` (commit), `P p` (push), `F p` (pull), `b b` (checkout), `l l` (log), `d d` (diff).

## LSP Integration

### eglot (Built-in, Emacs 29+)

```elisp
(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t))
```

### lsp-mode (Feature-rich)

```elisp
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp-deferred)
         (typescript-mode . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.5)
  :config
  (lsp-enable-which-key-integration t))
```

## Modern Completion Stack

```elisp
(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless basic)))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)))

(use-package corfu
  :ensure t
  :custom (corfu-auto t)
  :init (global-corfu-mode))
```

## Tree-sitter (Emacs 29+)

```elisp
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (javascript-mode . js-ts-mode)))
```

## Anti-Patterns to Avoid

- **Dynamic binding** -- add `lexical-binding: t` to file header
- **Hardcoded paths** -- use `expand-file-name`, `user-emacs-directory`
- **Unconditional require** -- use autoload, `use-package` with `:defer`
- **Lambdas in hooks** -- define named functions (easier to remove)
- **setq for defcustom** -- use `:custom` in use-package or `customize-set-variable`
- **Deprecated cl library** -- use `cl-lib` with `cl-` prefixed functions
- **Complex early-init.el** -- keep minimal (frame settings, package setup)

## Workflow

1. **Analyze**: Check package dependencies and autoloads, review config patterns, identify hook/advice usage
2. **Implement**: Use lexical binding, follow elisp conventions, provide defcustom options
3. **Validate**: Byte-compile without warnings, test in clean Emacs, verify keybinding conflicts

## Best Practices

- Enable `lexical-binding: t` in all elisp files
- Use `#'function-name` for function references (enables byte-compiler warnings)
- Document functions with docstrings, namespace symbols with package prefix
- Prefer `:custom` over `setq` in use-package, `:hook` over `add-hook`
- Lazy load with `:defer`, `:commands`, or `:hook`
- Use native compilation (Emacs 28+) and tree-sitter modes (Emacs 29+)
- Prefer eglot for LSP (built-in, simpler)
- End files with `(provide 'module-name)`
