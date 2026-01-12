---
name: Emacs Ecosystem
description: This skill should be used when the user asks to "write elisp", "emacs config", "init.el", "use-package", ".el file", "emacs lisp", or "magit". Provides comprehensive Emacs ecosystem patterns and best practices. For org-mode, use org-ecosystem skill.
---

<purpose>
  Provide comprehensive patterns for Emacs Lisp, configuration management, package systems, and major packages including Magit and LSP integration. For org-mode patterns, see org-ecosystem skill.
</purpose>

<elisp_fundamentals>
  <concept name="basic_syntax">
    <description>S-expressions as code and data (homoiconicity). Prefix notation for all operations.</description>
  </concept>

  <concept name="data_types">
    <description>Emacs Lisp data types: symbol, cons cell, list, vector, hash-table, string, number</description>
    <example>
      ;; symbol: Named objects
      'foo
      :keyword

      ;; cons_cell: Pair
      (cons 1 2) ; => (1 . 2)

      ;; list: Linked cons cells
      '(1 2 3)

      ;; vector: Fixed-size array
      [1 2 3]

      ;; hash-table: Key-value store
      (make-hash-table)

      ;; string: Text
      "hello"

      ;; number: Integer or float
      42
      3.14
    </example>
  </concept>

  <pattern name="defun">
    <description>Define functions with defun</description>
    <example>
      (defun my-function (arg1 arg2)
        "Docstring describing the function."
        (+ arg1 arg2))
    </example>
  </pattern>

  <pattern name="let_binding">
    <description>Local variable binding with let and let*</description>
    <example>
      (let ((x 1)
            (y 2))
        (+ x y))

      (let\* ((x 1)
              (y (+ x 1))) ; y can reference x
        y)
    </example>
  </pattern>

  <pattern name="conditionals">
    <description>Conditional forms: if, when, unless, cond, pcase</description>
    <example>
      (if condition
          then-form
        else-form)

      (when condition
        body-forms...)

      (unless condition
        body-forms...)

      (cond
        (condition1 result1)
        (condition2 result2)
        (t default-result))

      (pcase value
        ('symbol (handle-symbol))
        ((pred stringp) (handle-string))
        (\_ (handle-default)))
    </example>
  </pattern>

  <pattern name="iteration">
    <description>Iteration patterns: dolist, dotimes, cl-loop, seq functions</description>
    <example>
      (dolist (item list)
        (process item))

      (dotimes (i 10)
        (process i))

      (cl-loop for item in list
               collect (transform item))

      (seq-map #'transform sequence)
      (seq-filter #'predicate sequence)
      (seq-reduce #'fn sequence initial)
    </example>
  </pattern>

  <pattern name="lambda">
    <description>Anonymous functions with lambda</description>
    <example>
      (lambda (x) (* x 2))

      (mapcar (lambda (x) (\* x 2)) '(1 2 3))

      ;; Short form (Emacs 28+)
      (mapcar (lambda (x) (+ x 1)) list)
    </example>
  </pattern>

  <pattern name="macros">
    <description>Define macros with defmacro. Use backquote for templates, comma for evaluation</description>
    <example>
      (defmacro with-temp-message (msg &amp;rest body)
        "Execute BODY with MSG displayed temporarily."
        `(let ((message-log-max nil))
           (message "%s" ,msg)
           (unwind-protect
               (progn ,@body)
             (message nil))))
    </example>
  </pattern>
</elisp_fundamentals>

<configuration_patterns>
  <pattern name="init_el_structure">
    <description>Modern init.el organization</description>
    <example>
      ;;; init.el --- Emacs configuration -\*- lexical-binding: t; -\_-

      ;;; Commentary:
      ;; Personal Emacs configuration

      ;;; Code:

      ;; Bootstrap package manager
      (require 'package)
      (setq package-archives
            '(("melpa" . "https://melpa.org/packages/")
              ("gnu" . "https://elpa.gnu.org/packages/")
              ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
      (package-initialize)

      ;; Install use-package if not present
      (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

      (eval-when-compile
        (require 'use-package))

      ;; Configuration sections...

      (provide 'init)
      ;;; init.el ends here
    </example>
  </pattern>

  <pattern name="use_package">
    <description>Declarative package configuration with use-package keywords</description>
    <decision_tree name="when_to_use">
      <question>Does the package need lazy loading or declarative configuration?</question>
      <if_yes>Use use-package for clean, maintainable configuration</if_yes>
      <if_no>Use require for simple packages with no configuration needs</if_no>
    </decision_tree>
    <example>
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
    </example>
    <note>
      Keywords:
      - :ensure - Install package if not present
      - :defer - Lazy load (t or seconds)
      - :hook - Add to mode hooks
      - :bind - Define keybindings
      - :custom - Set customizable variables
      - :init - Run before package loads
      - :config - Run after package loads
      - :commands - Autoload commands
      - :after - Load after specified packages
      - :if/:when/:unless - Conditional loading
    </note>
  </pattern>

  <pattern name="keybinding">
    <description>Key binding patterns: global-set-key, define-key, use-package :bind</description>
    <example>
      ;; Global keybinding
      (global-set-key (kbd "C-c l") #'org-store-link)

      ;; Mode-specific
      (define-key emacs-lisp-mode-map (kbd "C-c C-e") #'eval-last-sexp)

      ;; With use-package
      (use-package magit
        :bind (("C-x g" . magit-status)
               ("C-x M-g" . magit-dispatch)))

      ;; Keymap definition
      (defvar my-prefix-map (make-sparse-keymap)
        "Keymap for my custom commands.")
      (global-set-key (kbd "C-c m") my-prefix-map)
      (define-key my-prefix-map (kbd "f") #'find-file)
    </example>
  </pattern>

  <pattern name="hooks">
    <description>Hook management with add-hook and use-package :hook</description>
    <example>
      ;; Add function to hook
      (add-hook 'prog-mode-hook #'display-line-numbers-mode)

      ;; Remove function from hook
      (remove-hook 'prog-mode-hook #'display-line-numbers-mode)

      ;; Lambda in hook (discouraged for removability)
      (add-hook 'after-save-hook
                (lambda () (message "Saved!")))

      ;; With use-package
      (use-package flycheck
        :hook (prog-mode . flycheck-mode))
    </example>
  </pattern>

  <pattern name="advice">
    <description>Modify existing functions with advice-add and advice-remove</description>
    <example>
      (defun my-after-save-message (orig-fun &amp;rest args)
        "Show message after save."
        (apply orig-fun args)
        (message "Buffer saved at %s" (current-time-string)))

      (advice-add 'save-buffer :around #'my-after-save-message)

      ;; Remove advice
      (advice-remove 'save-buffer #'my-after-save-message)
    </example>
  </pattern>

  <pattern name="custom_variables">
    <description>Define customizable variables with defgroup and defcustom</description>
    <example>
      (defgroup my-package nil
        "My package customization."
        :group 'convenience
        :prefix "my-package-")

      (defcustom my-package-option t
        "Enable my-package option."
        :type 'boolean
        :group 'my-package)

      (defcustom my-package-list '("a" "b")
        "List of strings."
        :type '(repeat string)
        :group 'my-package)
    </example>
  </pattern>
</configuration_patterns>

<tools>
  <tool name="package.el">
    <description>Built-in package manager for Emacs</description>
    <example>
      ;; Commands:
      ;; - package-install - Install a package
      ;; - package-delete - Remove a package
      ;; - package-refresh-contents - Update package list
      ;; - package-list-packages - Browse packages

      (require 'package)
      (setq package-archives
            '(("melpa" . "https://melpa.org/packages/")
              ("gnu" . "https://elpa.gnu.org/packages/")))
      (package-initialize)

      ;; Install a package
      (package-install 'magit)
    </example>
  </tool>

  <tool name="straight.el">
    <description>Functional package manager with Git integration</description>
    <example>
      ;; Bootstrap
      (defvar bootstrap-version)
      (let ((bootstrap-file
             (expand-file-name "straight/repos/straight.el/bootstrap.el"
                               user-emacs-directory)))
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el")
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage))

      ;; Use with use-package
      (straight-use-package 'use-package)
      (setq straight-use-package-by-default t)

      ;; Install package
      (use-package magit
        :straight t)
    </example>
  </tool>

  <tool name="elpaca">
    <description>Modern async package manager</description>
    <example>
      ;; Bootstrap
      (defvar elpaca-installer-version 0.7)
      (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
      ;; ... (bootstrap code)

      ;; Use with use-package
      (elpaca elpaca-use-package
        (elpaca-use-package-mode))

      (use-package magit
        :ensure t)
    </example>
  </tool>
</tools>

<magit>
  <description>Git porcelain for Emacs</description>

  <pattern name="basic_usage">
    <description>Basic Magit setup with use-package</description>
    <example>
      (use-package magit
        :ensure t
        :bind (("C-x g" . magit-status)
               ("C-x M-g" . magit-dispatch)
               ("C-c M-g" . magit-file-dispatch)))
    </example>
  </pattern>

  <pattern name="status_buffer">
    <description>Magit status buffer keybindings</description>
    <example>
      ;; s - Stage file/hunk
      ;; u - Unstage file/hunk
      ;; c c - Commit
      ;; P p - Push
      ;; F p - Pull
      ;; b b - Checkout branch
      ;; b c - Create branch
      ;; l l - Log current branch
      ;; d d - Diff
    </example>
  </pattern>

  <pattern name="configuration">
    <description>Magit configuration settings</description>
    <example>
      (setq magit-save-repository-buffers 'dontask)
      (setq magit-display-buffer-function
            #'magit-display-buffer-same-window-except-diff-v1)
      (setq magit-diff-refine-hunk 'all)
    </example>
  </pattern>

  <pattern name="forge">
    <description>GitHub/GitLab integration with Forge</description>
    <example>
      (use-package forge
        :after magit
        :ensure t)
    </example>
  </pattern>
</magit>

<lsp_integration>
  <decision_tree name="when_to_use">
    <question>Do you need LSP features like completion, go-to-definition, and diagnostics?</question>
    <if_yes>Use eglot for built-in simplicity or lsp-mode for rich features</if_yes>
    <if_no>Use basic major modes without LSP overhead</if_no>
  </decision_tree>

  <pattern name="eglot">
    <description>Built-in LSP client (Emacs 29+)</description>
    <example>
      (use-package eglot
        :ensure nil ; built-in
        :hook ((python-mode . eglot-ensure)
               (typescript-mode . eglot-ensure)
               (rust-mode . eglot-ensure))
        :config
        (setq eglot-autoshutdown t)
        (setq eglot-events-buffer-size 0))

      ;; Custom server configuration
      (add-to-list 'eglot-server-programs
                   '(rust-mode . ("rust-analyzer")))
    </example>
  </pattern>

  <pattern name="lsp_mode">
    <description>Feature-rich LSP client with lsp-mode and lsp-ui</description>
    <example>
      (use-package lsp-mode
        :ensure t
        :hook ((python-mode . lsp-deferred)
               (typescript-mode . lsp-deferred))
        :commands (lsp lsp-deferred)
        :custom
        (lsp-keymap-prefix "C-c l")
        (lsp-idle-delay 0.5)
        (lsp-log-io nil)
        :config
        (lsp-enable-which-key-integration t))

      (use-package lsp-ui
        :ensure t
        :hook (lsp-mode . lsp-ui-mode)
        :custom
        (lsp-ui-doc-enable t)
        (lsp-ui-sideline-enable t))
    </example>
  </pattern>

  <pattern name="completion">
    <description>LSP completion with corfu/company</description>
    <example>
      ;; With corfu (modern)
      (use-package corfu
        :ensure t
        :custom
        (corfu-auto t)
        (corfu-cycle t)
        :init
        (global-corfu-mode))

      ;; With company (traditional)
      (use-package company
        :ensure t
        :hook (after-init . global-company-mode)
        :custom
        (company-idle-delay 0.2))
    </example>
  </pattern>
</lsp_integration>

<modern_packages>
  <tool name="vertico">
    <description>Vertical completion UI with orderless, marginalia, and consult</description>
    <example>
      (use-package vertico
        :ensure t
        :init (vertico-mode))

      (use-package orderless
        :ensure t
        :custom
        (completion-styles '(orderless basic)))

      (use-package marginalia
        :ensure t
        :init (marginalia-mode))

      (use-package consult
        :ensure t
        :bind (("C-s" . consult-line)
               ("C-x b" . consult-buffer)
               ("M-g g" . consult-goto-line)))
    </example>
  </tool>

  <tool name="which_key">
    <description>Display available keybindings</description>
    <example>
      (use-package which-key
        :ensure t
        :diminish
        :init (which-key-mode))
    </example>
  </tool>

  <tool name="treesit">
    <description>Tree-sitter integration (Emacs 29+)</description>
    <example>
      (setq treesit-language-source-alist
            '((python "https://github.com/tree-sitter/tree-sitter-python")
              (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
              (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                          "master" "typescript/src")))

      ;; Install grammars
      (mapc #'treesit-install-language-grammar
            (mapcar #'car treesit-language-source-alist))

      ;; Remap modes
      (setq major-mode-remap-alist
            '((python-mode . python-ts-mode)
              (javascript-mode . js-ts-mode)))
    </example>
  </tool>
</modern_packages>

<context7_integration>
  <library name="Emacs Docs" id="/websites/emacsdocs" trust="7.5" snippets="6792" />

  <usage_pattern>
    <step>Resolve library ID (known: /websites/emacsdocs)</step>
    <step>Fetch documentation with specific topic</step>
    <examples>
      <example topic="elisp">Emacs Lisp programming patterns</example>
      <example topic="use-package">Package configuration patterns</example>
      <example topic="org-mode">Org mode configuration</example>
      <example topic="magit">Magit usage and configuration</example>
      <example topic="hooks">Hook usage patterns</example>
    </examples>
  </usage_pattern>

  <common_queries>
    <query topic="keybindings">Key binding patterns</query>
    <query topic="defun">Function definition</query>
    <query topic="advice">Advice system usage</query>
    <query topic="custom">Customization variables</query>
  </common_queries>
</context7_integration>

<best_practices>
  <practice priority="critical">Enable lexical-binding in all Elisp files: -\*- lexical-binding: t; -\_-</practice>
  <practice priority="high">Use #'function-name for function references (enables byte-compiler warnings)</practice>
  <practice priority="high">Document functions with docstrings</practice>
  <practice priority="high">Namespace all symbols with package prefix</practice>
  <practice priority="medium">Prefer seq.el functions for sequence operations</practice>
  <practice priority="medium">Use pcase for complex pattern matching</practice>
  <practice priority="medium">Use defcustom for user-configurable options</practice>
  <practice priority="medium">Use provide at end of file</practice>
  <practice priority="medium">Prefer :custom over setq in use-package</practice>
  <practice priority="medium">Use :hook instead of add-hook in use-package</practice>
  <practice priority="medium">Lazy load packages with :defer, :commands, or :hook</practice>
  <practice priority="medium">Use native-compilation when available (Emacs 28+)</practice>
  <practice priority="medium">Prefer eglot for LSP (built-in, simpler)</practice>
  <practice priority="medium">Use tree-sitter modes when available (Emacs 29+)</practice>
</best_practices>

<anti_patterns>
  <avoid name="dynamic_binding">
    <description>Using dynamic binding when lexical is needed</description>
    <instead>Add lexical-binding: t to file header</instead>
  </avoid>

  <avoid name="hardcoded_paths">
    <description>Hardcoding absolute paths</description>
    <instead>Use expand-file-name, user-emacs-directory, or locate-user-emacs-file</instead>
  </avoid>

  <avoid name="require_at_top">
    <description>Requiring packages at top level unconditionally</description>
    <instead>Use autoload, use-package with :defer, or eval-after-load</instead>
  </avoid>

  <avoid name="global_state">
    <description>Modifying global state without restoration</description>
    <instead>Use let-binding or save-excursion/save-restriction</instead>
  </avoid>

  <avoid name="lambda_in_hooks">
    <description>Adding lambdas to hooks (hard to remove)</description>
    <instead>Define named functions and add those</instead>
  </avoid>

  <avoid name="setq_for_custom">
    <description>Using setq for defcustom variables</description>
    <instead>Use customize-set-variable or :custom in use-package</instead>
  </avoid>

  <avoid name="cl_library">
    <description>Using deprecated cl library</description>
    <instead>Use cl-lib with cl- prefixed functions</instead>
  </avoid>

  <avoid name="eval_after_load_string">
    <description>Using eval-after-load with string</description>
    <instead>Use with-eval-after-load or use-package :config</instead>
  </avoid>

  <avoid name="inhibit_startup">
    <description>Complex logic in early-init.el</description>
    <instead>Keep early-init.el minimal (frame settings, package setup)</instead>
  </avoid>
</anti_patterns>

<workflow>
  <phase name="analyze">
    <objective>Understand Emacs Lisp requirements</objective>
    <step>1. Check package dependencies and autoloads</step>
    <step>2. Review existing configuration patterns</step>
    <step>3. Identify hook and advice usage</step>
  </phase>
  <phase name="implement">
    <objective>Write idiomatic Emacs Lisp code</objective>
    <step>1. Use lexical binding</step>
    <step>2. Follow Emacs Lisp conventions</step>
    <step>3. Provide appropriate customization options</step>
  </phase>
  <phase name="validate">
    <objective>Verify Emacs Lisp correctness</objective>
    <step>1. Byte-compile without warnings</step>
    <step>2. Test in clean Emacs instance</step>
    <step>3. Verify keybindings don't conflict</step>
  </phase>
</workflow>

<related_agents>
  <agent name="design">Architecture analysis for elisp package structure</agent>
  <agent name="docs">Docstring and commentary generation</agent>
  <agent name="execute">Elisp implementation and configuration tasks</agent>
  <agent name="bug">Debugging elisp errors and hook issues</agent>
</related_agents>

<related_skills>
  <skill name="org-ecosystem">Org-mode document creation, GTD workflow, Babel, export patterns</skill>
  <skill name="serena-usage">Symbol operations for elisp code navigation</skill>
  <skill name="context7-usage">Emacs documentation lookup via /websites/emacsdocs</skill>
  <skill name="investigation-patterns">Debugging package conflicts and performance issues</skill>
  <skill name="technical-documentation">Creating package documentation and README files</skill>
</related_skills>

<error_escalation>
  <level severity="low">
    <example>Byte-compilation warning</example>
    <action>Fix warning, ensure clean compilation</action>
  </level>
  <level severity="medium">
    <example>Configuration error on startup</example>
    <action>Debug with --debug-init, fix issue</action>
  </level>
  <level severity="high">
    <example>Package conflict or version mismatch</example>
    <action>Stop, present resolution options to user</action>
  </level>
  <level severity="critical">
    <example>Emacs becomes unusable</example>
    <action>Provide recovery steps, require user action</action>
  </level>
</error_escalation>

<constraints>
  <must>Use lexical-binding: t in all files</must>
  <must>Provide customization via defcustom</must>
  <must>Follow Emacs Lisp naming conventions</must>
  <avoid>Dynamic binding without justification</avoid>
  <avoid>Overriding standard keybindings silently</avoid>
  <avoid>Blocking operations in hooks</avoid>
</constraints>
