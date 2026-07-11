---
name: Emacs Ecosystem
description: This skill should be used when the user asks to "write elisp", "emacs config", "init.el", "use-package", ".el file", "emacs lisp", or "magit". Provides comprehensive Emacs ecosystem patterns and best practices. For org-mode, use org-ecosystem skill.
version: 2.1.0
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

      (let* ((x 1)
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
        (_ (handle-default)))
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

      (mapcar (lambda (x) (* x 2)) '(1 2 3))

      ;; lambda self-quotes — #' is optional when passing it directly
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

<patterns>
  <pattern name="init_el_structure">
    <description>Modern init.el organization</description>
    <example>
      ;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

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

      ;; use-package is built-in since Emacs 29; no installation needed
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
      (use-package corfu
        :ensure t
        :defer t
        :hook (prog-mode . corfu-mode)
        :bind (:map corfu-map
                    ("C-n" . corfu-next)
                    ("C-p" . corfu-previous))
        :custom
        (corfu-auto t)
        (corfu-cycle t))
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
</patterns>

<tools>
  <tool name="package.el">
    <description>Built-in package manager for Emacs. Reliable and sufficient for most workflows.</description>
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

  <tool name="use-package">
    <description>Built-in since Emacs 29. The standard declarative way to configure packages. No installation needed on Emacs 29+.</description>
    <example>
      ;; use-package is built-in since Emacs 29; just require it
      (eval-when-compile
        (require 'use-package))

      ;; Declarative package configuration
      (use-package magit
        :ensure t
        :bind ("C-x g" . magit-status))
    </example>
  </tool>

  <tool name="straight.el">
    <description>Functional package manager with Git integration. Still widely used, but elpaca is gaining adoption for reproducible package management.</description>
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
    <description>Modern async package manager gaining adoption for reproducible package management. An alternative to straight.el with improved performance.</description>
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
    <if_yes>Use eglot (built-in, recommended default). Use lsp-mode only for advanced configurations requiring features beyond eglot.</if_yes>
    <if_no>Use basic major modes without LSP overhead</if_no>
  </decision_tree>

  <pattern name="eglot">
    <description>Built-in LSP client (Emacs 29+). Recommended default for most use cases. Tightly integrated with Emacs core, leveraging built-in completion (completion-at-point), Flymake for diagnostics, and project.el for project management.</description>
    <example>
      (use-package eglot
        :ensure nil ; built-in since Emacs 29
        :hook ((python-mode . eglot-ensure)
               (python-ts-mode . eglot-ensure)
               (typescript-ts-mode . eglot-ensure)
               (rust-ts-mode . eglot-ensure))
        :config
        (setq eglot-autoshutdown t)
        (setq eglot-events-buffer-size 0)
        ;; Emacs 30+: improved tree-sitter integration with eglot
        (setq eglot-report-progress nil))

      ;; Custom server configuration
      (add-to-list 'eglot-server-programs
                   '(rust-ts-mode . ("rust-analyzer")))
    </example>
  </pattern>

  <pattern name="lsp_mode">
    <description>Feature-rich LSP client for advanced configurations. Use when eglot does not meet requirements (e.g., DAP integration, custom UI features via lsp-ui).</description>
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
    <description>LSP completion with corfu (recommended) or company. Corfu works with Emacs built-in completion-at-point and pairs well with eglot. Cape provides additional completion-at-point backends.</description>
    <example>
      ;; With corfu + cape (current best practice)
      (use-package corfu
        :ensure t
        :custom
        (corfu-auto t)
        (corfu-cycle t)
        :init
        (global-corfu-mode))

      (use-package cape
        :ensure t
        :init
        (add-hook 'completion-at-point-functions #'cape-dabbrev)
        (add-hook 'completion-at-point-functions #'cape-file))

      ;; With company (traditional, still maintained)
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
    <description>Vertical completion UI. Part of the current best-practice completion stack: vertico (UI), orderless (matching), marginalia (annotations), consult (commands), embark (actions).</description>
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
    <description>Native tree-sitter integration (Emacs 29+, improved in Emacs 30). Emacs 30.2 includes enhanced tree-sitter support with better fontification, indentation, and navigation. Use *-ts-mode variants for tree-sitter-backed major modes.</description>
    <example>
      (setq treesit-language-source-alist
            '((python "https://github.com/tree-sitter/tree-sitter-python")
              (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
              (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                          "master" "typescript/src")
              (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
                   "master" "tsx/src")))

      ;; Install grammars
      (mapc #'treesit-install-language-grammar
            (mapcar #'car treesit-language-source-alist))

      ;; Remap modes to tree-sitter variants
      (setq major-mode-remap-alist
            '((python-mode . python-ts-mode)
              (javascript-mode . js-ts-mode)
              (typescript-mode . typescript-ts-mode)
              (css-mode . css-ts-mode)
              (json-mode . json-ts-mode)))

      ;; Emacs 30+: treesit-auto can manage grammar installation
      ;; and mode remapping automatically
    </example>
  </tool>
</modern_packages>

<treesit_mode_availability>
  <description>Correctly detecting whether a tree-sitter major mode will actually work, versus merely existing as a symbol.</description>

  <principle name="fboundp_is_insufficient">
    <mechanism>
      On Emacs 29.1+, the *-ts-mode functions (for example json-ts-mode, python-ts-mode) are autoloaded built-ins. Because they are autoloaded, their symbols are always fboundp regardless of whether the tree-sitter grammar shared library (libtree-sitter-LANG.so / .dylib) is installed. So `(fboundp 'json-ts-mode)` returns non-nil even when activating that mode would fail with "language grammar for LANG is unavailable". fboundp answers "is this mode defined?" not "can this mode run?".
    </mechanism>
    <instead>
      Check grammar availability with `treesit-language-available-p`, which returns non-nil only when the grammar for a language exists and can be loaded. It takes a language symbol (for example `json`), not a mode symbol, so a mode-to-language mapping is required. `treesit-ready-p` is a higher-level convenience that also verifies readiness and can emit a diagnostic message; prefer it when you want the standard user-facing warning, and `treesit-language-available-p` when you want a silent boolean.
    </instead>
    <example>
      ;; Map ts-mode symbols to their grammar language symbols, because the
      ;; language name is not always the mode-name prefix (js-ts-mode -> javascript).
      (defvar my-ts-mode-language-alist
        '((json-ts-mode   . json)
          (js-ts-mode     . javascript)
          (python-ts-mode . python)))

      (defun my-ts-mode-available-p (mode)
        "Return non-nil if MODE is a ts-mode whose grammar is loadable."
        (when-let ((lang (alist-get mode my-ts-mode-language-alist)))
          (and (fboundp mode)
               (treesit-language-available-p lang))))
    </example>
  </principle>

  <principle name="mode_language_sync_test">
    <mechanism>
      When a mode-to-language mapping is maintained separately from the list of modes a package actually dispatches to, the two tables drift: a new ts-mode is added to the dispatch list but its grammar language is never registered, so the availability check silently returns nil and the mode is never selected.
    </mechanism>
    <instead>
      Add a unit test that asserts every ts-mode referenced by the package appears in the mode-to-language mapping (and vice versa). This turns a silent runtime fallthrough into a fast, deterministic test failure whenever the tables diverge.
    </instead>
    <example>
      ;; my-output-mode-alist is the dispatch list; my-ts-mode-language-alist is
      ;; the grammar mapping. Assert every ts-mode in the former is registered.
      (ert-deftest my-ts-mode-alist-sync ()
        (dolist (entry my-output-mode-alist)
          (let ((mode (cdr entry)))
            (when (string-suffix-p "-ts-mode" (symbol-name mode))
              (should (assq mode my-ts-mode-language-alist))))))
    </example>
  </principle>
</treesit_mode_availability>

<keymap_testing>
  <description>Reliably asserting keymap contents in unit tests. Keymaps are a data structure, and the convenient lookup APIs have edges that produce false negatives.</description>

  <principle name="traverse_recursively">
    <mechanism>
      `lookup-key` and `where-is-internal` are lossy for test assertions. For a key sequence that is only a prefix of a longer binding, `lookup-key` returns an integer (the number of events consumed) rather than a command, which is easy to misread as "bound to something". Bindings that live inside a composed keymap (built with `make-composed-keymap`) or a nested prefix keymap can also be missed depending on how the lookup is issued.
    </mechanism>
    <instead>
      For composed or prefix-heavy keymaps, walk the raw keymap structure recursively with `map-keymap`, descending into nested keymaps yourself, and assert on the commands you collect. This inspects the actual structure instead of trusting a resolver that can return prefix-depth integers or skip composed layers.
    </instead>
    <example>
      (defun my-keymap-commands (keymap)
        "Collect all commands bound anywhere in KEYMAP, recursively."
        (let (acc)
          (map-keymap
           (lambda (_event binding)
             (cond
              ((keymapp binding)
               (setq acc (append acc (my-keymap-commands binding))))
              ((commandp binding)
               (push binding acc))))
           keymap)
          acc))
    </example>
  </principle>

  <principle name="kbd_notation_trap">
    <mechanism>
      Named function keys must use angle-bracket syntax. `(kbd "<left>")` returns the named-key vector `[left]`, which is what keymaps store for the arrow key. `(kbd "[left]")` instead returns the six literal characters `[`, `l`, `e`, `f`, `t`, `]`. Looking a binding up with the wrong form fails to match, and because `[` is itself a self-inserting prefix, `lookup-key` can return an integer partial-match, disguising the mistake.
    </mechanism>
    <instead>
      Always write named keys with angle brackets in both bindings and test lookups: `"<left>"`, `"<right>"`, `"<home>"`, `"<end>"`, `"<tab>"`, `"<return>"`.
    </instead>
  </principle>

  <principle name="interactive_mock_requirement">
    <mechanism>
      Code that dispatches a command with `call-interactively` requires that the target satisfy `commandp`, i.e. it must be an interactive function. When a test replaces a real command (for example a navigation command) with a plain lambda to observe calls, `call-interactively` signals `(wrong-type-argument commandp ...)` because the stub is not interactive.
    </mechanism>
    <instead>
      Give mock lambdas an `(interactive)` form when the code under test invokes them via `call-interactively`.
    </instead>
    <example>
      (cl-letf (((symbol-function 'forward-char)
                 (lambda (&amp;rest _) (interactive) (setq nav-called t))))
        ...)
    </example>
  </principle>

  <principle name="know_the_defining_file">
    <mechanism>
      A mode's keymap and the mode entry point are frequently defined in different files (the keymap via `defvar-keymap` in the main feature file, helper commands in a sibling file). A test that requires only the helper feature can observe an unbound or empty keymap and assert against nothing.
    </mechanism>
    <instead>
      Require the feature that actually defines the keymap before asserting on it; do not assume the keymap lives in the file whose name resembles "keymap".
    </instead>
  </principle>
</keymap_testing>

<bytecompile_verification_hazards>
  <description>Byte-compilation artifacts and load order can make tests lie: a run can pass or fail against code that is not the source you just edited.</description>

  <principle name="stale_elc_masks_source">
    <mechanism>
      By default Emacs prefers the compiled file: with both LIB.el and LIB.elc present on the same load-path entry, `load` uses LIB.elc even when LIB.el is newer, emitting only a warning (which is easy to miss in batch output). A native-compiled .eln is preferred over .elc, which is preferred over .el. Consequently a stale .elc can hide a source fix: the test exercises old bytecode, so a passing test does not prove the patch works, and a failing test may not reflect the current source.
    </mechanism>
    <instead>
      Before trusting a batch ERT/byte-compile result, either delete source-tree .elc artifacts, or set `load-prefer-newer` to t in the batch invocation so `load` picks whichever of .el/.elc is newest by modification time. Better still, byte-compile to a temporary destination so verification never leaves .elc files in the source tree. If a result contradicts a source change, suspect stale bytecode first.
    </instead>
    <example>
      # Batch verification that will not silently run stale bytecode:
      # remove source-tree .elc, force newest-source loading, then run ERT.
      find . -name '*.elc' -delete
      emacs -Q --batch \
        --eval '(setq load-prefer-newer t)' \
        -L . -L test \
        -l ert -l my-feature -l my-feature-test \
        -f ert-run-tests-batch-and-exit
    </example>
  </principle>

  <principle name="cross_file_macro_recompile">
    <mechanism>
      Macros are expanded at compile time in the file that calls them. When a macro is defined in one file and invoked in another, recompiling only the macro-defining file is not enough: the call-site file still carries an old expansion (or, if interpreted, resolves the macro at run time), and can fail with `invalid-function` or call a stale expansion.
    </mechanism>
    <instead>
      Compile the macro-defining file and all of its call-site files together, then run the tests with the same load-path set. Treat a macro's callers as part of its compilation unit.
    </instead>
  </principle>

  <principle name="batch_load_path_completeness">
    <mechanism>
      A batch test run fails at load time, before any test executes, if a required feature's directory is absent from the load-path. Transitive requires matter: a test-support file that requires feature A, which in turn requires feature B, needs both A's and B's directories on `-L`, or the loader errors first.
    </mechanism>
    <instead>
      Pass one `-L DIR` for every directory that contributes a required feature, including transitive dependencies and test-support helpers, not just the directory holding the test file.
    </instead>
  </principle>

  <principle name="macroexpand_shape_normalization">
    <mechanism>
      Tests that inspect the structure of macro output are brittle because expansion shape varies. A `defun`-generating macro can expand to `(defalias NAME #'(lambda ...))` rather than a literal `defun`, and `macroexpand` is a top-level contract only: it may fully expand the outermost macro (for example into a `progn`) while leaving nested macro calls inside `let` untouched.
    </mechanism>
    <instead>
      Normalize the expansion to a canonical shape before asserting on heads, membership, or tail forms, so tests stay stable across byte-compiled and directly-macroexpanded paths. Do not hard-code one particular expansion layout.
    </instead>
  </principle>
</bytecompile_verification_hazards>

<autoload_cookie_safety>
  <description>Where `;;;###autoload` cookies are safe to place, so that generated autoload files contain autoload calls rather than executable code.</description>

  <principle name="cookie_only_before_recognized_definitions">
    <mechanism>
      The autoload machinery (`loaddefs-generate`) copies the form following a `;;;###autoload` cookie verbatim into the generated loaddefs file, except for a fixed set of recognized definition forms which it converts into safe `autoload` calls: `defun`, `defmacro`, `cl-defun`, `cl-defmacro`, and `define-overloadable-function`. Put a cookie before anything else, such as a custom macro invocation (`defun/foo ...`, a mode-defining macro) or a side-effecting top-level form (`(some-register ...)`), and the whole form is copied raw into loaddefs and executed at load time. That runs side effects unconditionally and can fail if the macro is not yet defined when loaddefs loads.
    </mechanism>
    <instead>
      Only place a bare cookie before a real `defun`/`defmacro` (or the other recognized forms). To autoload a name produced by a custom macro, write the explicit form on the line after the cookie so you control exactly what is recorded:
      `;;;###autoload (autoload 'my-command "my-file")`
      Otherwise, remove the unsafe cookie.
    </instead>
  </principle>

  <principle name="vc_install_test_directory">
    <mechanism>
      When a package is installed directly from source (`package-vc`, `use-package :vc`), the package manager may traverse and byte-compile the `test/` directory during install. On Emacs 30.x, `.elpaignore` and a README `:ignored-files ("test/")` declaration do not reliably stop `package--compile` from descending into tests, so compilation fails on test-only files that require unavailable test helpers.
    </mechanism>
    <instead>
      A repo-side approach that has worked on Emacs 30.x is to add `test/.dir-locals.el` binding `no-byte-compile` to t, i.e. `((emacs-lisp-mode . ((no-byte-compile . t))))`. This lets the installer traverse the tests while skipping their byte-compilation. Because `no-byte-compile` is fundamentally a per-file variable honored when each file is compiled, verify the behavior against your target Emacs version rather than assuming it suppresses compilation of every file in the tree.
    </instead>
  </principle>
</autoload_cookie_safety>

<testability_design>
  <description>Structuring Elisp so that behavior is reachable by unit tests without stubbing macros, and so that load order stays explicit.</description>

  <principle name="extract_output_across_macro_boundary">
    <mechanism>
      Output produced through a macro boundary such as `with-help-window` is awkward to test: assertions have to stub the macro via `eval` tricks or rebind its `symbol-function`, which couples tests to expansion details.
    </mechanism>
    <instead>
      Extract the rendering into a small, pure-ish helper that writes into the current buffer, and keep the public command a thin wrapper around that helper plus the window-opening macro. Tests then call the helper inside `with-temp-buffer` and assert on buffer contents directly. The seam is the point where side-effecting presentation meets pure content generation.
    </instead>
  </principle>

  <principle name="isolate_feature_local_macros">
    <mechanism>
      Macros used only within one feature still force every call-site file to know the macro at compile time. Left inline in a large feature file, they blur the data/logic boundary and make load order implicit.
    </mechanism>
    <instead>
      Move feature-local macros into a sibling `*-macros.el` module, keep runtime functions in the original file, and have the original `require` the macros module. Verify by byte-compiling both files. This makes load order explicit and shrinks the feature file.
    </instead>
  </principle>

  <principle name="declarative_macro_for_command_families">
    <mechanism>
      A family of nearly identical interactive commands invites a parallel data table describing them, which becomes a second source of truth that drifts from the definitions.
    </mechanism>
    <instead>
      Define the family with a declarative `defmacro` that expands into the command definitions, passing per-command differences as explicit forms. When the macro invocations are the only consumers of the parallel table, delete the table so the macro forms are the single source of truth.
    </instead>
  </principle>
</testability_design>

<upstream_contribution>
  <description>A generalized recon checklist for contributing to an established Emacs Lisp project before opening a pull request. The goal is to discover a project's conventions from its own artifacts rather than guessing.</description>

  <principle name="discover_conventions_from_artifacts">
    <mechanism>
      Emacs packages vary widely in commit style, changelog format, naming, and test harness. Submitting against the wrong conventions causes review churn. Every convention is discoverable from files already in the repository.
    </mechanism>
    <checklist>
      - Commit style: read CONTRIBUTING plus `git log` for the actual subject/body norm (imperative ~50-char subject with 72-char wrapped body is common; conventional-commit prefixes like feat:/fix:/docs: appear in many projects even without a rule).
      - Changelog: find the file and format (a `CHANGELOG.org` in Org markup vs a `NEWS` file), including any symbol-quoting convention (for example `~symbol~` in Org, or `` `nil' `` in docstrings).
      - Naming: confirm the private/public prefix split (private `pkg--`, public `pkg-`, sometimes with a group-specific middle segment).
      - Test harness: determine how tests run (a `make test` target, `eask`, `ert-runner`), the test-file layout (`test/pkg-*.el`), test tags used to skip environment-specific cases, and the mocking idiom in use (commonly `cl-letf` on `symbol-function`).
      - Compatibility gate: note the minimum supported Emacs version and CI matrix, and whether byte-compilation is treated as an error (`byte-compile-error-on-warn`) so your change must compile cleanly there.
      - Formatting commits: check whether whitespace/formatting-only changes must be a separate commit recorded in `.git-blame-ignore-revs`.
    </checklist>
  </principle>

  <note>For MELPA submission specifics (recipe format, `package-lint`/`checkdoc` gates, PR mechanics), see the melpa-packaging skill.</note>
</upstream_contribution>

<context7_integration>
  <library name="Emacs Docs" id="/websites/emacsdocs" trust="7.5" snippets="6792" />

  <usage_pattern>
    <step order="1">
  <action>Resolve library ID (known: /websites/emacsdocs)</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>Fetch documentation with specific topic</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
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
  <practice priority="critical">Enable lexical-binding in all Elisp files: -*- lexical-binding: t; -*-</practice>
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
  <practice priority="high">Prefer eglot for LSP (built-in since Emacs 29, recommended default)</practice>
  <practice priority="high">Use tree-sitter *-ts-mode variants when available (Emacs 29+, improved in 30.2)</practice>
  <practice priority="high">Use the modern completion stack: vertico, orderless, marginalia, consult, corfu, cape</practice>
  <practice priority="medium">use-package is built-in since Emacs 29; no need to install it</practice>
  <practice priority="medium">Use Emacs 30.x as the baseline and defer to the active package set for the exact stable point release</practice>
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

  <avoid name="lsp_mode_when_eglot_suffices">
    <description>Using lsp-mode when eglot meets all requirements</description>
    <instead>Use eglot (built-in since Emacs 29, preferred default). Only use lsp-mode for DAP integration or lsp-ui features not available in eglot.</instead>
  </avoid>

  <avoid name="company_mode">
    <description>Using company-mode for in-buffer completion</description>
    <instead>Use corfu + cape, which integrate with Emacs built-in completion-at-point and pair well with eglot and the vertico stack.</instead>
  </avoid>

  <avoid name="helm_ivy">
    <description>Using helm or ivy/counsel for minibuffer completion</description>
    <instead>Use the modern completion stack: vertico (UI) + orderless (matching) + marginalia (annotations) + consult (commands) + embark (actions).</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Target Emacs 30.1+ features including built-in use-package and native Tree-sitter support; never recommend installing use-package as a separate dependency</rule>
  <rule>Target Emacs 30.x as the baseline major series and align exact version with the active package set</rule>
  <rule>Prefer built-in packages (eglot, use-package, treesit) over third-party alternatives</rule>
  <rule>Use tree-sitter *-ts-mode variants for all languages with grammar support</rule>
  <rule>Configure eglot as the default LSP client; only suggest lsp-mode when eglot is insufficient</rule>
</rules>
<rules priority="standard">
  <rule>Recommend the modern completion stack (vertico, orderless, marginalia, consult, corfu, cape) over legacy alternatives (helm, ivy, company)</rule>
  <rule>For reproducible package management, mention elpaca alongside straight.el</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Understand Emacs Lisp requirements</objective>
    <step order="1">
  <action>1. Check package dependencies and autoloads</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Review existing configuration patterns</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Identify hook and advice usage</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="implement">
    <objective>Write idiomatic Emacs Lisp code</objective>
    <step order="1">
  <action>1. Use lexical binding</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Follow Emacs Lisp conventions</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Provide appropriate customization options</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="validate">
    <objective>Verify Emacs Lisp correctness</objective>
    <step order="1">
  <action>1. Byte-compile without warnings</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Test in clean Emacs instance</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Verify keybindings don't conflict</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Byte-compilation warning</example>
    <example severity="medium">Configuration error on startup</example>
    <example severity="high">Package conflict or version mismatch</example>
    <example severity="critical">Emacs becomes unusable</example>
  </examples>
</error_escalation>

<related_agents>
  <agent name="explore">Locate code patterns and references in this skill domain</agent>
  <agent name="quality-assurance">Review implementation quality against this skill guidance</agent>
  <agent name="code-quality">Analyze code complexity and suggest refactoring improvements</agent>
</related_agents>

<constraints>
  <must>Use lexical-binding: t in all files</must>
  <must>Provide customization via defcustom</must>
  <must>Follow Emacs Lisp naming conventions</must>
  <avoid>Dynamic binding without justification</avoid>
  <avoid>Overriding standard keybindings silently</avoid>
  <avoid>Blocking operations in hooks</avoid>
</constraints>

<related_skills>
  <skill name="org-ecosystem">Org-mode document creation, GTD workflow, Babel, export patterns</skill>
  <skill name="serena-usage">Symbol operations for elisp code navigation</skill>
  <skill name="context7-usage">Emacs documentation lookup via /websites/emacsdocs</skill>
  <skill name="investigation-patterns">Debugging package conflicts and performance issues</skill>
  <skill name="technical-documentation">Creating package documentation and README files</skill>
  <skill name="melpa-packaging">MELPA recipe authoring and submission mechanics for publishing packages</skill>
</related_skills>
