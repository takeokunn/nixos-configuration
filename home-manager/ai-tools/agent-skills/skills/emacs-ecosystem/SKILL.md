---
name: Emacs Ecosystem
description: Use for Emacs Lisp, init.el, use-package, and Emacs runtime hazards such as hook ordering, condition-case versus quit, overlays versus text properties, buffer-local state, keymap precedence, and subprocess handling. For org-mode, see org-ecosystem.
version: 2.4.0
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

<keymap_precedence>
  <description>Which keymap wins a key lookup, and why a keymap object reachable from more than one buffer must never be mutated to express buffer-local state. Sibling to keymap_testing above, which covers asserting keymap contents.</description>

  <principle name="major_mode_map_cannot_outrank_a_minor_mode_map">
    <mechanism>
      Emacs consults keymaps in a fixed order, and the major-mode map sits near the bottom of it: overriding and terminal-local maps first, then `emulation-mode-map-alists`, then `minor-mode-overriding-map-alist`, then `minor-mode-map-alist`, then the local (major-mode) map, then the global map. Any package that installs its bindings through the minor-mode layers — modal editing packages all do — therefore shadows a major-mode map unconditionally, no matter how that map is populated. A feature that must see raw keys, such as an input layer or a mode forwarding keystrokes to a subprocess, cannot achieve that with a major-mode keymap.
    </mechanism>
    <instead>
      Install bindings that must outrank other minor modes through `emulation-mode-map-alists`. Because a package enabled later in the session pushes its own entry ahead of yours, re-assert your entry at the head of that list every time your mode is enabled, not once at load time. When you only need to outrank other minor modes within one buffer, `minor-mode-overriding-map-alist` is the buffer-local equivalent and is the smaller hammer.
    </instead>
  </principle>

  <principle name="never_mutate_a_shared_keymap_for_local_state">
    <mechanism>
      The keymap created by `define-minor-mode` or bound by `defvar-keymap` is a single global object, and every buffer using that mode reaches the same object. Mutating it to reflect buffer-local state — most commonly setting its parent with `set-keymap-parent` to select an input mode, layout, or profile — therefore changes the effective bindings of every other buffer using that mode at the same time. The symptom appears far from the cause: switching state in one buffer silently rebinds keys in another.
    </mechanism>
    <instead>
      Leave the shared map immutable and compose per buffer, installing the composition as that buffer's local map: `(use-local-map (make-composed-keymap buffer-specific-map shared-mode-map))`. Composition allocates a fresh object per buffer and leaves the shared map untouched. This is the keymap instance of a wider rule — buffer-local state belongs in an object the buffer owns — which also governs the choice between overlays and text properties.
    </instead>
    <example>
      ;; Per-buffer variation, without touching the shared mode map.
      (defvar-local my-feature--local-map nil
        "Buffer-local overlay of bindings for the current input state.")

      (defun my-feature--apply-local-map ()
        (use-local-map
         (make-composed-keymap my-feature--local-map my-feature-mode-map)))
    </example>
  </principle>
</keymap_precedence>

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

  <principle name="compilation_removes_the_seam_you_stubbed">
    <mechanism>
      `cl-letf` on a symbol's function cell intercepts only calls that actually go through that cell, and two byte-compiler behaviours route around it silently. First, the compiler lowers many primitives to dedicated opcodes: a compiled caller of `set`, `setcar`, `setcdr`, `car`, `cdr`, or `aref` emits the opcode and never consults the function cell, so a fault injected into such a primitive is never triggered. Second, a function defined with `defsubst` is inlined into its compiled callers, so a stub installed on the `defsubst`'s own symbol is never consulted either. In both cases the stub installs without complaint and the code under test runs the real implementation, producing a green result for an injection that never happened — or a red one attributed to the wrong cause.
    </mechanism>
    <instead>
      Inject faults at a named, non-inlinable boundary you own: put the primitive mutation behind an ordinary `defun`, or expose an explicit injectable hook variable, and stub that instead of the primitive. Keep any function that tests rely on as a `cl-letf` seam as a plain `defun`; never promote such a function to `defsubst` for speed. Assert that the injected fault actually fired — a counter incremented, a sentinel observed — rather than only asserting the downstream outcome, and run that assertion in both the interpreted and the byte-compiled configuration, because the interpreted run is the one where the stub does work.
    </instead>
  </principle>

  <principle name="load_path_candidate_order_beats_timestamp">
    <mechanism>
      `load-prefer-newer` chooses between `.el` and `.elc` within a single load-path directory. It says nothing about which directory is consulted first. When the same feature exists both in the worktree and in an installed location — a Nix site-lisp path, a distribution's package directory, an installed `package.el` tree — the first matching candidate in `load-path` wins regardless of modification time, so an installed `.elc` can shadow the source just edited even with `load-prefer-newer` set. In one investigation, ten of fifteen apparent test failures were this loader false negative rather than a regression.
    </mechanism>
    <instead>
      In a hermetic verification run, place every worktree source directory ahead of any installed location explicitly, and then prove provenance rather than assuming it. `(symbol-file 'my-feature-function)` reports the file a definition was actually loaded from, and `(locate-library "my-feature")` reports which candidate the loader would pick. Consult both first whenever observed behaviour contradicts the current source; deleting `.elc` files and setting `load-prefer-newer` does not settle the multi-candidate case.
    </instead>
  </principle>

  <principle name="warning_suppression_has_specific_correct_forms">
    <mechanism>
      Under `byte-compile-error-on-warn`, two natural suppression attempts do not work. A runtime `(boundp 'other-package-var)` guard does not suppress the free-variable warning, because the compiler sees the direct reference inside the guarded branch whether or not the branch ever runs. And on Emacs 29, a `cl-defstruct` with many slots generates a constructor usage docstring that can exceed the docstring width limit, and `with-suppressed-warnings ((docstrings) ...)` does not suppress that particular warning there.
    </mechanism>
    <instead>
      Read an optional late-bound global through `(symbol-value 'other-package-var)` after the `boundp` check; the compiler cannot resolve that into a free-variable reference. Use `symbol-value` only for optional data access — when the variable is a genuine cross-module mutation contract, declare it with a value-less `(defvar other-package-var)` so the contract stays visible instead. For the generated-docstring case, wrap only the offending `cl-defstruct` form in `with-no-warnings`, which is the narrowest scope that actually works, rather than widening suppression across the file.
    </instead>
  </principle>
</bytecompile_verification_hazards>

<dynamic_module_artifacts>
  <description>When Emacs loads a compiled dynamic module, the file it loads is usually not the file the build just produced. Both failure modes below present as inexplicable behaviour rather than as build errors.</description>

  <principle name="loaded_artifact_versus_built_artifact">
    <mechanism>
      A module's load path is typically hardcoded to an install prefix rather than the build output directory, so a successful build does not imply the running Emacs sees the new code. A stale installed library keeps executing code that has since been deleted from the source, which surfaces as behavioural bugs — doubled effects, rendering artifacts, empty output — with no error message anywhere and no correspondence to the source being read.
    </mechanism>
    <instead>
      Before debugging module behaviour at all, compare the modification times of the built artifact and the installed artifact and confirm the install is current. Make this the first check rather than the last: it costs one command, and the symptom is otherwise indistinguishable from a real logic bug. This is the compiled-artifact sibling of a stale `.elc` shadowing source, and of a long-running Emacs daemon still holding previously loaded definitions.
    </instead>
  </principle>

  <principle name="macos_signature_invalidated_by_copy">
    <mechanism>
      On macOS, copying a dynamic library into place invalidates the linker-signed ad-hoc signature it was built with. AMFI then refuses the load and the kernel kills the process: Emacs dies with SIGKILL and exit status 137 at `module-load` time, with no Lisp error, no backtrace, and nothing naming the module that caused it.
    </mechanism>
    <instead>
      Re-sign after any copy of the module into its install location, with `codesign --force --sign - /path/to/module.dylib`. Treat an Emacs process that dies with status 137 immediately on module load as a signature problem until proven otherwise, since the failure produces no other diagnostic.
    </instead>
  </principle>
</dynamic_module_artifacts>

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

<lifecycle_and_error_boundaries>
  <description>Cleanup, teardown, and error propagation in Elisp. Each rule below follows from a fixed property of the Emacs condition system or the buffer and mode lifecycle. For the language-neutral rules about ownership, atomicity, rollback ordering, and aggregating failures across a collection, see the state-transactions skill; this section covers the Emacs mechanisms those rules have to be built on.</description>

  <principle name="quit_is_not_an_error">
    <mechanism>
      `C-g` signals the condition `quit`, and `quit` is not a subtype of `error`. A `condition-case` whose handler list names only `error` therefore does not run for a user interrupt: the non-local exit passes straight through the guard and skips whatever the handler was going to do. Because `C-g` is a routine user action rather than an exceptional one, this supposedly rare path is in fact common, and an adversarially injected `quit` walks out of a cleanup helper instead of making it fail closed.
    </mechanism>
    <instead>
      Every cleanup, teardown, or fault-isolating boundary must handle both `error` and `quit`. The correct aggregator shape is: capture the first condition that occurs, keep running every remaining step regardless, then re-signal the captured condition unchanged — same condition symbol, same payload — so callers observe the original failure rather than a cleanup artefact. When a second pending `C-g` could interrupt the restoration itself, bind `inhibit-quit` around the whole restoration.
    </instead>
    <example>
      (defun my-run-cleanup-steps (steps)
        "Run every function in STEPS, then re-signal the first failure."
        (let ((inhibit-quit t)
              primary)
          (dolist (step steps)
            (condition-case err
                (funcall step)
              ((error quit) (unless primary (setq primary err)))))
          (when primary
            (signal (car primary) (cdr primary)))))
    </example>
  </principle>

  <principle name="major_mode_change_erases_buffer_local_state">
    <mechanism>
      Changing a buffer's major mode calls `kill-all-local-variables`, which runs `change-major-mode-hook` first and erases buffer-local variable bindings afterwards. A buffer-local minor mode that records its resources — overlays, markers, timers, process objects, registry entries — in buffer-local variables therefore loses the handle to them the moment the user types `M-x fundamental-mode`. Its disable command never runs, `kill-buffer-hook` never runs because the buffer is still alive, and a global disable command can no longer discover the orphaned resources because the variable that named them is gone. Two independent packages have hit this in the same way.
    </mechanism>
    <instead>
      Register a buffer-local entry on `change-major-mode-hook` that calls one shared teardown function — the same one the disable command and `kill-buffer-hook` call — so resources are released while the local state still exists. Make that function idempotent, because all three paths can fire for the same buffer.
    </instead>
    <example>
      (defun my-feature--teardown ()
        "Release every resource this feature owns in the current buffer.
      Safe to call repeatedly."
        (mapc #'delete-overlay my-feature--overlays)
        (setq my-feature--overlays nil))

      (define-minor-mode my-feature-mode
        "Toggle My Feature in the current buffer."
        :lighter " MyF"
        (if my-feature-mode
            (progn
              (add-hook 'change-major-mode-hook #'my-feature--teardown nil t)
              (add-hook 'kill-buffer-hook #'my-feature--teardown nil t))
          (remove-hook 'change-major-mode-hook #'my-feature--teardown t)
          (remove-hook 'kill-buffer-hook #'my-feature--teardown t)
          (my-feature--teardown)))
    </example>
  </principle>

  <principle name="isolate_hook_functions_with_run_hook_wrapped">
    <mechanism>
      A hook variable is not a plain list. Its buffer-local value may contain the sentinel `t`, which splices the global value in at that position, and the standard runners honour that. Two mistakes follow. Wrapping the aggregate `run-hooks` call in a `condition-case` isolates the hook as a whole, so the first observer that signals prevents every later observer from running and can skip required follow-up such as cache invalidation. Hand-rolling the traversal with `dolist` over the variable's value loses the `t` splice and the local/global merge entirely, so buffer-local observers or global ones silently stop being called.
    </mechanism>
    <instead>
      Use `run-hook-wrapped`, which performs the standard traversal while calling a wrapper of your choosing around each individual hook function. Put the `condition-case` inside the wrapper, so one failing observer is isolated and the rest still run. Have the wrapper return nil so traversal continues. Demote only the ordinary errors the hook documents as suppressible, and never swallow `quit`.
    </instead>
    <example>
      (defun my-feature--call-observer (fn &amp;rest args)
        "Call FN with ARGS, demoting its errors so later observers still run."
        (condition-case err
            (apply fn args)
          (error (message "observer %S failed: %S" fn err)))
        nil)

      (run-hook-wrapped 'my-feature-after-change-hook
                        #'my-feature--call-observer buffer)
    </example>
  </principle>

  <principle name="variable_watcher_restoration_order">
    <mechanism>
      `add-variable-watcher` prepends to a symbol's watcher list while `remove-variable-watcher` deletes destructively, so watchers are not a stack that can be restored by replaying a saved list front to back — replaying in saved order reverses the effective order. This behaviour has been batch-verified on Emacs 30.2 and 31 and checked against the Emacs 29 implementation. Separately, installing an anonymous lambda as a watcher makes it unremovable by identity, so every module reload accumulates another copy: the same trap as putting a lambda in a hook, with no `remove-hook`-style escape.
    </mechanism>
    <instead>
      To save and restore watchers, snapshot `(copy-sequence (get-variable-watchers SYMBOL))` together with the bound-or-unbound state and the value. On restore, remove every watcher currently present including any the body installed, restore the value or the unbound state, then re-add the saved watchers in reverse order so the original ordering is reproduced. In production code, always install a stable named function and `remove-variable-watcher` it before re-adding, so reloading the module cannot accumulate duplicates.
    </instead>
  </principle>
</lifecycle_and_error_boundaries>

<buffer_state_and_editing_contracts>
  <description>Choosing the right primitive for state attached to a buffer, and the exact contracts of the editing primitives commands are built on. The general rule that you must never snapshot and restore state you do not own belongs to the state-transactions skill; what follows is the set of Emacs mechanisms that make ownership possible or impossible in the first place.</description>

  <principle name="overlays_are_owned_text_properties_are_not">
    <mechanism>
      Text properties live in the buffer text itself, so they are shared with every indirect buffer made from the same base buffer: applying `read-only` or `cursor-intangible` in an indirect buffer makes the base buffer read-only too, even though the buffer-local variable tracking that decoration is not shared and the base buffer has no record of it. Text properties also have no notion of an owner — two features writing the same property name over overlapping ranges are indistinguishable — so a feature that captures the previous value and restores it later silently discards whatever a concurrent writer added in between. That has been reproduced as real state loss, not as a theoretical concern. Overlays are the opposite: each belongs to exactly one buffer, is not shared with indirect buffers, and is a first-class object you hold a reference to and delete by identity.
    </mechanism>
    <instead>
      Represent decoration your feature owns and must be able to remove exactly — visibility, read-only guards, transient highlighting — with overlays, one per owned region, deleted by identity in teardown. Reserve text properties for attributes that genuinely belong to the text and should travel with it through copying and indirect buffers. Never implement ownership as "record the old value of a shared property and put it back later".
    </instead>
  </principle>

  <principle name="overlay_modification_hooks_have_endpoint_gaps">
    <mechanism>
      An overlay's `modification-hooks`, `insert-in-front-hooks`, and `insert-behind-hooks` do not fire for insertions at absolute buffer endpoints that fall outside the overlay, and a zero-width or otherwise degenerate overlay inherits the same hole because there is no interior for a change to land in. A region guard built only from overlay hooks therefore permits edits at `point-min` and `point-max`. The follow-on bug is just as reliable: cached marker bounds refreshed after a boundary edit drift out of sync with overlays that were never repositioned.
    </mechanism>
    <instead>
      Back an overlay-based region guard with buffer-local `before-change-functions` and `after-change-functions`, which observe every change including endpoint insertions. Keep the authoritative bounds in markers and resynchronize the overlays from those markers after each change, rather than treating overlay extents as the source of truth.
    </instead>
  </principle>

  <principle name="region_and_atomic_change_contracts">
    <mechanism>
      Two contracts of the core editing primitives are routinely misread. `(interactive "r")` supplies point and mark as a range whenever a mark exists at all — it does not require the region to be active — so a command declared with `"r"` will cheerfully transform a stale range the user does not believe is selected. And `atomic-change-group` groups buffer modifications so they undo as a unit and reverts buffer *text* on a non-local exit, but it does not restore point or mark, so an error or `C-g` partway through leaves the cursor and the region somewhere the user did not put them.
    </mechanism>
    <instead>
      Guard region-consuming commands with an explicit `use-region-p` check before turning point and mark into a range. For a destructive region edit, compute the replacement text before touching the buffer, save point and mark including the direction of the region, perform the delete-and-insert inside `atomic-change-group`, and restore point and mark yourself on both the error and the quit path.
    </instead>
  </principle>

  <principle name="self_insert_preserves_observer_semantics">
    <mechanism>
      A great deal of Emacs behaviour hangs off `post-self-insert-hook`: `electric-pair-mode`, `electric-indent-mode`, auto-fill, abbrev expansion, and many minor modes. `insert` does not run that hook. Code that types characters on the user's behalf — an input method, a snippet expander, a command that inserts a delimiter — therefore disables all of those features silently, with no error and no visible cause.
    </mechanism>
    <instead>
      Insert on the user's behalf with `self-insert-command`, dynamically binding `last-command-event` to the character, because `self-insert-command` and its observers read the character being typed from that variable rather than from an argument.
    </instead>
    <example>
      (defun my-insert-char (char n)
        "Insert CHAR N times as though the user had typed it."
        (let ((last-command-event char))
          (self-insert-command n char)))
    </example>
  </principle>
</buffer_state_and_editing_contracts>

<data_structure_hazards>
  <description>Properties of Elisp's built-in containers and of the evaluator that turn ordinary-looking code into silently wrong or scale-dependent behaviour.</description>

  <principle name="mutating_a_stored_key_orphans_the_entry">
    <mechanism>
      A hash table computes a key's hash once, at insertion. Under the `equal` test the key is compared structurally, so a mutable key — a string, a list, a vector — that the caller destructively modifies after `puthash` no longer hashes to the bucket its entry sits in. The entry becomes unreachable under both the old key and the new one while the physical entry remains, so logical size and physical size diverge and repeated put-then-mutate grows the table without bound. A reproducer with a capacity of one and twenty iterations ended with a logical size of one and a physical size of twenty. A separate edge in the same area: on Emacs 30 the built-in `equal` hash-table test can signal `circular-list` when it compares cyclic cons keys.
    </mechanism>
    <instead>
      A public API that accepts a caller-owned mutable value as a key must detach it before storing — `copy-sequence` for a string or vector, a deep copy for a structured key — so later caller mutation cannot reach the stored key. Where keys may be cyclic, register a dedicated test with `define-hash-table-test` backed by `sxhash-equal` and a cycle-safe comparison instead of relying on the built-in `equal` test. Unbounded growth of a table whose logical size stays small is the diagnostic signature of this bug.
    </instead>
  </principle>

  <principle name="recursion_bounded_by_collection_length">
    <mechanism>
      Elisp recursion is bounded by `max-lisp-eval-depth`, and that ceiling is reached by ordinary data rather than by pathological data. A recursive walk whose depth tracks the *length of a collection* — the characters of a long key, the entries of a hash bucket, the elements of a list being copied — passes every small test and then fails once real input arrives, which makes it a scale-dependent failure rather than a bug with a reproducible trigger. Recursion whose depth is bounded by *structural nesting*, such as the depth of a tree or the nesting of a form, is a different case and is generally fine.
    </mechanism>
    <instead>
      Rewrite any recursion whose depth tracks collection length as an explicit loop over an explicit worklist. To copy a mutable object graph that may contain cycles and shared structure, use a two-phase traversal with an `eq`-keyed memo table: first walk the graph allocating and memoizing one empty shell per mutable object while discovering children, then walk the memo connecting edges between the completed shells. This preserves cycles and shared identity in O(V+E) work at constant Lisp call depth.
    </instead>
  </principle>

  <principle name="absent_stamp_is_not_a_comparable_value">
    <mechanism>
      A cache validated by "the recorded stamp still equals the current stamp" degenerates into "always valid" whenever the stamp function returns nil for an absent input. If the stamp is a sentinel file's modification time read from `file-attributes`, a missing sentinel yields nil, the cached entry stores nil, and `(equal nil nil)` reports a hit forever — so the cache never notices that anything changed. The scheme is correct exactly when the sentinel exists, which is the case the author tested.
    </mechanism>
    <instead>
      Treat an absent or unobtainable stamp as a cache miss, not as a value to compare. Check for the sentinel explicitly and refuse to store an entry whose validity token is nil.
    </instead>
  </principle>
</data_structure_hazards>

<process_and_remote_boundaries>
  <description>Emacs process APIs have contracts that differ from what their names suggest, and Emacs file-name primitives silently accept remote paths that subprocess primitives silently ignore.</description>

  <principle name="with_timeout_does_not_bound_a_synchronous_call">
    <mechanism>
      `with-timeout` schedules a timer, and timers only fire when Emacs reaches its event loop. A synchronous `call-process` does not return to the event loop, so wrapping it in `with-timeout` does not reliably terminate a hung helper — Emacs simply stays blocked, and the timeout appears to work only because the helper usually returns quickly. The same call also accumulates unbounded stdout into its destination buffer and returns an exit status that is easy to discard by accident.
    </mechanism>
    <instead>
      Run any external helper that could hang under asynchronous process management: `make-process`, one decrementing wait budget shared by startup and by output draining, a byte-counted cap on accumulated stdout, a separate non-accumulating destination for stderr, and an explicit check that the exit status was zero before believing the output.
    </instead>
  </principle>

  <principle name="exit_sentinel_can_precede_pending_output">
    <mechanism>
      A sentinel reporting that a process finished does not mean its output has been delivered. Output the child already wrote may still be pending in the filter when the sentinel runs, so a wait loop that terminates on "the sentinel said finished" can return with stdout truncated or empty. A stress probe of a short-lived helper writing 32 bytes lost stdout in 12 of 20 runs for exactly this reason, and a test written against that helper fails intermittently in a full suite while passing when run alone.
    </mechanism>
    <instead>
      After the sentinel reports termination, keep draining with `accept-process-output` until the process is no longer live *and* no further output arrives, and only then treat collection as complete. Read an intermittent output-truncation failure that appears only under load as a drain-race signature rather than as flakiness to be retried away.
    </instead>
  </principle>

  <principle name="wait_budget_shared_by_startup_and_drain">
    <mechanism>
      Emacs gives a wait loop only one lever, the TIMEOUT argument of `accept-process-output`, and that argument bounds a single slice rather than the whole wait, so any aggregate bound has to be maintained by the caller across startup and draining alike. Maintain it as a budget each slice decrements rather than as an absolute deadline recomputed from `float-time`; the reasoning for preferring a budget over a wall-clock deadline is the state-transactions skill's general rule about time and accumulation.
    </mechanism>
    <instead>
      Pass a per-call timeout to `accept-process-output` and decrement a remaining-budget variable by the slice actually consumed, looping while the budget stays positive. Validate the configured budget on entry and reject degenerate values — non-numeric, NaN, non-finite, zero, negative — rather than clamping them silently. Cap the iteration count as well, so a slice that returns immediately cannot spin.
    </instead>
    <example>
      (defun my-drain (proc budget)
        "Drain PROC for at most BUDGET seconds of aggregate wait."
        (unless (and (numberp budget)
                     (&gt; budget 0)
                     (&lt; budget 1.0e+INF))
          (error "Invalid wait budget: %S" budget))
        (let ((remaining budget)
              (iterations 0))
          (while (and (process-live-p proc)
                      (&gt; remaining 0)
                      (&lt; (setq iterations (1+ iterations)) 10000))
            (let ((slice (min 0.05 remaining)))
              ;; JUST-THIS-ONE is non-nil here: this process is established.
              (accept-process-output proc slice nil t)
              (setq remaining (- remaining slice))))))
    </example>
  </principle>

  <principle name="just_this_one_asymmetry">
    <mechanism>
      The JUST-THIS-ONE argument of `accept-process-output` suppresses processing of other processes' events. That is what you want while draining a response body, because it stops unrelated filters running re-entrantly in the middle of your read. It is wrong while waiting for a `:nowait` network connection to be established, because the connection-completion event is dispatched through the same machinery: pinning attention to that one process can prevent Emacs from ever observing that it connected.
    </mechanism>
    <instead>
      When waiting for `:nowait` connection establishment, pass the process as PROCESS but leave JUST-THIS-ONE nil. When waiting for output on an already-established process, pass JUST-THIS-ONE non-nil.
    </instead>
  </principle>

  <principle name="process_tree_cleanup_needs_identity">
    <mechanism>
      A PID is not an identity. The operating system reuses PIDs, so a cleanup routine that records a PID and signals it later can signal an unrelated process. Descendants make it worse: a helper that forks and exits immediately leaves a `setsid` child that is reparented away before any process-table scan sees it, which reproduced in 10 of 10 attempts. And a scan bounded for memory and latency reaches its cap as *saturation*, which is not the same thing as having enumerated everything — treating the cap as completion silently orphans the remaining descendants.
    </mechanism>
    <instead>
      Identify a process by the pair of PID and immutable process start time, and re-verify that identity immediately before and immediately after stopping it. The safe sequence is SIGSTOP, re-verify identity, then SIGKILL only identities confirmed stopped, so a recycled PID can never be killed. Close the reparent race by putting a cryptographically opaque ownership token into the child's environment and scanning for that token immediately after launch as well as at the cleanup boundary, rather than relying on parentage. Report a saturated scan and any signal-delivery failure as an incomplete cleanup; never fold either into a successful result. Use a monotonic clock for the cleanup deadline, and spool large output to a bounded temporary file instead of an unbounded in-memory buffer.
    </instead>
  </principle>

  <principle name="remote_paths_block_and_subprocesses_do_not_follow">
    <mechanism>
      Emacs file-name primitives are remote-transparent: `file-exists-p`, `file-attributes`, and `directory-files` on a remote path go over the network and can block for a full remote-access timeout, which freezes the UI during what looked like a local bookkeeping operation. Subprocess primitives are not symmetrically transparent — `shell-command-to-string` and `call-process` run on the local machine regardless of `default-directory` being remote — so a helper invoked to inspect "the project" inspects the wrong machine and returns confidently wrong metadata. Two unrelated packages have been recorded hitting one side each of this asymmetry.
    </mechanism>
    <instead>
      Guard bulk or persistent filesystem work with `(and (file-exists-p path) (not (file-remote-p path)))` so a remote path cannot stall the UI. When a subprocess must run where the directory actually lives, use `process-file` and `start-file-process`, which honour a remote `default-directory`. When the tool genuinely exists only locally, detect `file-remote-p` and decline, rather than returning local results for a remote tree.
    </instead>
  </principle>
</process_and_remote_boundaries>

<untrusted_input_in_emacs>
  <description>Emacs-specific channels through which externally controlled data becomes executable, or escapes validation that appeared to cover it. The general discipline — validate at one owning boundary, fail closed, revalidate at each language boundary — belongs to the trust-boundaries skill; the mechanisms below are properties of particular Emacs APIs and are easy to miss precisely because the APIs look inert.</description>

  <principle name="opening_a_file_can_execute_it">
    <mechanism>
      Visiting a file applies its file-local variables, and a file-local `eval:` entry is code chosen by whoever wrote the file. Any package that opens a path derived from outside the user's own intent — a request handled by an in-Emacs server, a link inside rendered content, an entry from a project or search index — therefore hands arbitrary code execution to whoever controls that file, at the moment of preview.
    </mechanism>
    <instead>
      Bind `enable-local-variables` and `enable-local-eval` to nil around the open whenever the path came from untrusted input. There is no reason for a machine-driven open to honour file-local settings at all, and the mitigation is two bindings.
    </instead>
    <example>
      (let ((enable-local-variables nil)
            (enable-local-eval nil))
        (with-temp-buffer
          (insert-file-contents path)
          (my-render-preview)))
    </example>
  </principle>

  <principle name="decode_then_validate_is_not_enough">
    <mechanism>
      `url-unhex-string` is a decoder, not a validator, and it fails open in two directions. It normalizes some sequences — `%0d%0a` can emerge as spaces — so a check applied only to the decoded string never sees the CRLF that was present in the input, which is the classic header-injection bypass. And it preserves malformed percent triplets such as `%ZZ` verbatim rather than rejecting them, so invalid encodings pass straight through to whatever consumes the result.
    </mechanism>
    <instead>
      Validate the raw, still-encoded form first, rejecting percent-encoded control characters and malformed triplets there; then decode; then validate the decoded value again for the properties that only make sense after decoding. Neither check alone is sufficient, and the pre-decode check is the one that is usually missing.
    </instead>
  </principle>

  <principle name="strings_carry_text_properties_into_the_display">
    <mechanism>
      An Elisp string is not a leaf value: it can carry text properties, and properties such as `display`, `keymap`, `local-map`, and `face` change what the user sees and what their keys do. A string that arrived from a data file, a network response, or persisted state can carry any of them. Ordinary string operations preserve them — `concat` keeps them, `format` propagates them from a `%s` operand into its result, and `propertize` *adds* a property rather than replacing the existing set, so applying your own outer face does not remove an attacker-selected `keymap`. Copying does not help either: `copy-tree` does not copy strings, so a deep copy of a structure still shares the propertized string objects inside it.
    </mechanism>
    <instead>
      At the presentation boundary — the point where untrusted text is about to enter a buffer or be handed to `message` or `format` for display — replace it with a clean copy via `substring-no-properties`, then apply your own properties to that copy. Leave the original object intact when semantic properties on it matter internally; the stripping belongs at display time. It applies to every untrusted operand passed through the format call, not only to the one you were thinking about.
    </instead>
  </principle>
</untrusted_input_in_emacs>

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
  <practice priority="critical">Handle both error and quit in every cleanup path; condition-case on error alone does not catch C-g</practice>
  <practice priority="high">Give a buffer-local minor mode a change-major-mode-hook teardown, so resources are released before local variables are erased</practice>
  <practice priority="high">Use overlays, not text properties, for buffer state your feature owns and must remove exactly</practice>
  <practice priority="high">Strip properties with substring-no-properties before displaying a string that came from outside your code</practice>
  <practice priority="high">Bind enable-local-variables and enable-local-eval to nil when opening a file on behalf of untrusted input</practice>
  <practice priority="medium">Isolate hook observers with run-hook-wrapped rather than wrapping the aggregate run-hooks call</practice>
  <practice priority="medium">Prove which file a definition was loaded from with symbol-file before trusting a verification result</practice>
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

  <avoid name="with_timeout_around_call_process">
    <description>Bounding a synchronous call-process with with-timeout</description>
    <instead>The timer cannot fire while Emacs is blocked. Use make-process with an explicit deadline, a stdout byte cap, and exit-status validation.</instead>
  </avoid>

  <avoid name="insert_for_user_typed_characters">
    <description>Using insert for characters typed on the user's behalf, which skips post-self-insert-hook and disables electric-pair, auto-fill, and abbrev</description>
    <instead>Call self-insert-command with last-command-event bound to the character.</instead>
  </avoid>

  <avoid name="mutating_a_shared_keymap">
    <description>Setting the parent of a mode's global keymap to express buffer-local state, which changes every other buffer using that mode</description>
    <instead>Compose per buffer with make-composed-keymap and install the result via use-local-map.</instead>
  </avoid>

  <avoid name="defsubst_on_a_test_seam">
    <description>Declaring a function defsubst when tests stub it via cl-letf; inlining removes the call site and the stub is never consulted</description>
    <instead>Keep functions used as test seams as plain defuns, and assert that an injected fault actually fired.</instead>
  </avoid>

  <avoid name="recursion_over_collection_length">
    <description>Recursing once per element of a collection, which reaches max-lisp-eval-depth on ordinary large input</description>
    <instead>Loop over an explicit worklist. Recursion bounded by structural nesting is fine; recursion bounded by length is a latent scale failure.</instead>
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

<error_escalation>
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
  <skill name="state-transactions">Ownership, atomicity, rollback ordering, snapshot semantics, and the general time rule that a wait must be bounded by a decrementing budget rather than a wall-clock deadline; this skill supplies the Emacs mechanisms those rules are built on</skill>
  <skill name="trust-boundaries">General discipline for untrusted input: one owning validation boundary, fail closed, revalidate at each language boundary</skill>
  <skill name="testing-patterns">Test design, fixtures, and assertions that can actually fail, alongside the verification hazards documented here</skill>
  <skill name="lisp-macro">Macro-writing technique, hygiene, and compile-time contracts for Elisp code generation</skill>
</related_skills>
