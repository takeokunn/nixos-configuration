---
name: Common Lisp Ecosystem
description: This skill should be used when the user asks to "write common lisp", "CLOS", "ASDF", "defpackage", "defsystem", or works with Common Lisp, SBCL, or Coalton. Provides comprehensive Common Lisp ecosystem patterns and best practices.
version: 2.1.0
---

<purpose>
  Provide comprehensive patterns for Common Lisp, CLOS, ASDF system definition, SBCL-specific features, and Coalton integration.
</purpose>

<tools>
  <tool>Read - Analyze ASDF system definitions and Lisp source files</tool>
  <tool>Edit - Modify Common Lisp code and system definitions</tool>
  <tool>Bash - Run SBCL, Roswell, Qlot commands</tool>
  <tool>mcp__plugin_claude-code-home-manager_context7__query-docs - Fetch ASDF, SBCL, and library documentation</tool>
</tools>

<concepts>
  <concept name="s_expressions">Homoiconic syntax: code and data share the same structure, enabling powerful macro systems</concept>
  <concept name="clos">Generic functions with multiple dispatch; method combination (:before, :after, :around)</concept>
  <concept name="conditions">Handler-case for catching, restart-case for recovery points; more powerful than exceptions</concept>
  <concept name="packages">Namespace management with defpackage; use :import-from or local-nicknames over bare :use</concept>
</concepts>

<common_lisp_fundamentals>
  <concept name="s_expressions">
    <description>Code and data share the same syntax (homoiconicity). Enables powerful macro systems for code transformation.</description>
  </concept>

  <concept name="symbols">
    <description>First-class named objects used for identifiers. Interned in packages, can have value, function, and property list.</description>
  </concept>

  <concept name="multiple_values">
    <description>Functions can return multiple values using values, multiple-value-bind, multiple-value-list.</description>
  </concept>

  <concept name="dynamic_binding">
    <description>Special variables with dynamic scope using defvar/defparameter. Convention: *earmuffs* for special variables.</description>
  </concept>
</common_lisp_fundamentals>

<clos>
  <description>Common Lisp Object System - Generic functions and multiple dispatch</description>

  <pattern name="defclass">
    <description>Define a class with slots. Slot options: :initarg, :initform, :accessor, :reader, :writer, :type, :documentation.</description>
    <example>
      (defclass person ()
        ((name :initarg :name :accessor person-name)
         (age :initarg :age :accessor person-age))
        (:documentation "Represents a person."))
    </example>
  </pattern>

  <pattern name="defgeneric_defmethod">
    <description>Define generic functions with multiple method implementations.</description>
    <example>
      (defgeneric greet (entity)
        (:documentation "Greet an entity."))

      (defmethod greet ((p person))
        (format t "Hello, ~a!~%" (person-name p)))
    </example>
    <decision_tree name="when_to_use">
      <question>Do you need polymorphic behavior based on multiple types?</question>
      <if_yes>Use defgeneric and defmethod for multiple dispatch</if_yes>
      <if_no>Use regular functions for single implementation</if_no>
    </decision_tree>
  </pattern>

  <pattern name="method_combination">
    <description>Method qualifiers (:before, :after, :around) for aspect-oriented programming.</description>
    <example>
      (defmethod greet :before ((p person))
        (format t "Preparing to greet...~%"))

      (defmethod greet :around ((p person))
        (format t "[Start]~%")
        (call-next-method)
        (format t "[End]~%"))
    </example>
  </pattern>

  <pattern name="multiple_inheritance">
    <description>Classes can inherit from multiple parent classes. Uses C3 linearization for method resolution order.</description>
    <example>
      (defclass employee (person job-holder)
        ((employee-id :initarg :id :accessor employee-id)))
    </example>
  </pattern>
</clos>

<conditions>
  <description>Common Lisp condition system - Restarts and handlers</description>

  <pattern name="handler_case">
    <description>Handle conditions similar to try-catch</description>
    <example>
      (handler-case
          (/ 1 0)
        (division-by-zero (c)
          (format t "Caught: ~a~%" c)
          0))
    </example>
  </pattern>

  <pattern name="handler_bind">
    <description>Handle conditions without unwinding stack</description>
    <example>
      (handler-bind
          ((error #'(lambda (c)
                      (format t "Error occurred: ~a~%" c)
                      (invoke-restart 'use-value 0))))
        (restart-case
            (error "Something went wrong")
          (use-value (v) v)))
    </example>
  </pattern>

  <pattern name="restart_case">
    <description>Define recovery points</description>
    <example>
      (defun parse-entry (entry)
        (restart-case
            (parse-integer entry)
          (use-value (v)
            :report "Use a different value"
            :interactive (lambda () (list (read)))
            v)
          (skip-entry ()
            :report "Skip this entry"
            nil)))
    </example>
    <decision_tree name="when_to_use">
      <question>Can the error be recovered interactively or programmatically?</question>
      <if_yes>Provide restarts for different recovery strategies</if_yes>
      <if_no>Use handler-case for simple error handling</if_no>
    </decision_tree>
  </pattern>

  <pattern name="define_condition">
    <description>Define custom condition types for structured error handling.</description>
    <example>
      (define-condition invalid-input (error)
        ((value :initarg :value :reader invalid-input-value))
        (:report (lambda (c stream)
                   (format stream "Invalid input: ~a"
                           (invalid-input-value c)))))
    </example>
  </pattern>
</conditions>

<packages>
  <pattern name="defpackage">
    <description>Define packages with explicit dependencies and exports.</description>
    <example>
      (defpackage #:my-project
        (:use #:cl)
        (:import-from #:alexandria #:when-let #:if-let)
        (:export #:main
                 #:process-data))
    </example>
  </pattern>

  <pattern name="package_local_nicknames">
    <description>Define local package nicknames for shorter, clearer references.</description>
    <example>
      (defpackage #:my-project
        (:use #:cl)
        (:local-nicknames (#:a #:alexandria)
                          (#:s #:serapeum)))
    </example>
  </pattern>
</packages>

<asdf>
  <description>Another System Definition Facility - Build system for Common Lisp (ASDF 3.4+)</description>

  <pattern name="basic_defsystem">
    <description>Basic ASDF system definition with metadata and component dependencies.</description>
    <example>
      (defsystem "my-project"
        :description "My project description"
        :version "0.1.0"
        :author "Author Name"
        :license "MIT"
        :depends-on ("alexandria" "cl-ppcre")
        :components ((:file "package")
                     (:file "utils" :depends-on ("package"))
                     (:file "main" :depends-on ("utils"))))
    </example>
  </pattern>

  <pattern name="module_organization">
    <description>Organize system components into modules for better structure.</description>
    <example>
      (defsystem "my-project"
        :components
        ((:module "src"
          :components ((:file "package")
                       (:file "core" :depends-on ("package"))))
         (:module "tests"
          :depends-on ("src")
          :components ((:file "test-suite")))))
    </example>
  </pattern>

  <pattern name="package_inferred_system">
    <description>Infer dependencies from defpackage forms for modern, maintainable systems.</description>
    <example>
      (defsystem "my-project"
        :class :package-inferred-system
        :depends-on ("my-project/main"))

      ;; In my-project/main.lisp:
      (defpackage #:my-project/main
        (:use #:cl)
        (:import-from #:my-project/utils #:helper))
    </example>
    <decision_tree name="when_to_use">
      <question>Do you want automatic dependency inference from package definitions?</question>
      <if_yes>Use package-inferred-system for modern projects</if_yes>
      <if_no>Use traditional defsystem with explicit component dependencies</if_no>
    </decision_tree>
  </pattern>

  <pattern name="test_system">
    <description>Define test system with automatic test execution using test-op.</description>
    <example>
      (defsystem "my-project/test"
        :depends-on ("my-project" "fiveam")
        :components ((:file "tests"))
        :perform (test-op (o s)
                   (uiop:symbol-call :fiveam '#:run!
                     (uiop:find-symbol* '#:my-test-suite :my-project/test))))
    </example>
  </pattern>

  <pattern name="project_structure">
    <description>Recommended directory layout for Common Lisp projects.</description>
    <example>
      my-project/
      ├── my-project.asd
      ├── src/
      │   ├── package.lisp
      │   ├── utils.lisp
      │   └── main.lisp
      └── tests/
          └── test-suite.lisp
    </example>
  </pattern>
</asdf>

<asdf_path_resolution>
  <description>
    Resolving repository-relative files (fixtures, READMEs, data, sibling test fragments)
    correctly under both fresh-process ASDF loads and direct source loads. The core hazard:
    when ASDF loads a compiled FASL, *load-truename* points into the FASL output cache, not the
    source tree, so merge-pathnames against it resolves under the cache and fails.
  </description>

  <principle name="resolve_from_the_system_not_load_truename">
    <statement>Resolve project-local paths through the system object — asdf:system-relative-pathname or asdf:system-source-directory — not from *load-truename* / merge-pathnames. In a fresh test process *load-truename* may even be unbound inside a test file.</statement>
    <example>
      ;; robust: anchored to the system's source directory
      (asdf:system-relative-pathname :my-project "tests/fixtures/data.txt")

      ;; fragile under FASL loads: *load-truename* points into the cache
      ;; (merge-pathnames "fixtures/data.txt" *load-truename*)
    </example>
    <how_to_apply>
      Require :asdf at compile/load/execute time; resolve the base directory from the system
      when it is registered; fall back to *compile-file-truename* / *load-truename* /
      *load-pathname* only for direct script/source loads that run outside ASDF. This applies to
      any split test loader that calls load on sibling fragments.
    </how_to_apply>
  </principle>

  <principle name="initialize_source_registry_first">
    <statement>A fresh or inherited ASDF session must have its source registry pointed at the project root before asdf:load-system; loading the .asd file alone is not sufficient and can stall inside find-system/load-system discovery. Treat clean CL_SOURCE_REGISTRY execution as a required smoke path, run from a child process.</statement>
  </principle>
</asdf_path_resolution>

<asdf_system_definition_pitfalls>
  <description>Recurring traps when defining a library system plus its test system in a .asd file.</description>

  <pitfall name="conditional_test_system_definition">
    <problem>Guarding the test-system definition with (unless (asdf:find-system "proj/test" nil) ...) makes asdf:test-system recurse into the same .asd load path and can surface as a circular dependency during system discovery.</problem>
    <fix>Define the library system and the test system unconditionally; let ASDF handle repeated loads/redefinitions of the .asd file.</fix>
  </pitfall>

  <pitfall name="bare_operation_symbol_in_perform">
    <problem>Writing :perform (test-op ...) or :in-order-to with a bare test-op resolves to COMMON-LISP-USER::TEST-OP, which is not the ASDF operation class, and fails with class-not-found at run time.</problem>
    <fix>Qualify the operation as asdf:test-op in :perform, and prefer an explicit (asdf:test-system ...) call in the :perform body over a chained :in-order-to graph, which is easier to isolate and less likely to stall the compile/load plan.</fix>
  </pitfall>

  <pitfall name="relative_file_pathnames_in_raw_checkout">
    <problem>:file "src/..." / :file "t/..." relative component paths can raise "Invalid relative pathname" in a raw checkout.</problem>
    <fix>Group components under (:module "src" :pathname "src" :components (...)) so the module carries the pathname, rather than embedding directory segments in each :file.</fix>
  </pitfall>

  <pitfall name="canonical_system_in_alias_named_asd">
    <problem>Defining the canonical test system inside an alias-named .asd (e.g. proj-test.asd), so that loading the library does not let ASDF discover it, triggers an ASDF warning and a fresh-registry smoke gap.</problem>
    <fix>Keep the canonical proj/test system in the primary proj.asd; let the alias-named .asd define only a thin compatibility alias depending on proj/test. In a fresh registry, load the alias system explicitly before asserting the canonical one is reachable.</fix>
  </pitfall>

  <example>
    ;; proj.asd — both systems defined unconditionally; module carries the pathname;
    ;; the operation class is qualified as asdf:test-op and runs the framework directly.
    (defsystem "proj"
      :components ((:module "src" :pathname "src"
                    :components ((:file "package")
                                 (:file "core" :depends-on ("package"))))))

    (defsystem "proj/test"
      :depends-on ("proj" "fiveam")
      :components ((:module "tests" :pathname "tests"
                    :components ((:file "suite"))))
      :perform (asdf:test-op (o c)
                 (uiop:symbol-call :fiveam '#:run!
                   (uiop:find-symbol* '#:proj-suite :proj/test))))
  </example>
</asdf_system_definition_pitfalls>

<asdf_parallel_execution>
  <principle name="isolate_fasl_output_translations">
    <statement>Concurrent CLI/test invocations that each call asdf:load-system can race on an inherited default FASL cache and fail with "Failed to find the TRUENAME of ...fasl". Initialize output translations in the launcher, before load-system, to a private per-user cache, and keep that initialization in the packaged launcher (not only in ad hoc scripts) so every subcommand inherits it.</statement>
    <example>
      (asdf:initialize-output-translations
        '(:output-translations
          (t (:home ".cache" "common-lisp" :implementation))
          :ignore-inherited-configuration))
    </example>
  </principle>
</asdf_parallel_execution>

<constant_reload_safety>
  <principle name="defconstant_is_eql_reload_unsafe_for_compound_literals">
    <statement>ANSI leaves the consequences undefined if a constant is redefined to a value that is not eql to its current value; SBCL enforces this by signalling SB-EXT:DEFCONSTANT-UNEQL. Because eql is identity-based for compound objects, re-loading a file that defconstant's a vector, list, or string literal fails even when the contents are visually identical, since each load builds a fresh object.</statement>
    <how_to_apply>Reserve defconstant for scalars and objects with stable eql identity. For tables, vectors, quoted lists, string defaults, and any compound literal that must survive repeated load/compile cycles, use defparameter (or defvar). alexandria:define-constant with :test #'equal is the portable alternative when a genuine constant is required.</how_to_apply>
    <scope>The eql redefinition rule is ANSI; the DEFCONSTANT-UNEQL condition name is SBCL-specific.</scope>
    <example>
      ;; unsafe on reload: each load builds a fresh vector, not eql to the prior one
      (defconstant +md5-table+ #(1 2 3 4))    ; => SB-EXT:DEFCONSTANT-UNEQL on reload

      ;; reload-safe: mutable-binding forms rebind without an eql check
      (defparameter +md5-table+ #(1 2 3 4))

      ;; genuine constant with structural identity: alexandria:define-constant
      (alexandria:define-constant +md5-table+ #(1 2 3 4) :test #'equalp)
    </example>
  </principle>
</constant_reload_safety>

<test_suite_architecture>
  <description>Design principles for organizing a test system so it stays fast, isolatable, and
  robust against the compile-unit stalls documented in sbcl-usage.</description>

  <principle name="zero_runtime_deps_test_only_framework">
    <statement>Keep the main system's runtime dependencies at zero (or minimal) and concentrate test-only dependencies (e.g. FiveAM) in the separate proj/test system. Runtime source then loads in a plain SBCL image, while the canonical verification path is the one that pulls the test framework — commonly a pinned dev shell where the framework is provisioned.</statement>
  </principle>
  <principle name="stratified_suites">
    <statement>Stratify the test system into explicit tiers — unit, integration, e2e, perf — as separate components, and keep property-based test support in its own support file. This lets a fast tier run in isolation and keeps slow/perf tiers opt-in.</statement>
  </principle>
  <principle name="layered_component_decomposition">
    <statement>For a component that both defines a surface syntax and executes it, separate the specification/description layer, the parsing layer, and the orchestration layer into distinct units. Beyond clarity, this bounds each compile unit and lets every layer be loaded and tested independently.</statement>
  </principle>
</test_suite_architecture>

<sbcl>
  <description>Steel Bank Common Lisp - High-performance implementation (current: SBCL 2.6.x, monthly releases)</description>

  <pattern name="save_executable">
    <description>Create standalone executable with SBCL.</description>
    <example>
      (defun main ()
        (format t "Hello, World!~%")
        (sb-ext:exit :code 0))

      (sb-ext:save-lisp-and-die "my-app"
        :toplevel #'main
        :executable t
        :compression t)
    </example>
  </pattern>

  <pattern name="threading">
    <description>SBCL threading support with make-thread and mutex synchronization.</description>
    <example>
      (defvar *result* nil)

      (let ((thread (sb-thread:make-thread
                      (lambda ()
                        (setf _result_ (heavy-computation)))
                      :name "worker")))
        (sb-thread:join-thread thread))

      ;; Mutex
      (defvar _lock_ (sb-thread:make-mutex))
      (sb-thread:with-mutex (_lock_)
        (critical-section))
    </example>
  </pattern>

  <pattern name="foreign_function">
    <description>Call C functions from SBCL using sb-alien interface.</description>
    <example>
      (sb-alien:define-alien-routine "strlen" sb-alien:int
        (str sb-alien:c-string))

      (strlen "hello") ; => 5
    </example>
  </pattern>

  <pattern name="optimization">
    <description>Use declarations for type information and optimization settings. Options: type, ftype, inline, optimize.</description>
    <example>
      (defun fast-add (x y)
        (declare (type fixnum x y)
                 (optimize (speed 3) (safety 0)))
        (the fixnum (+ x y)))
    </example>
  </pattern>

  <pattern name="sbcl_extensions">
    <description>SBCL-specific extensions for system interaction and performance tuning.</description>
    <example>
      ;; Command-line arguments
      sb-ext:*posix-argv*

      ;; Execute external programs
      (sb-ext:run-program "/bin/ls" '("-l"))

      ;; Trigger garbage collection
      (sb-ext:gc)

      ;; POSIX interface: sb-posix
      ;; Network sockets: sb-bsd-sockets
    </example>
  </pattern>
</sbcl>

<coalton>
  <description>Statically typed functional programming on Common Lisp</description>

  <pattern name="basic_types">
    <description>Define algebraic data types in Coalton with type-safe operations.</description>
    <example>
      (coalton-toplevel
        (define-type (Maybe a)
          None
          (Some a))

        (declare safe-div (Integer -> Integer -> (Maybe Integer)))
        (define (safe-div x y)
          (if (== y 0)
              None
              (Some (/ x y)))))
    </example>
  </pattern>

  <pattern name="type_classes">
    <description>Define type classes for polymorphic behavior in Coalton.</description>
    <example>
      (coalton-toplevel
        (define-class (Printable a)
          (print-it (a -> String)))

        (define-instance (Printable Integer)
          (define (print-it x)
            (into x))))
    </example>
  </pattern>

  <pattern name="coalton_integration">
    <description>Coalton compiles to efficient Common Lisp code and is interoperable with regular CL.</description>
    <note>Use coalton-toplevel for type-safe code sections</note>
    <note>Coalton functions can call CL functions and vice versa</note>
    <note>Provides Hindley-Milner type inference with type classes</note>
  </pattern>
</coalton>

<common_patterns>
  <pattern name="with_macro">
    <description>Resource management with unwind-protect for cleanup.</description>
    <example>
      (defmacro with-open-socket ((var host port) &amp;body body)
        `(let ((,var (make-socket ,host ,port)))
           (unwind-protect
               (progn ,@body)
             (close-socket ,var))))
    </example>
  </pattern>

  <pattern name="loop_macro">
    <description>Loop macro for iteration with collection, filtering, and accumulation.</description>
    <example>
      (loop for item in list
            for i from 0
            when (evenp i)
              collect item into evens
            finally (return evens))
    </example>
  </pattern>

  <pattern name="format_directives">
    <description>Common format directives: ~a (aesthetic), ~s (standard), ~d (decimal), ~f (float), ~% (newline), ~{~} (iteration), ~[~] (conditional).</description>
    <example>
      (format t "~a is ~d years old~%" name age)
    </example>
  </pattern>

  <pattern name="documentation">
    <description>Document functions with docstrings explaining purpose and parameters.</description>
    <example>
      (defun my-function (arg)
        "Docstring describing the function.
         ARG is the argument description."
        (process arg))
    </example>
  </pattern>
</common_patterns>

<standard_libraries>
  <library name="alexandria">
    <description>Conservative utility library. Provides essential utilities: when-let, if-let, hash-table-alist, ensure-list, mappings, and more. De facto standard for CL projects.</description>
  </library>

  <library name="serapeum">
    <description>Comprehensive utility library (superset of alexandria). Provides additional utilities: string manipulation, sequences, types, binding macros, and more.</description>
  </library>

  <library name="cffi">
    <description>Common Foreign Function Interface. Portable FFI for calling C libraries from Common Lisp. Preferred over implementation-specific FFI (e.g., sb-alien).</description>
    <example>
      (cffi:defcfun ("strlen" c-strlen) :int
        (str :string))

      (c-strlen "hello") ; => 5
    </example>
  </library>
</standard_libraries>

<package_sources>
  <source name="quicklisp">
    <description>Primary package distribution for Common Lisp. Monthly dist updates with tested library versions.</description>
  </source>

  <source name="ultralisp">
    <description>Complementary distribution with more frequent updates. Tracks latest library versions from GitHub.</description>
  </source>
</package_sources>

<modern_tooling>
  <tool name="qlot">
    <description>Per-project dependency manager (like bundler/npm). Manages dependencies via qlfile, supports Quicklisp and Ultralisp distributions.</description>
    <use_case>Install dependencies from qlfile</use_case>
    <use_case>Run commands with project dependencies</use_case>
    <example>
      qlot install
      qlot exec ros run
    </example>
  </tool>

  <tool name="roswell">
    <description>Lisp implementation manager and script runner</description>
    <use_case>Install Lisp implementations or libraries</use_case>
    <use_case>Start REPL with specified implementation</use_case>
    <use_case>Build standalone executable</use_case>
    <example>
      ros install sbcl
      ros run
      ros build myapp.ros
    </example>
  </tool>
</modern_tooling>

<context7_integration>
  <description>Available Context7 documentation libraries for Common Lisp ecosystem.</description>

  <tool name="context7_common_lisp_docs">
    <description>Common Lisp Docs - General Common Lisp documentation</description>
    <param name="library_id">/lisp-docs/lisp-docs.github.io</param>
    <param name="trust_score">4.7</param>
    <param name="snippets">580</param>
  </tool>

  <tool name="context7_asdf">
    <description>ASDF - Another System Definition Facility documentation</description>
    <param name="library_id">/websites/asdf_common-lisp_dev</param>
    <param name="trust_score">7.5</param>
    <param name="snippets">190</param>
  </tool>

  <tool name="context7_sbcl">
    <description>SBCL - Steel Bank Common Lisp documentation</description>
    <param name="library_id">/sbcl/sbcl</param>
    <param name="trust_score">8.0</param>
    <param name="snippets">86</param>
  </tool>

  <tool name="context7_cffi">
    <description>CFFI - Common Foreign Function Interface documentation</description>
    <param name="library_id">/websites/cffi_common-lisp_dev</param>
    <param name="trust_score">7.5</param>
    <param name="snippets">198</param>
  </tool>

  <tool name="context7_fiveam">
    <description>FiveAM - Testing framework documentation</description>
    <param name="library_id">/websites/fiveam_common-lisp_dev</param>
    <param name="trust_score">7.5</param>
    <param name="snippets">164</param>
  </tool>

  <tool name="context7_coalton">
    <description>Coalton - Statically typed functional programming documentation</description>
    <param name="library_id">/coalton-lang/coalton</param>
    <param name="trust_score">6.6</param>
    <param name="snippets">568</param>
  </tool>

  <pattern name="retrieve_documentation">
    <description>Use resolve-library-id then get-library-docs for latest documentation.</description>
    <example>
      ;; Get ASDF documentation
      mcp__plugin_claude-code-home-manager_context7__query-docs
        context7CompatibleLibraryID="/websites/asdf_common-lisp_dev"
        topic="defsystem"
    </example>
  </pattern>
</context7_integration>

<best_practices>
  <practice priority="high">Use `*earmuffs*` for special variables</practice>
  <practice priority="high">Use +plus-signs+ for constants</practice>
  <practice priority="high">Prefer functional style, minimize mutation</practice>
  <practice priority="high">Provide restarts for recoverable situations</practice>
  <practice priority="high">Document exported symbols</practice>
  <practice priority="medium">Use appropriate condition types, not just error</practice>
  <practice priority="medium">Use check-type for argument validation</practice>
  <practice priority="medium">Prefer ASDF package-inferred-system for new projects</practice>
  <practice priority="medium">Consider Qlot for per-project dependency management</practice>
  <practice priority="medium">Use Roswell for portable script execution</practice>
  <practice priority="high">Use Alexandria and Serapeum as standard utility libraries</practice>
  <practice priority="medium">Use CFFI for foreign function calls (portable across implementations)</practice>
  <practice priority="medium">Consider Coalton for type-safe functional subsystems</practice>
  <practice priority="high">Resolve project-relative paths via asdf:system-relative-pathname / system-source-directory, never *load-truename* under FASL loads</practice>
  <practice priority="high">Define library and test systems unconditionally, and qualify operation classes as asdf:test-op in :perform</practice>
  <practice priority="medium">Use defparameter/defvar (or alexandria:define-constant :test #'equal) for compound-literal tables; reserve defconstant for eql-stable scalars</practice>
  <practice priority="medium">Initialize asdf output-translations to a private cache in launchers that may run concurrently</practice>
</best_practices>

<anti_patterns>
  <avoid name="global_state">
    <description>Global mutable state makes code harder to test and reason about.</description>
    <instead>Pass state explicitly or use closures to encapsulate mutable state.</instead>
  </avoid>

  <avoid name="bare_use">
    <description>Using :use for packages other than :cl creates namespace pollution.</description>
    <instead>Use :import-from or package-local-nicknames for clearer dependencies.</instead>
  </avoid>

  <avoid name="ignore_conditions">
    <description>Ignoring conditions loses error context and recovery opportunities.</description>
    <instead>Handle conditions with handler-case or handler-bind, and provide appropriate restarts.</instead>
  </avoid>

  <avoid name="deep_nesting">
    <description>Deeply nested code reduces readability and maintainability.</description>
    <instead>Extract helper functions and use early returns to reduce nesting depth.</instead>
  </avoid>

  <avoid name="eval_usage">
    <description>Using eval in application code is slow and defeats compile-time optimization.</description>
    <instead>Use macros for compile-time code generation or first-class functions for runtime dispatch.</instead>
  </avoid>

  <avoid name="read_macros_overuse">
    <description>Custom reader macros make code harder to read for others.</description>
    <instead>Use reader macros sparingly and document them clearly when necessary.</instead>
  </avoid>

  <avoid name="loop_for_everything">
    <description>Using the loop macro for all iteration, even when simpler constructs suffice.</description>
    <instead>Use mapcar/remove-if/reduce for simple functional transforms. Consider iterate or series for complex iteration that loop handles poorly. Reserve loop for multi-clause iteration with collection and accumulation.</instead>
  </avoid>

  <avoid name="ignoring_conditions_system">
    <description>Using simple error signaling without restarts, or catching and discarding conditions.</description>
    <instead>Design APIs with restart-case to offer recovery strategies. Use handler-bind to handle conditions without unwinding the stack when possible.</instead>
  </avoid>

  <avoid name="defconstant_for_compound_literals">
    <description>Using defconstant for vectors, lists, strings, or other compound literals that get reloaded; SBCL's eql redefinition check signals DEFCONSTANT-UNEQL on the fresh object even with identical contents.</description>
    <instead>Use defparameter/defvar, or alexandria:define-constant with :test #'equal when a real constant is needed.</instead>
  </avoid>

  <avoid name="load_truename_for_project_paths">
    <description>Resolving project fixtures/data via merge-pathnames against *load-truename*, which points into the FASL cache (or is unbound) when ASDF loads a compiled file.</description>
    <instead>Resolve through asdf:system-relative-pathname / asdf:system-source-directory.</instead>
  </avoid>

  <avoid name="conditional_test_system_definition">
    <description>Guarding a test-system definition with (unless (asdf:find-system ...)) so test-system recurses into the same ASD load and can surface as a circular dependency.</description>
    <instead>Define the test system unconditionally and let ASDF manage repeated ASD loads.</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Use ASDF for all system definitions; never load files directly</rule>
  <rule>Provide restarts for recoverable error conditions</rule>
  <rule>Document all exported symbols with docstrings</rule>
  <rule>Target SBCL 2.5+ features; use modern ASDF 3.3+ defsystem patterns; never use legacy DEFINE-SYSTEM forms</rule>
</rules>

<rules priority="standard">
  <rule>Use *earmuffs* for special variables, +plus-signs+ for constants</rule>
  <rule>Prefer :import-from over bare :use for clear dependencies</rule>
  <rule>Use check-type for argument validation at function boundaries</rule>
  <rule>Consider package-inferred-system for new projects</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Understand Lisp code requirements</objective>
    <step order="1">
  <action>1. Check ASDF system definition</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Review existing macros and patterns</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Identify CLOS class hierarchies</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="implement">
    <objective>Write idiomatic Common Lisp code</objective>
    <step order="1">
  <action>1. Use appropriate abstraction level</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Follow condition system for errors</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Design reusable macros carefully</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="validate">
    <objective>Verify Lisp code correctness</objective>
    <step order="1">
  <action>1. Load system with ASDF</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Run tests with appropriate framework</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Check for compilation warnings</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Style inconsistency</example>
    <example severity="medium">Compilation warning or type error</example>
    <example severity="high">Macro expansion error</example>
    <example severity="critical">Reader macro conflict</example>
  </examples>
</error_escalation>

<patterns>
  <pattern name="usage">
    <description>Apply this skill when task keywords and domain match</description>
    <example>Use the canonical workflow and verify with project conventions</example>
  </pattern>
</patterns>

<decision_tree name="skill_activation">
  <question>Does the task clearly match this skill domain?</question>
  <branch condition="Yes">Use this skill workflow and constraints</branch>
  <branch condition="No">Use a more appropriate domain skill</branch>
</decision_tree>

<related_agents>
  <agent name="explore">Locate code patterns and references in this skill domain</agent>
  <agent name="quality-assurance">Review implementation quality against this skill guidance</agent>
  <agent name="code-quality">Analyze code complexity and suggest refactoring improvements</agent>
</related_agents>

<constraints>
  <must>Use ASDF for system definition</must>
  <must>Follow condition system for error handling</must>
  <must>Document macros with clear examples</must>
  <avoid>Overly complex macros without documentation</avoid>
  <avoid>Global state without clear lifecycle</avoid>
  <avoid>Reader macros without namespace isolation</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Navigate CLOS hierarchies, generic functions, and symbol definitions</skill>
  <skill name="context7-usage">Access ASDF, SBCL, and Common Lisp library documentation</skill>
  <skill name="investigation-patterns">Debug condition handling, macro expansion, and SBCL-specific issues</skill>
  <skill name="sbcl-usage">Operational SBCL execution, debugger, profiling, and executable build workflows</skill>
</related_skills>
