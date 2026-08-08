---
name: Common Lisp Ecosystem
description: This skill should be used when the user asks to "write common lisp", "CLOS", "ASDF", "defpackage", "defsystem", or works with Common Lisp, SBCL, or Coalton. Covers package hygiene (export interns a symbol but does not define it; stub packages contaminating a shared image), condition design with define-condition and a bounded retained payload, never passing data as a format control string, Unicode-aware standard character predicates, validating before normalizing, read-time evaluation via the sharp-dot reader macro and its load-order limits, publishing a file atomically by rename within the target directory, and hash-table keys that must not be caller-owned mutable strings. Also covers ASDF dependency changes across every affected surface and keeping a mutable structure's derived index rebuilt through its mutators.
version: 2.4.0
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

  <principle name="construction_boundary_is_the_class_not_the_constructor">
    <statement>Exporting a class exports make-instance on it. Any validation, normalization, or invariant that lives only in a convenience constructor like make-foo is advisory: a caller who writes (make-instance 'foo :slot ...) bypasses all of it and gets an object the rest of the system assumes cannot exist.</statement>
    <why>The convenience constructor reads like the API, so reviewers check it and stop. Nothing in the source of make-foo hints that a second, unvalidated construction path is exported alongside it.</why>
    <how_to_apply>
      Pick one of three, and state which in the class documentation:
      enforce the invariants in an initialize-instance :after (or shared-initialize) method so every
      path runs them; keep the class package-internal and export only the constructor; or, when the
      class must be exported as-is, give every optional slot a bound :initform so direct make-instance
      is as safe as convenience construction. A privileged fast path must not be expressible as an
      initarg at all — carry it in package-internal dynamic state or a private constructor, or
      make-instance becomes a way to request the unvalidated path by name.
    </how_to_apply>
    <example>
      ;; invariant enforced at the construction boundary, not in a helper
      (defmethod initialize-instance :after ((c connection) &amp;key)
        (unless (slot-boundp c 'endpoint)
          (error "connection: endpoint is required"))
        (check-type (slot-value c 'timeout) (integer 1 3600)))

      ;; a trusted fast path must not be an initarg; keep it internal
      (defvar *trusted-construction* nil)   ; bound only by package-internal callers
    </example>
  </principle>
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

  <principle name="never_pass_data_as_a_format_control_string">
    <statement>An already-formatted message, a parser fragment, or anything else derived from input is data. Print it with ~A against a fixed control string; never pass it as the control string itself. FORMAT is a full language — ~R, ~V, deeply nested ~{~}, and the recursive ~? directive — so a control string under an attacker's influence is CL's format-string vulnerability, with resource exhaustion and information disclosure available directly from the directive set.</statement>
    <how_to_apply>
      Error helpers are where this is introduced, because (error msg) reads so naturally. Write
      (error 'my-error :detail msg) or (error "~A" msg) instead. Audit for any call whose control
      string is a variable rather than a literal. The same rule covers warn, cerror, and format
      itself.
    </how_to_apply>
    <example>
      ;; unsafe: MSG is interpreted as a control string
      (defun fail (msg) (error msg))

      ;; safe: MSG is data
      (defun fail (msg) (error "~A" msg))
    </example>
  </principle>

  <principle name="bound_the_payload_a_condition_retains">
    <statement>Sanitizing the report output is not enough: a public condition that retains an unbounded payload keeps it alive for the lifetime of the condition object, and anything that later prints, logs, or serializes the condition re-materializes it. Bound and sanitize at initialization, in the slot, not at render time.</statement>
    <how_to_apply>Validate report and backtrace limits against fixed hard maxima before allocating any buffer, and truncate during rendering rather than building an unbounded intermediate string and cutting it afterwards. The generic rule — enforce limits before allocation, not at emission — is owned by trust-boundaries; the CL-specific expression is that the intermediate is usually a with-output-to-string whose size nobody bounded.</how_to_apply>
    <example>
      ;; wrong shape: the blowup happens before the limit is consulted
      (let ((s (with-output-to-string (o) (print-object huge o))))
        (subseq s 0 (min +max+ (length s))))

      ;; right shape: the limit bounds the work, not just the result
      (with-output-to-string (o)
        (print-object-bounded huge o :limit +max+))
    </example>
  </principle>
</conditions>

<input_validation_in_common_lisp>
  <description>
    Two CL-specific ways a validation boundary silently stops validating. The general rules —
    enforce limits before allocation, validate raw input before a normalizing coercion — belong
    to trust-boundaries; what follows is the concrete shape they take in Common Lisp, where the
    standard library's own predicates and pathname coercions are the trap.
  </description>

  <principle name="standard_character_predicates_are_unicode_aware">
    <statement>digit-char-p is not an ASCII test. On implementations with full Unicode support it accepts decimal digits from any script — fullwidth, Arabic-Indic, Devanagari and others — returning their numeric weight. alpha-char-p and alphanumericp are Unicode-aware in the same way. Any grammar defined over U+0030 to U+0039 that reaches for digit-char-p has quietly widened its accepted language.</statement>
    <why>The widening is invisible in review because digit-char-p is exactly what the specification prose seems to ask for, and every ASCII test case passes. It surfaces as a downstream conversion failure, or worse, as two components disagreeing about whether a token was a number.</why>
    <how_to_apply>
      Define ASCII-only predicates once and use them at every stage of the grammar — start
      detection, digit consumption, numeric conversion, and stream framing. A single stage still
      using the standard predicate reintroduces the disagreement. Keep fullwidth, Arabic-Indic,
      and Devanagari digits as standing regression inputs; they are cheap and they catch the
      stage you forgot.
    </how_to_apply>
    <example>
      (declaim (inline ascii-digit-p))
      (defun ascii-digit-p (ch)
        (and (char&lt;= #\0 ch #\9) (- (char-code ch) (char-code #\0))))

      ;; (digit-char-p #\３) =&gt; 3   on a Unicode-capable implementation
      ;; (ascii-digit-p  #\３) =&gt; NIL
    </example>
    <scope>The standard permits but does not require implementations to recognize non-ASCII digits; the major ones do. Write the ASCII predicate rather than testing which behavior your implementation has.</scope>
  </principle>

  <principle name="validate_before_you_normalize">
    <statement>When a guard exists to reject input class A, and a normalizing coercion maps A into B, running the coercion first makes the guard unreachable. The check remains in the source, passes review, and is a no-op. Validate the original input, then coerce.</statement>
    <why>The observed case is the worst place for it: a recursive directory delete whose :validate option could not reject a bare file pathname, because ensure-directory-pathname had already folded the file name into directory form before the guard ran. "Normalize, then validate" is the safest-sounding possible ordering and is exactly backwards.</why>
    <how_to_apply>For any function taking both a pathspec and a validation flag, assert on the raw argument in the first form of the body. In CL specifically, watch every uiop pathname coercion — ensure-directory-pathname, ensure-pathname, parse-namestring with defaults — because each is lossy about precisely the distinction a guard is usually there to enforce.</how_to_apply>
  </principle>

  <related_skills>
    <skill name="trust-boundaries">Owns the general rules this section instantiates: limits enforced before allocation, raw input validated before normalizing coercion</skill>
  </related_skills>
</input_validation_in_common_lisp>

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

  <pitfall name="stub_packages_contaminate_a_shared_image">
    <problem>A verification script that defines a partial stub package before loading selected real sources permanently occupies that name in the image's global package namespace — a defpackage is not scoped and does not unwind. If a later path in the same image evaluates the canonical definition, its :import-from fails on symbols the stub never exported, and the error points at the canonical file, which is innocent.</problem>
    <fix>Run stub-defining harnesses in their own process, never sharing an image with canonical loads. When a definition fails that has no business failing, establish first whether the image is fresh: a polluted image produces errors that indict entirely unrelated code. This is the concrete reason behind the fresh-process-per-unit rule in sbcl-usage.</fix>
  </pitfall>
</packages>

<definition_reachability_verification>
  <description>
    A whole class of "loads fine, explodes at call time" failures comes from confusing three
    different things: a symbol existing, a symbol being exported, and a symbol having a binding.
    Package loading cannot detect the gap, and neither can a structural parenthesis check. This
    is the verification ladder that can.
  </description>

  <principle name="export_interns_it_does_not_bind_it">
    <statement>The :export clause of defpackage interns a symbol and marks it external. It establishes no function binding, no value, and no class. A package whose exports name functions that no longer exist loads without complaint; the failure appears later as an undefined-function call, typically at test-image startup, and reads like a load-order problem rather than the deletion it actually is.</statement>
    <how_to_apply>When deleting or replacing a module, audit the retained exports with fboundp (and boundp / find-class for the other namespaces) as an explicit step, and search for every remaining top-level caller. Encode the audit as a test over the package's external symbols so the next deletion is caught mechanically rather than remembered.</how_to_apply>
    <example>
      ;; audit every external symbol of a package for a live function binding
      (loop for sym being the external-symbols of (find-package :my-project)
            unless (or (fboundp sym) (boundp sym) (find-class sym nil))
              collect sym)
      ;; a non-empty result means the package promises names it cannot deliver
    </example>
  </principle>

  <principle name="balance_is_not_nesting_correctness">
    <statement>A structural checker proves the parentheses balance. It does not prove the nesting is what the author meant. A misplaced closing parenthesis can nest two defuns inside a third, or produce a form like (defparameter (defparameter *table* ...)), and the file still reads as valid Lisp. The exported symbols then exist but are not fbound, because the definitions never became top-level forms.</statement>
    <why>Passing a structural check feels like proof, which is exactly what makes this dangerous — the tooling's green result is used as evidence for a property it never examined. Two independent occurrences of this shape were observed in unrelated files.</why>
    <how_to_apply>
      Use the full ladder and stop treating any single rung as sufficient:
      balance check → top-level form outline (does each expected definition appear at depth zero?)
      → fboundp on the expected exports → an actual system load. Structural repair tooling should
      be verified at the outline and fboundp rungs, not just at the balance rung.
    </how_to_apply>
  </principle>
</definition_reachability_verification>

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
    <qualification name="only_when_you_own_the_process">
      This applies to a launcher that owns its process — a test runner, a CLI entry point, a
      coverage script. It does not apply to a bootstrap fragment that a caller loads into an
      already-configured image: asdf:initialize-source-registry replaces the caller's
      configuration rather than extending it, so a bootstrap that calls it silently discards
      whatever the caller set up. A library-side bootstrap should add paths with
      (pushnew path asdf:*central-registry* :test #'equal) and leave the source registry alone.
      Decide which of the two you are writing before choosing the call.
    </qualification>
  </principle>

  <principle name="register_directories_not_trees">
    <statement>Prefer :directory entries naming precise project roots over a broad :tree rooted at a parent checkout. Recursive discovery traverses everything under the root, including unrelated build outputs and, in store-backed environments, root-level result symlinks that lead into an immutable store closure. The symptom is not an error — it is a launcher that appears to hang, or a bootstrap that exceeds its command timeout.</statement>
    <how_to_apply>
      Enumerate the sibling project roots the build actually needs and register each as a
      :directory. Where a checkout can be a linked worktree rather than the primary one, detect
      that case and derive sibling dependency paths from the owning repository root, since the
      worktree directory does not contain them.
    </how_to_apply>
    <example>
      ;; precise: name the roots, do not sweep a parent directory
      (asdf:initialize-source-registry
        `(:source-registry
          (:directory ,(merge-pathnames "proj/"     workspace-root))
          (:directory ,(merge-pathnames "proj-dep/" workspace-root))
          :ignore-inherited-configuration))

      ;; risky: traverses build trees and store symlinks under WORKSPACE-ROOT
      ;; (:tree ,workspace-root)
    </example>
    <note>If a stall survives switching to :directory, the traversal source may be the implementation's own wrapping registry rather than your configuration — see the ASDF plan-layer triage in sbcl-usage.</note>
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

<dependency_change_surface>
  <description>
    Swapping, removing, or externalizing a dependency is not a code change with follow-up chores.
    It is one atomic edit across a fixed set of surfaces, and a partial application leaves the
    system unloadable — ASDF still names components that no longer exist, so the next fresh load
    fails for everyone. Three unrelated codebases independently produced the same surface list,
    which is why it is worth carrying as a checklist rather than rediscovering each time.
  </description>

  <checklist name="surfaces_a_dependency_change_touches">
    <surface>The .asd build manifest: :depends-on of the library system and of the test system, and the :components entries for any deleted files.</surface>
    <surface>The package definitions: :import-from clauses, :export lists, and local-nicknames referencing the departing package.</surface>
    <surface>The dependency lockfile and any pinned revision, so the removed input stops being fetched.</surface>
    <surface>The development shell and source-registry configuration that made the dependency discoverable at all.</surface>
    <surface>CI runner scripts and coverage configuration that load or enumerate the affected systems.</surface>
    <surface>Call sites and test helpers — including helpers that only construct fixtures, which are easy to miss because they compile until the package disappears.</surface>
    <surface>README and changelog claims. A removed dependency that documentation still advertises is a claim the code no longer supports.</surface>
  </checklist>

  <principle name="one_review_unit">
    <statement>Land the whole surface list as a single review and commit unit. Deferring the manifest, the lockfile, or the source-registry entry to a follow-up commit produces an intermediate state in which the system cannot load, which blocks everyone who pulls between the two commits and makes bisection over that range useless.</statement>
    <how_to_apply>Verify the change in a fresh registry and a fresh image, not in the session where you made it — a warm image already has the departing package loaded and will happily resolve symbols that no longer have a source. The documentation surfaces are part of the atomic unit, not a courtesy afterwards.</how_to_apply>
  </principle>
</dependency_change_surface>

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

<read_time_evaluation_load_order>
  <description>
    Read-time evaluation with #. is a legitimate tool for handing a literal to a macro that needs
    it at expansion time. Its cost is routinely under-appreciated: it converts what looks like an
    ordinary data reference into a dependency one phase earlier than compile time, and therefore
    earlier than every intuition about ordering.
  </description>

  <principle name="sharp_dot_creates_a_read_time_dependency">
    <statement>#.+some-table+ is evaluated while the file is being read. The defining unit must therefore be fully loaded before the referencing file is read — not before it is compiled, and not before the form runs. In an ASDF system this makes an ordinary-looking constant reference into a hard :depends-on edge, and getting it wrong surfaces as an unbound-variable error during load rather than as a dependency error.</statement>
    <how_to_apply>Declare the component dependency explicitly whenever a file uses #. against a constant defined elsewhere, and treat the reference as documentation-worthy: a reader coming back later sees a constant name, not a load-order constraint.</how_to_apply>
  </principle>

  <principle name="sharp_dot_cannot_see_later_forms_in_its_own_file">
    <statement>#. can never reference a value defined later in the same file, no matter how far apart the forms are, because the read of the referencing form happens before the defining form has been evaluated. This is a common self-inflicted version of the previous trap.</statement>
    <how_to_apply>Prefer a plain symbol reference for plist and table constants unless the value is genuinely required at read time and is guaranteed to exist then. The plain reference is resolved at run time, costs nothing here, and removes the ordering constraint entirely.</how_to_apply>
    <example>
      ;; read-time: requires the defining unit to be loaded before this file is READ
      (define-strategy foo :parameters #.+foo-parameters+)

      ;; run-time reference: no read-time ordering constraint at all
      (define-strategy foo :parameters +foo-parameters+)
    </example>
  </principle>
</read_time_evaluation_load_order>

<source_file_decomposition_constraints>
  <description>
    The counterpart to the "shrink the compile unit" advice in sbcl-usage. Splitting a large file
    is often the right fix, but the split points are constrained by the grammar rather than by
    taste, and a split that fights those constraints costs more than the file it replaced.
  </description>

  <principle name="every_fragment_must_read_to_completion_alone">
    <statement>In a language whose unit of loading is the file, each fragment must be independently readable: it must contain only complete top-level forms. A single defun continued across a file boundary does not work, and a fragment with one trailing unclosed parenthesis surfaces as a reader end-of-file, not as a helpful structural message.</statement>
    <how_to_apply>Verify each fragment boundary by actually reading or loading the fragment. A whitespace-and-conflict-marker diff check does not detect an unclosed form, and the resulting failure is reported against the fragment that follows, not the one that is broken.</how_to_apply>
  </principle>

  <principle name="retreat_when_boundaries_are_not_stable">
    <statement>Split only where the fragment boundaries are genuinely stable. If achieving a split requires duplicating loader scaffolding across fragments, or cutting through a form, the file wants one cohesive data fragment plus a thin loader rather than N fragments. An observed four-way split of a registry file proved brittle at every boundary and was collapsed back.</statement>
    <why>The signal that a decomposition is wrong is mechanical rather than aesthetic: repeated loader text and forms that resist separation both mean the chosen seams are not real seams in the code's structure.</why>
  </principle>
</source_file_decomposition_constraints>

<atomic_output_and_temporary_files>
  <description>
    Writing a file that readers may observe concurrently, and the temporary-file lifecycle that
    supports it. The general atomic-publish rule belongs to state-transactions; what follows is
    the CL and POSIX mechanics of implementing it correctly.
  </description>

  <principle name="publish_by_rename_within_the_target_directory">
    <statement>Create the temporary file in the target's own parent directory, write it, close and flush the stream, and only then publish it with a rename that overwrites the target. Renaming across filesystems is not atomic and may not even be a rename; keeping the temporary beside the target guarantees both files are on one filesystem so the POSIX rename is a single atomic replacement.</statement>
    <how_to_apply>uiop:rename-file-overwriting-target is the portable form of the publish step. The ordering matters as much as the call: a rename issued before the stream is closed can publish a partially flushed file.</how_to_apply>
  </principle>

  <principle name="failure_deletes_only_the_temporary">
    <statement>If writing or renaming fails, cleanup deletes the temporary file and leaves any existing target untouched. A cleanup path that removes the target as well converts a failed update into data loss — the previous good version is exactly what the caller still needs.</statement>
    <note>Test this invariant by file name or truename, never by raw pathname equality: on macOS the /tmp path a test wrote to canonicalizes to /private/tmp, so a pathname-equality assertion fails on a correct implementation. This is a routine source of platform-only flaky filesystem tests.</note>
  </principle>

  <principle name="retry_only_on_a_confirmed_collision">
    <statement>Open the temporary exclusively with :if-exists nil, and retry only when the resulting file-error is confirmed to be a name collision by probe-file. Every other open failure must escape immediately. Bound the loop with an explicit attempt count and treat exhaustion as a structured operation failure rather than an infinite retry.</statement>
    <why>Without the probe-file confirmation, a permission error or a missing parent directory is retried the full attempt count and then reported as "could not find a free temporary name", which points the investigation at name generation instead of at permissions.</why>
    <example>
      ;; exclusive create; NIL means the name was taken
      (let ((stream (open candidate :direction :output :if-exists nil)))
        (cond (stream stream)
              ((probe-file candidate) :retry)      ; genuine collision
              (t (error 'temp-file-open-failure :path candidate))))
    </example>
  </principle>

  <related_skills>
    <skill name="state-transactions">Owns the general atomic-publish rule; this section is its CL/POSIX implementation</skill>
  </related_skills>
</atomic_output_and_temporary_files>

<numeric_frontend_correctness>
  <description>
    Rules for writing or testing a numeric front end — a parser, a serializer, a converter —
    in Common Lisp. Both rules exist because the obvious reference point is the host
    implementation, and the host implementation is not a specification.
  </description>

  <principle name="the_host_reader_is_not_a_floating_point_oracle">
    <statement>Do not validate a float parser by requiring identity with the implementation's own reader. A reader can be off by one unit in the last place on subnormals and other hard cases, so a differential test using it as the oracle reports failures where the implementation under test is the more accurate of the two. Use an exact rational-to-binary64 computation — or libc strtod — as the reference.</statement>
    <why>Measured case: of 90,041 inputs where both sides produced double-floats, 1,118 differed; every difference was an adjacent subnormal one unit apart, and exact rational distance favored the direct parser in all 1,118. Reading that run as 1,118 bugs would have meant "fixing" the correct implementation to reproduce the reader's error.</why>
    <how_to_apply>Build the oracle from exact arithmetic: parse the decimal into an exact rational, round to nearest with ties to even against the binary64 grid, and compare bit patterns. When two implementations disagree, decide the winner by exact rational distance rather than by which one is the host.</how_to_apply>
  </principle>

  <principle name="enforce_exponent_bounds_before_constructing_anything">
    <statement>Check the exponent against its maximum before any coercion, (expt 10 n), ratio construction, or decimal conversion. Implementations disagree about whether numeric overflow signals at all — one may signal where another returns positive infinity — so overflow detection must never be implemented by catching a condition the implementation might not raise. Worse, a token like an exponent of a billion can exhaust storage during the construction that was supposed to reveal the overflow.</statement>
    <how_to_apply>Order every numeric front end the same way: validate the textual exponent range, then build. The same reasoning covers ratio serialization, where denominator factorization or zero padding must be bounded before it runs rather than after it produces a value.</how_to_apply>
  </principle>
</numeric_frontend_correctness>

<derived_state_and_cache_coherence>
  <description>
    Three linked invariants for any structure that carries derived state — an index, a compiled
    plan, a memoized signature — alongside the data it is derived from. All three failed in
    observed code without raising a single error; the system simply computed against a stale view.
  </description>

  <principle name="every_mutator_must_go_through_the_rebuild">
    <statement>A derived index is only as coherent as the least disciplined mutator. Public mutators that write the underlying collection directly — bypassing the setter that rebuilds the index — leave newly added entries unusable and removed entries still live. Route every mutation through the canonical setter, or make each mutator rebuild explicitly.</statement>
    <how_to_apply>Write the regression test against behavior, not representation: exercise the operation that consumes the index immediately after an add and after a remove. A test that inspects the underlying list passes on exactly the broken code this rule describes, because the list is correct and the index is not.</how_to_apply>
  </principle>

  <principle name="a_revision_counter_is_only_valid_if_nothing_leaks">
    <statement>A revision counter bumped by the container's own mutators is a valid cache key only when every path that can invalidate the cache goes through the container. If the public API hands out the mutable node and edge objects it owns, a caller can mutate one directly and the counter never moves. An O(1) revision fast path therefore requires an ownership design — back-references that make every element setter notify its owning containers — not just a counter.</statement>
    <how_to_apply>Before adopting a revision-counter cache, enumerate what the public API returns. If any returned object is both mutable and part of the cached computation, the counter is unsound and the honest choices are to return copies, to add the back-reference notification, or to keep validating structurally.</how_to_apply>
  </principle>

  <principle name="validity_checks_must_not_use_normalizing_accessors">
    <statement>Write the cache-validity check against the internal raw representation, not the public getters. Public getters commonly normalize on every call — a fresh mapcar, a fresh hash table — so a validity check built on them allocates on the hot path every time it runs and defeats the cache it was added to protect.</statement>
    <why>This is a performance bug that looks like correctness care. The check is right; it is the accessor choice that turns a steady-state O(1) hit into per-element allocation.</why>
  </principle>

  <principle name="never_retain_caller_owned_mutable_strings_as_hash_keys">
    <statement>Common Lisp strings are mutable and equal hashes on content, so retaining a caller-owned string as an equal hash key is a latent orphaning bug: if the caller destructively modifies that string it still owns, the entry becomes unreachable. There is no error — the lookup simply misses, and the entry leaks for the life of the table.</statement>
    <how_to_apply>Copy at key-construction time. Build keys from copied signature strings rather than from the caller's node names or port names, and rebuild them when the existing invalidation detects a change. The fix is one call to copy-seq at the boundary; the diagnosis without it is very expensive.</how_to_apply>
    <example>
      ;; orphaning: the caller still owns NAME and may destructively modify it
      (setf (gethash name table) value)

      ;; safe: the table owns its key
      (setf (gethash (copy-seq name) table) value)
    </example>
  </principle>
</derived_state_and_cache_coherence>

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
    <description>Use resolve-library-id then query-docs for latest documentation.</description>
    <example>
      ;; Get ASDF documentation
      mcp__plugin_claude-code-home-manager_context7__query-docs
        libraryId="/websites/asdf_common-lisp_dev"
        query="defsystem"
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
  <practice priority="critical">Never pass input-derived text as a FORMAT control string; print it as data with ~A against a literal control string</practice>
  <practice priority="high">Audit exported symbols with fboundp after deleting or replacing a module; :export interns a name without binding it</practice>
  <practice priority="high">Enforce class invariants at the construction boundary (initialize-instance :after, or an unexported class), since exporting a class exports make-instance on it</practice>
  <practice priority="high">Validate the raw argument before any normalizing pathname coercion, or the guard becomes unreachable</practice>
  <practice priority="high">Use explicit ASCII predicates for ASCII-defined grammars; digit-char-p and alphanumericp accept Unicode digits from any script</practice>
  <practice priority="high">Land a dependency swap or removal as one commit across manifest, packages, lockfile, dev shell, CI, docs, and call sites</practice>
  <practice priority="high">Publish files by writing a temporary in the target's own directory, closing the stream, then renaming; on failure delete only the temporary</practice>
  <practice priority="medium">Register precise :directory roots in the source registry rather than a broad :tree over a parent checkout</practice>
  <practice priority="medium">Copy caller-owned strings before retaining them as equal hash keys</practice>
  <practice priority="medium">Rebuild derived indexes in every mutator, and write cache-validity checks against raw internals rather than normalizing public getters</practice>
  <practice priority="medium">Validate exponent bounds before any coercion or expt in a numeric front end; do not rely on overflow signalling</practice>
  <practice priority="medium">Prefer a plain symbol reference over #. unless the value is genuinely needed at read time</practice>
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

  <avoid name="data_as_format_control_string">
    <description>Writing an error helper as (error msg) so an already-formatted or input-derived message is interpreted as a FORMAT control string, exposing the full directive language to whoever supplied the text.</description>
    <instead>(error "~A" msg) or a structured condition carrying the text in a slot; audit every FORMAT-family call whose control string is a variable.</instead>
  </avoid>

  <avoid name="export_list_as_proof_of_definition">
    <description>Treating a clean system load as evidence that the exported API exists, when :export only interns names and a misplaced parenthesis can nest definitions so they never become top-level.</description>
    <instead>Run the ladder: balance check, top-level form outline, fboundp over the expected exports, then a real load.</instead>
  </avoid>

  <avoid name="validation_after_normalization">
    <description>Coercing an argument into canonical form and then checking it, so a guard meant to reject one input class can no longer see that class — a safety check silently reduced to a no-op.</description>
    <instead>Assert on the raw argument first, then coerce; be especially careful with pathname coercions, which are lossy about exactly the distinctions guards enforce.</instead>
  </avoid>

  <avoid name="standard_predicates_for_ascii_grammars">
    <description>Implementing an ASCII-defined grammar with digit-char-p / alphanumericp, which accept fullwidth, Arabic-Indic, Devanagari and other Unicode digits, silently widening the accepted language.</description>
    <instead>Define ASCII-only predicates and use them at every stage — start detection, consumption, conversion, framing — with non-ASCII digits as standing regression inputs.</instead>
  </avoid>

  <avoid name="partial_dependency_removal">
    <description>Removing a dependency's code while deferring the manifest, lockfile, source-registry, CI, or documentation updates to a follow-up commit, leaving an intermediate revision in which the system cannot load.</description>
    <instead>Treat the full surface list as one review and commit unit, and verify it in a fresh registry and fresh image.</instead>
  </avoid>

  <avoid name="caller_owned_string_as_hash_key">
    <description>Retaining a string the caller still owns as an equal hash key; destructive mutation by the caller orphans the entry with no error, only a silent lookup miss.</description>
    <instead>Copy the string at key-construction time so the table owns its keys.</instead>
  </avoid>

  <avoid name="mutator_that_bypasses_the_index_rebuild">
    <description>A public mutator writing the underlying collection directly and skipping the setter that rebuilds a derived index, so added entries are unusable and removed entries stay live — with a test that inspects the collection and passes.</description>
    <instead>Route every mutation through the canonical setter, and assert on behavior after add and remove rather than on the underlying list.</instead>
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

<error_escalation>
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
  <skill name="trust-boundaries">General input-validation rules: limits before allocation, validate before normalize — instantiated here for FORMAT and pathname coercion</skill>
  <skill name="state-transactions">General atomic-publish and rollback discipline behind the temporary-file mechanics above</skill>
  <skill name="lisp-macro">Compile-time metaprogramming, code walkers, and source-rewriting correctness</skill>
</related_skills>
