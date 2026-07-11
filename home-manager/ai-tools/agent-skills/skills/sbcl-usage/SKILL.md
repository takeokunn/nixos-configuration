---
name: sbcl-usage
description: Practical SBCL (Steel Bank Common Lisp) operations guide. Use this skill whenever the user mentions SBCL execution/debugging, --script usage, REPL workflows, backtraces, ASDF loading, save-lisp-and-die, profiling, or SLY-based Common Lisp development.
version: 2.1.0
---

<purpose>
  Provide end-to-end operational guidance for SBCL: running programs, debugging failures,
  profiling performance, and producing executables.
  This complements common-lisp-ecosystem by focusing on practical runtime workflows.
</purpose>

<version_info>
  <current_version>SBCL 2.5.x (2026 stable)</current_version>
  <note>All patterns in this skill apply to SBCL 2.x series. SBCL releases frequently (monthly builds); use nix or Roswell to pin a specific version.</note>
</version_info>

<tools>
  <tool>Read - Inspect Lisp/ASDF files, configs, logs</tool>
  <tool>Edit - Update *.lisp / *.asd / run scripts</tool>
  <tool>Bash - Execute sbcl / roswell / qlot / nix commands</tool>
  <tool>mcp__plugin_claude-code-home-manager_context7__query-docs - Verify SBCL / ASDF / CFFI details</tool>
</tools>

<scope>
  <in_scope>
    <item>SBCL invocation mode selection (REPL / script / non-interactive)</item>
    <item>Debugger-centric root cause analysis (backtrace, restarts, inspect, trace)</item>
    <item>ASDF, Quicklisp, Roswell, and Qlot execution workflows</item>
    <item>Performance measurement (time, sb-profile, sb-sprof)</item>
    <item>Executable generation with save-lisp-and-die</item>
    <item>SBCL usage in Nix-based environments</item>
  </in_scope>
  <out_of_scope>
    <item>Full API tutorials for every external library (use Context7 when needed)</item>
    <item>General language architecture topics already covered by common-lisp-ecosystem</item>
  </out_of_scope>
</scope>

<sbcl_cli>
  <description>Primary SBCL startup modes and when to use each</description>

  <pattern name="repl_mode">
    <when_to_use>Interactive exploration and iterative debugging</when_to_use>
    <example>
      sbcl
      sbcl --noinform
    </example>
    <notes>
      <item>Reproduce the failure in REPL first, then minimize the input.</item>
      <item>Use --noinform when you want less startup noise.</item>
    </notes>
  </pattern>

  <pattern name="script_mode">
    <when_to_use>Batch jobs, automation, CI execution</when_to_use>
    <example>
      sbcl --script tools/task.lisp
    </example>
    <notes>
      <item>Design explicit exit codes for operational reliability.</item>
      <item>Wrap top-level failures with handler-case + sb-ext:exit.</item>
    </notes>
  </pattern>

  <pattern name="load_eval_mode">
    <when_to_use>One-liner load-and-run flows in CI or local automation</when_to_use>
    <example>
      sbcl --non-interactive \
        --eval '(require :asdf)' \
        --eval '(asdf:load-system :my-app)' \
        --eval '(my-app:main)'
    </example>
    <notes>
      <item>Prefer --non-interactive in CI to avoid hanging on prompts.</item>
      <item>Move complex startup logic to --script for maintainability.</item>
    </notes>
  </pattern>

  <pattern name="core_control">
    <when_to_use>Custom core image workflows or strict startup control</when_to_use>
    <example>
      sbcl --core my.core
      sbcl --disable-debugger --non-interactive --eval '(...)'
    </example>
    <caution>
      <item>Do not disable debugger during root-cause investigation.</item>
      <item>Use custom core images sparingly to preserve reproducibility.</item>
    </caution>
  </pattern>
</sbcl_cli>

<asdf_workflow>
  <description>ASDF-centered loading and execution patterns</description>

  <pattern name="interactive_load">
    <example>
      (require :asdf)
      (asdf:load-system :my-app)
    </example>
    <notes>
      <item>Validate load-system success before deeper debugging.</item>
      <item>Read the first ASDF failure carefully; avoid chasing secondary errors.</item>
    </notes>
  </pattern>

  <pattern name="test_system">
    <example>
      sbcl --non-interactive \
        --eval '(require :asdf)' \
        --eval '(asdf:test-system :my-app/test)'
    </example>
    <notes>
      <item>Keep a one-line reproducible test command for team sharing.</item>
    </notes>
  </pattern>

  <pattern name="quicklisp_and_qlot">
    <description>Prefer Qlot for dependency reproducibility</description>
    <example>
      qlot install
      qlot exec sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:load-system :my-app)'
    </example>
    <notes>
      <item>Use pinned dependency sets to reduce local-vs-CI drift.</item>
    </notes>
  </pattern>
</asdf_workflow>

<debugging_workflow>
  <description>High-efficiency SBCL debugging flow for fast root-cause discovery</description>

  <phase name="reproduce">
    <objective>Produce a stable minimal reproduction</objective>
    <step order="1">
      <action>Fix the execution mode first (REPL or script)</action>
      <output>Stable execution mode selected</output>
    </step>
    <step order="2">
      <action>Strip inputs/environment to a minimal failing case</action>
      <output>Minimal reproducible failure case</output>
    </step>
  </phase>

  <phase name="observe">
    <objective>Observe failure location, not only symptom text</objective>
    <step order="1">
      <action>Inspect debugger backtrace and stack frames</action>
      <output>Candidate fault location identified</output>
    </step>
    <step order="2">
      <action>Use inspect/describe for problematic objects</action>
      <output>Object state diagnostics captured</output>
    </step>
    <step order="3">
      <action>Use trace for high-value call-path visibility</action>
      <output>Call-path evidence captured</output>
    </step>
    <example>
      (trace my-app::parse-input)
      (untrace my-app::parse-input)
      (describe some-object)
      (inspect some-object)
    </example>
  </phase>

  <phase name="hypothesis">
    <objective>Test one root-cause hypothesis at a time</objective>
    <step order="1">
      <action>Define observable signals per hypothesis</action>
      <output>Hypothesis-to-signal mapping</output>
    </step>
    <step order="2">
      <action>Use step/break/log checks to prove or reject each signal</action>
      <output>Validated or rejected hypotheses</output>
    </step>
  </phase>

  <phase name="fix_and_verify">
    <objective>Apply minimal fix and verify non-regression</objective>
    <step order="1">
      <action>Re-run the same reproduction command after the fix</action>
      <output>Fix effectiveness verified</output>
    </step>
    <step order="2">
      <action>Add tests that preserve the failure case</action>
      <output>Regression protection added</output>
    </step>
  </phase>

  <pattern name="restart_oriented_debugging">
    <description>Use restart flows to keep diagnosing while preserving continuity</description>
    <example>
      (restart-case
          (dangerous-op x)
        (use-default () :report "fallback value" 0)
        (retry () :report "retry operation" (dangerous-op x)))
    </example>
    <why>
      Explicit recovery paths let you observe failures and continue operation
      without blindly swallowing diagnostics.
    </why>
  </pattern>

  <pattern name="debugger_controls">
    <description>Minimum interactive debugger controls to master</description>
    <items>
      <item>Backtrace and frame navigation</item>
      <item>Local variable inspection</item>
      <item>Restart selection (abort/retry/use-value)</item>
      <item>Explicit invoke-debugger usage when needed</item>
    </items>
  </pattern>
</debugging_workflow>

<compile_load_hang_triage>
  <description>
    A distinct class of failures where SBCL stops making progress (no error, no backtrace,
    no output) inside compile-file, load, or asdf:load-system rather than signalling. These
    are compile-unit and load-order phenomena, not ordinary runtime bugs: the same forms
    frequently compile and load fine in isolation but stall when combined in one file or one
    image. Diagnose them structurally, and prefer decomposition over per-form workarounds.
  </description>

  <general_principle name="shrink_the_compile_unit">
    <statement>Treat the compile unit (the file handed to compile-file, or a single ASDF component) as the primary variable. Splitting a stalling file into smaller source files loaded serially is the durable fix; per-form workarounds are stopgaps.</statement>
    <why>Many stalls arise from compile-time interaction between top-level forms in the same unit — macro-generation feeding later macro invocations, large constant folding, or definition ordering — not from any single form. Reducing the unit removes the interaction.</why>
  </general_principle>

  <general_principle name="keep_top_level_forms_boring">
    <statement>Define top-level helpers with plain defun rather than a top-level (setf (symbol-function 'name) (lambda ...)) or an eager (compile nil (lambda ...)) at registration time. Keep constant-heavy work inside runtime helper functions instead of thin top-level wrappers that invite constant folding of large literals.</statement>
    <why>Top-level symbol-function assignment of a full lambda body, and thin wrappers that return a large constant vector/list, have been observed to trigger compile/eval stalls where the equivalent plain defun (or a non-constant construction path) loads normally.</why>
    <scope>Specific triggers observed on SBCL 2.6.0 (macOS/Nix); the general guidance — keep top-level forms simple and side-effect-light — is dialect-stable.</scope>
    <example>
      ;; risky at top level: symbol-function assignment of a full lambda body
      (setf (symbol-function '%encode) (lambda (s) #| large body |#))
      ;; safe: plain defun
      (defun %encode (s) #| large body |#)

      ;; risky: a thin wrapper that folds a large constant vector at compile time
      (defun tokens () #(#| hundreds of literal specs |#))
      ;; safe: build the vector at runtime through a non-constant argument path
      (defun tokens (specs) (build-token-vector specs))

      ;; drop unneeded generated copiers that enlarge a defstruct-heavy compile unit
      (defstruct (node (:copier nil)) a b c)
    </example>
  </general_principle>

  <general_principle name="watch_macro_expansion_size">
    <statement>A macro whose expansion grows combinatorially with its arguments can make macroexpand/compile appear hung. Emit a linear runtime construction instead of enumerating a branch per argument subset, and add a macroexpansion-size regression test for high-arity call sites.</statement>
    <why>Observed case: a keyword-wrapper macro emitting one direct-call branch per &amp;key presence subset produced on the order of 2^N branches for N keys, so a wrapper with ~18 keys generated hundreds of thousands of branches. The load stall was macroexpansion blow-up, not the wrapped function.</why>
  </general_principle>

  <general_principle name="load_order_is_a_variable">
    <statement>A file that compiles alone can stall when compiled after another file has been loaded into the same image. When a stall appears only in-sequence, suspect load-order/compile-unit interaction and re-verify each unit in a fresh image.</statement>
    <why>Observed with definition-heavy files (e.g. a run of many defstruct forms) that compiled in a fresh image but stalled once an earlier file had been loaded first — evidence the trigger is cross-unit state, not the file's own source.</why>
  </general_principle>

  <observed_triggers>
    <note>Version-scoped field observations (SBCL 2.6.0), recorded as reproduction conditions rather than universal rules. Use them as hypotheses to test, not guarantees, and re-validate before relying on any workaround.</note>
    <trigger>A run of many top-level defstruct forms in one compile unit; adding one more struct crosses a threshold and compile-file stalls. Mitigations: split structs across serially-loaded files; add (:copier nil) to drop unneeded generated copiers.</trigger>
    <trigger>A thin top-level wrapper returning a large constant vector/list, folded at compile time. Mitigation: build the vector inside a runtime helper or pass the specs through a non-constant argument path.</trigger>
    <trigger>Top-level (setf (symbol-function 'name) (lambda ...)) with a substantial body. Mitigation: plain defun.</trigger>
    <trigger>Predicates branching on implementation Unicode category/width tables via member/case under a bootstrap-loaded image. Mitigation observed: bind the return value and compare with explicit eq/or checks.</trigger>
    <trigger>Forcing sb-ext:*evaluator-mode* to :interpret across a whole file to dodge a compile stall — frequently just relocates the stall to a later file or to execution time. Treat evaluator-mode guards as a dead end for a durable fix unless paired with a structural decomposition and fresh verification.</trigger>
  </observed_triggers>
</compile_load_hang_triage>

<headless_verification_harness>
  <description>
    A sound, non-interactive harness is a prerequisite for diagnosing the stalls above: if the
    timeout mechanism is unsound, a stalled form and a stalled harness are indistinguishable,
    producing false positives. Build the harness correctly before trusting any hang observation.
  </description>

  <principle name="real_subprocess_timeout">
    <statement>The timeout must run in a parent process that keeps the ability to kill the child. A wrapper that arms an alarm and then exec's SBCL replaces itself with SBCL and cancels the alarm — the timeout never fires, so an ASDF/load hang survives indefinitely and looks like a stalled form. Use fork + wait in the parent, with the parent owning the alarm and the kill.</statement>
    <example>
      # Perl fork/wait timeout skeleton: the parent keeps the alarm and can signal the child.
      # (exec-after-alarm in a single process would silently cancel the alarm.)
      perl -e '
        my $pid = fork();
        if ($pid == 0) { setpgrp(0,0); exec @ARGV or die; }
        local $SIG{ALRM} = sub { kill "KILL", -$pid; exit 124; };
        alarm($ENV{TIMEOUT} || 60);
        waitpid($pid, 0);
        exit($? >> 8);
      ' -- sbcl --script run.lisp
    </example>
  </principle>

  <principle name="kill_the_process_group">
    <statement>Signal the child's process group, not just the wrapper's parent PID. A child that has called setpgid/setpgrp is orphaned (not reaped) if only the parent is killed, and keeps holding resources. Put the child in its own group and send TERM/KILL to the group, or let the wrapper live to its deadline and reap the child.</statement>
  </principle>

  <principle name="deterministic_child_flags">
    <statement>Launch every verification child with a fixed, minimal, non-interactive flag set so results are reproducible and cannot drop into the interactive debugger.</statement>
    <items>
      <item>--disable-debugger — never enter the interactive debugger in automation. This does not contradict the root-cause rule: disable it in the batch harness, keep it enabled while actively investigating a single failure interactively.</item>
      <item>--no-sysinit --no-userinit — ignore site/user init files so the child does not inherit local state.</item>
      <item>Exit with a fully qualified (sb-ext:exit ...) / (sb-ext:quit); an unqualified (quit) can become unsafe after package changes during ASDF loading.</item>
    </items>
    <example>
      sbcl --no-sysinit --no-userinit --disable-debugger \
           --eval '(require :asdf)' \
           --load run-one-unit.lisp \
           --eval '(sb-ext:exit :code 0)'
    </example>
  </principle>

  <principle name="fresh_process_per_unit">
    <statement>Run each file/test in a fresh SBCL process rather than many units in one long-lived image. This both avoids cross-unit compile-state interaction and prevents one stalled unit from blocking the rest.</statement>
    <why>Whole-suite single-process runs have been observed to hang at function/test boundaries even when each unit passes alone; per-unit fresh processes (chunk size 1) is the stable path. The isolation must be complete — a bootstrap step that itself calls compile-file in the long-lived process defeats a per-file subprocess strategy.</why>
  </principle>

  <principle name="isolate_the_fasl_cache">
    <statement>Give each run a private, initialized output-translations / cache root before asdf:load-system. Parallel processes sharing an inherited default FASL cache can race and fail with "Failed to find the TRUENAME of ...fasl". Initialize output translations in the launcher itself, and set a fresh HOME/XDG_CACHE_HOME when reproducing in isolation.</statement>
  </principle>

  <caveat name="timeout_threshold_vs_contention">
    <statement>Distinguish a genuine per-file stall from ambient machine contention. When many SBCL sessions run concurrently, baseline load latency can exceed a low per-file timeout and report every file as a timeout. Raise the threshold or reduce concurrency before attributing blame to any single file.</statement>
  </caveat>
</headless_verification_harness>

<form_bisect_and_package_preflight>
  <description>Techniques for pinning the exact offending form once a stall is confirmed, and for
  keeping the reproducer itself from manufacturing false failures.</description>

  <principle name="bisect_by_form_not_by_line">
    <statement>When narrowing which top-level form stalls compile/load, slice by complete top-level forms, never by raw line ranges. A line-range slice can cut through the middle of a form and produce malformed Lisp that fails to read, masquerading as the original stall (e.g. INPUT-ERROR-IN-LOAD).</statement>
    <how_to_apply>Use a read/eval form-trace: read one top-level form at a time, log its head before evaluating and log completion after, and stop on the first form that logs a head but never completes. This respects form boundaries and pinpoints the offending form directly.</how_to_apply>
    <example>
      ;; streaming form-trace: reader sees each in-package before it reads the next form,
      ;; and the last "head:" without a matching "done:" names the stalling form.
      (with-open-file (in path)
        (loop for form = (read in nil :eof)
              until (eq form :eof)
              for head = (and (consp form) (car form))
              do (format *error-output* "~&head: ~S~%" head)
                 (finish-output *error-output*)
                 (eval form)
                 (format *error-output* "~&done: ~S~%" head)))
    </example>
  </principle>

  <principle name="reader_intern_timing_and_package_preflight">
    <statement>The reader interns every symbol in the current package at read time, before an in-package in the same batch takes effect. Reading a whole file (or a whole --eval) into a list of forms first, then evaluating, interns later symbols in the wrong package and can make package-local functions look undefined — a false failure unrelated to the code under test.</statement>
    <how_to_apply>
      Keep package creation, package switch, and definitions as separate top-level evaluations
      (or stream forms so the reader sees in-package before it reads later forms). When a child
      process receives a test/symbol name via environment variable or argument, read or resolve
      it in the target package, not in CL-USER — otherwise it interns into COMMON-LISP-USER and
      the dispatch can miss or hang at the boundary.
    </how_to_apply>
  </principle>

  <principle name="minimal_reproducer_hygiene">
    <statement>Before trusting a "hang", rule out defects in the reproducer: an unbalanced paren in a probe loader can leave a form open so later defuns never become top-level, and a package-mismatched read can fake a missing-symbol error. A malformed harness produces false hangs.</statement>
  </principle>
</form_bisect_and_package_preflight>

<coverage_measurement_bias>
  <principle name="sb_cover_definition_bias">
    <statement>sb-cover reports low expression coverage for files dominated by top-level defining forms and metadata side effects (defpackage, define-condition, top-level documentation/table assignments), even when the runtime contracts they establish are fully tested. These forms are counted as expressions but are not all attributed as executed by ordinary test runs.</statement>
    <how_to_apply>Separate genuine runtime gaps from instrumentation bias by comparing a low-coverage file against its shape: definition-heavy files may warrant a few explicit contract tests but need not reach 100%; logic-heavy files are the higher-value targets for additional tests or refactoring. Do not distort public API design solely to satisfy sb-cover on top-level metadata; prefer explicit tests plus a documented exception.</how_to_apply>
    <note>sb-cover does not clean its own HTML output directory; after splitting or renaming source files, clear the stale report before reading a new one.</note>
  </principle>
</coverage_measurement_bias>

<performance_profiling>
  <description>Standard SBCL performance workflow</description>

  <pattern name="quick_timing">
    <example>
      (time (my-app:run-once input))
    </example>
    <notes>
      <item>Start with time before introducing complex profiling.</item>
    </notes>
  </pattern>

  <pattern name="deterministic_profile">
    <example>
      (require :sb-profile)
      (sb-profile:profile my-app::hot-fn my-app::other-hot-fn)
      (my-app:run-benchmark)
      (sb-profile:report)
      (sb-profile:unprofile)
    </example>
    <notes>
      <item>Identify hot functions at call-site granularity.</item>
    </notes>
  </pattern>

  <pattern name="statistical_profile">
    <example>
      (require :sb-sprof)
      (sb-sprof:with-profiling (:max-samples 3000 :report :flat)
        (my-app:run-benchmark))
    </example>
    <notes>
      <item>Use when you need lower overhead and broad execution trends.</item>
    </notes>
  </pattern>

  <pattern name="optimization_policy">
    <description>Apply optimization declarations locally and verify impact</description>
    <example>
      (declaim (optimize (speed 3) (safety 1) (debug 1)))
      (defun hot (x y)
        (declare (type fixnum x y))
        (+ x y))
    </example>
    <caution>
      <item>Avoid safety 0 unless you have hard evidence and strong tests.</item>
    </caution>
  </pattern>
</performance_profiling>

<build_and_release>
  <description>Executable image generation baseline</description>

  <pattern name="save_lisp_and_die">
    <example>
      (defun main ()
        (handler-case
            (progn
              (my-app:run)
              (sb-ext:exit :code 0))
          (error (e)
            (format *error-output* "fatal: ~a~%" e)
            (sb-ext:exit :code 1))))

      (sb-ext:save-lisp-and-die "my-app"
        :toplevel #'main
        :executable t
        :compression t)
    </example>
    <notes>
      <item>Always define explicit process exit codes.</item>
      <item>Validate ASDF load and tests before image generation.</item>
    </notes>
  </pattern>
</build_and_release>

<ecosystem_integration>
  <sly>
    <description>In this environment, prefer SLY over SLIME</description>
    <notes>
      <item>Assume sly / sly-asdf / sly-macrostep workflows for Emacs integration.</item>
      <item>When explaining editor actions, provide SLY-compatible guidance.</item>
    </notes>
  </sly>

  <nix>
    <description>Reproducible SBCL execution in Nix environments</description>
    <example>
      nix shell nixpkgs#sbcl
      sbcl --version
    </example>
    <notes>
      <item>Pin project environments via shell.nix or flake.nix when needed.</item>
      <item>Combine with Qlot for stronger dependency reproducibility.</item>
    </notes>
  </nix>

  <roswell>
    <description>Simplify implementation management and script execution</description>
    <example>
      ros install sbcl
      ros run
      ros build app.ros
    </example>
  </roswell>
</ecosystem_integration>

<decision_tree name="execution_mode_selection">
  <question>Which run mode should be selected?</question>
  <if condition="Need interactive diagnosis and rapid iteration">REPL mode (sbcl / --noinform)</if>
  <if condition="Need CI-safe, non-interactive execution">--non-interactive + --eval/--script</if>
  <if condition="Need a distributable single executable">save-lisp-and-die executable flow</if>
  <if condition="Need stable dependencies across machines">Qlot + Nix (and Roswell when useful)</if>
</decision_tree>

<best_practices>
  <practice priority="critical">Do not use --disable-debugger during root-cause analysis; capture backtraces first.</practice>
  <practice priority="critical">Keep one reproducible command line for before/after fix verification.</practice>
  <practice priority="high">Use trace/inspect/describe to convert assumptions into observable evidence.</practice>
  <practice priority="high">Require measurement (time/sb-profile/sb-sprof) before performance changes.</practice>
  <practice priority="high">Adopt Qlot for projects that need deterministic dependency state.</practice>
  <practice priority="medium">Design batch jobs with explicit success/failure exit codes.</practice>
  <practice priority="medium">Avoid SLIME-only advice in SLY-based environments.</practice>
  <practice priority="critical">Verify every hang observation with a sound fork/wait timeout; an exec-after-alarm wrapper never fires and fabricates false stalls.</practice>
  <practice priority="high">Decompose a stalling compile unit into smaller serially-loaded files rather than reaching for per-form workarounds or whole-file evaluator-mode guards.</practice>
  <practice priority="high">Bisect compile/load stalls by complete top-level forms, never by line ranges; use a read/eval form-trace to name the offending form.</practice>
  <practice priority="medium">Run verification one unit per fresh SBCL process (--no-sysinit --no-userinit --disable-debugger) with an isolated FASL cache.</practice>
</best_practices>

<anti_patterns>
  <avoid name="disable_debugger_too_early">
    <description>Disabling debugger before diagnosis removes critical evidence.</description>
    <instead>Use interactive debugger state first (frames, restarts, object inspection).</instead>
  </avoid>

  <avoid name="optimize_without_measurement">
    <description>Applying optimization declarations without evidence.</description>
    <instead>Measure hotspots first; optimize only proven bottlenecks.</instead>
  </avoid>

  <avoid name="error_swallowing">
    <description>Swallowing errors with handler-case and hiding root cause.</description>
    <instead>Preserve diagnostic visibility with logging/rethrow/restart strategies.</instead>
  </avoid>

  <avoid name="non_reproducible_dependency_state">
    <description>Allowing per-machine dependency drift.</description>
    <instead>Use Qlot (and Nix when appropriate) to lock execution context.</instead>
  </avoid>

  <avoid name="exec_after_alarm_timeout">
    <description>Arming a timeout alarm and then exec'ing SBCL in the same process, which cancels the alarm so the timeout never fires.</description>
    <instead>Fork the child and wait in the parent; the parent owns the alarm and kills the child's process group on expiry.</instead>
  </avoid>

  <avoid name="line_range_bisect">
    <description>Narrowing a compile/load stall by slicing raw line ranges, which can cut through a form and produce malformed Lisp that fails to read.</description>
    <instead>Bisect by complete top-level forms via a read/eval form-trace that stops on the first form that starts but never completes.</instead>
  </avoid>

  <avoid name="evaluator_mode_guard_as_fix">
    <description>Forcing sb-ext:*evaluator-mode* :interpret over a file to dodge a compile stall and treating it as a durable fix.</description>
    <instead>Use it only as a scoped diagnostic; the durable fix is a structural decomposition of the compile unit plus fresh verification.</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Prioritize observability (backtrace, inspect, trace) during root-cause analysis.</rule>
  <rule>Fix proposals must include reproduction, verification, and failure-mode behavior.</rule>
  <rule>Performance recommendations must be grounded in measured data.</rule>
  <rule>Target SBCL 2.5+ (2.x series); use sb-ext:exit for process exit, not cl:exit or os-exit directly</rule>
</rules>

<rules priority="standard">
  <rule>Confirm ASDF load viability before diving into deeper implementation details.</rule>
  <rule>Select execution mode explicitly from task constraints.</rule>
  <rule>Use save-lisp-and-die with explicit exit-code policy for operational binaries.</rule>
</rules>

<workflow>
  <phase name="triage">
    <objective>Reproduce and record the failure</objective>
    <step order="1">
  <action>1. Choose run mode (REPL / script / non-interactive)</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Build a minimal reproduction</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Capture backtrace and input conditions</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>

  <phase name="debug">
    <objective>Narrow and validate root-cause hypotheses</objective>
    <step order="1">
  <action>1. Observe state with inspect / describe / trace</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Define recovery paths using restarts</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Test one hypothesis at a time</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>

  <phase name="validate">
    <objective>Confirm fix quality and regression safety</objective>
    <step order="1">
  <action>1. Re-run the exact reproduction command</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Run asdf:test-system</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Profile if performance side effects are possible</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor SBCL warning during compilation</example>
    <example severity="medium">Unhandled condition or ASDF load failure</example>
    <example severity="high">Runtime error or heap exhaustion in production image</example>
    <example severity="critical">Memory corruption or undefined behavior in FFI boundary</example>
  </examples>
</error_escalation>

<constraints>
  <must>Debugging guidance must preserve the sequence: reproduce → observe → verify</must>
  <must>Keep SLY compatibility in editor integration guidance</must>
  <must>Provide Nix/Qlot reproducibility guidance when environment drift is likely</must>
  <avoid>Unmeasured optimization</avoid>
  <avoid>Layering workaround code without identifying root cause</avoid>
</constraints>

<related_skills>
  <skill name="common-lisp-ecosystem">CLOS/ASDF/condition-system foundations</skill>
  <skill name="nix-ecosystem">Pinned SBCL runtime environments with nix shell/flake</skill>
  <skill name="investigation-patterns">Evidence-driven root-cause methodology</skill>
  <skill name="quality-tools">Automated checks and CI quality discipline</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate code patterns and references in this skill domain</agent>
  <agent name="quality-assurance">Review implementation quality against this skill guidance</agent>
  <agent name="code-quality">Analyze code complexity and suggest refactoring improvements</agent>
</related_agents>
