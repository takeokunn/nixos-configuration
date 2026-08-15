---
name: sbcl-usage
description: Use for SBCL execution and debugging — --script usage, REPL workflows, backtraces, ASDF loading, save-lisp-and-die, profiling, SLY development, terminating an unresponsive SBCL, or sb-thread/sb-cover hazards. Complements common-lisp-ecosystem's runtime operations.
version: 2.5.0
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
    <trigger>A large defun whose small helper is a candidate for open-coding. Adding (declaim (notinline %helper)) has been observed to let a runner load past the stalling boundary, which points at inlining as the trigger rather than the helper's own body. Treat it as a diagnostic that localises the cause, not as the fix — the durable answer is still to shrink the compile unit.</trigger>
    <trigger>Unresolved forward references in a definition-heavy file. A top-level (declaim (ftype function ...)) for the remaining forward references has cleared a load stall that no per-form change reached.</trigger>
  </observed_triggers>

  <in_image_timeouts_do_not_guard_this>
    <statement>sb-ext:with-timeout does not reliably interrupt the compiler, so an in-image timeout is not a valid guard against a compile or load stall. The timeout simply never fires and the session hangs exactly as it would have without it.</statement>
    <why>This is why the subprocess harness is mandatory rather than merely convenient — see headless_verification_harness. A stall has to be bounded from outside the image, by a process-level timeout with a kill grace, because the process being bounded may be in a state where nothing inside it can run.</why>
  </in_image_timeouts_do_not_guard_this>
</compile_load_hang_triage>

<asdf_plan_layer_hang_triage>
  <description>
    The sibling failure family to compile_load_hang_triage: a stall inside asdf:load-system
    that never reaches your code at all. ASDF's operation/plan layer — system-definition
    discovery, source-registry flattening, plan computation — runs before the first form of
    the target system is compiled, and it can hang there. Rule the environment out before
    spending any time bisecting project sources, because every technique in the previous
    section assumes the stall is in a compile unit you own.
  </description>

  <principle name="load_system_asdf_as_a_control">
    <statement>Run (asdf:load-system "asdf") as a control experiment. ASDF registers itself as a system, so loading it exercises the same find-system/operate machinery with none of your project's code in it. If (require :asdf) succeeds but (asdf:load-system "asdf") never returns, the fault is environmental and no amount of file-level bisection will find it.</statement>
    <how_to_apply>
      Probe the layers cheapest-first, each in a fresh timeout-bounded child process, and stop at
      the first one that hangs: (require :asdf) → (asdf:load-system "asdf") → (asdf:find-system "proj" nil)
      → (asdf:load-asd #p"/abs/path/proj.asd") → (asdf:operate 'asdf:load-op "proj"). The layer that
      stalls names the fault. A plain (load "src/file.lisp") that returns promptly while find-system
      hangs is direct evidence the stall is in discovery, not in the source.
    </how_to_apply>
    <why>Observed on Darwin/Nix with ASDF 3.3.7: (require :asdf) returned, then load-system, load-asd, find-system, and operate all hung after system-definition discovery, while direct load registered the same system immediately. Without the control experiment this reads as "our project hangs on load".</why>
  </principle>

  <principle name="ignore_inherited_configuration_does_not_disable_the_wrapper">
    <statement>:ignore-inherited-configuration suppresses inherited user and system source-registry configuration, but it does not bypass the implementation's wrapping source registry. SBCL's wrapping configuration recursively registers the implementation directory, so a blocked descriptor somewhere under the SBCL contrib tree can stall registry flattening even when your own configuration is fully explicit.</statement>
    <how_to_apply>When registry flattening is the suspect, inspect open descriptors of the stalled process (lsof/fs_usage on Darwin, /proc/PID/fd on Linux) rather than re-reading your configuration. A descriptor pinned inside the implementation's own contrib directory confirms the wrapper, not your project, is the traversal source. Reaching for a narrower :directory instead of a :tree does not help here either, because the wrapper is added independently of your entries.</how_to_apply>
    <scope>Mechanism observed with ASDF 3.3.7 on an SBCL provisioned through a store-backed package manager; treat it as a hypothesis to confirm by descriptor inspection, not as a universal ASDF property.</scope>
  </principle>

  <principle name="interrupt_disabled_regions_need_sigkill">
    <statement>A stall can sit inside a Lisp interrupt-disabled region, where SIGALRM and SIGTERM are deferred indefinitely. An in-image timeout, a handler-based deadline, and a TERM-only external watchdog all fail silently against it: the deadline "fires" and nothing happens.</statement>
    <how_to_apply>Every watchdog over an ASDF load must escalate to SIGKILL after a grace period, and must report which signal actually ended the child. If a process survived TERM and needed KILL, that fact is itself evidence about where it was stuck — record it alongside the stall.</how_to_apply>
    <note>This is the operational reason the harness below mandates an external parent process: an in-image timeout cannot be trusted to bound a load it is running inside.</note>
  </principle>
</asdf_plan_layer_hang_triage>

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

  <principle name="bound_timeout_with_a_kill_grace">
    <statement>When the shell-level equivalent is coreutils timeout(1), always pass a kill grace: timeout --foreground -k 10s &lt;limit&gt;s &lt;command&gt;. Plain timeout sends only TERM, and SBCL can remain alive after its initial termination signal, so a nominally bounded run leaks past the job budget and the escaped child keeps holding the FASL cache and any ports it opened.</statement>
    <why>Same root cause as the interrupt-disabled-region principle above: the first signal is a request, not a guarantee. -k converts the request into a bound by following up with KILL after the grace period.</why>
    <how_to_apply>Set the grace long enough for an orderly exit to complete (a few seconds is usually ample) but short enough that the total — limit plus grace — still fits inside the enclosing CI step timeout. Budget the outer timeout against limit + grace, not limit.</how_to_apply>
    <example>
      # bounded: TERM at the limit, KILL 10s later if the child is still alive
      timeout --foreground -k 10s 300s \
        sbcl --no-sysinit --no-userinit --disable-debugger --script run-tests.lisp
    </example>
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

<subprocess_process_group_contract>
  <description>
    What sb-ext:run-program actually guarantees about the child's process group, and why
    "I can kill the whole subprocess tree" is silently false for one specific input mode.
    This matters for any library that spawns a pipeline and promises cancellation or cleanup:
    the promise holds for most call sites and breaks for one, so it passes casual testing.
  </description>

  <principle name="inherited_stdin_suppresses_the_child_process_group">
    <statement>run-program only puts the child in its own process group when the child's input descriptor is a real (nonnegative) descriptor. With :input t — inherited stdin — SBCL prepares the descriptor as -1 and the forked child calls tcsetpgrp instead of creating a new group, so the child stays in the caller's process group. Every other supported input mode (nil, a stream, a pathname, :stream) takes the nonnegative path and does create the group: setpgid(0, getpid()) on Darwin, setpgrp() on Linux.</statement>
    <why>The dangerous half is not that the group is missing, but that a later kill of the "child's group" then targets the caller's own group. A cancellation routine written against the common case will signal the Lisp process itself the first time someone passes :input t.</why>
    <how_to_apply>
      Do not infer the process group from the spawn arguments. Verify it after spawn — compare
      sb-posix:getpgid of the child pid against the pid itself — and store the verified pgid in
      an opaque handle alongside the process. Public signal APIs consume the handle, never a
      caller-supplied pid. If verification fails, degrade to single-process signalling and say so
      in the handle rather than pretending group cancellation is available.
    </how_to_apply>
    <scope>Descriptor and syscall details observed on POSIX SBCL 2.6.x. The verify-then-record remedy is portable regardless of how a given release wires the modes.</scope>
  </principle>

  <principle name="a_saved_pgid_expires_with_its_leader">
    <statement>A saved pgid is only authorization to signal while the group leader is alive. Once the leader has been reaped, the kernel is free to reuse that pid and pgid, so a later kill(-pgid, signal) can land on an unrelated process group. Treat a terminal leader as revoking the handle.</statement>
    <how_to_apply>Public group-signal entry points must reject a handle whose leader has already reached a terminal state, rather than "cleaning up anyway". Best-effort cleanup paths that fire after reaping are exactly where reuse bites, so gate them on the same check.</how_to_apply>
  </principle>

  <principle name="distinguish_eperm_from_esrch">
    <statement>Cleanup code must distinguish the two failure modes of a group signal: ESRCH means no such group, so the target is genuinely gone and the cleanup succeeded; EPERM means the group exists but is not signalable by this process, so the target is still running and the cleanup failed. Collapsing both into "kill failed, ignore" silently converts a leaked process tree into a clean shutdown report.</statement>
    <how_to_apply>Return ESRCH as success from a reaper, and escalate EPERM as a real error carrying the pgid. This is POSIX-general and applies equally to a shell wrapper checking kill's exit status.</how_to_apply>
  </principle>
</subprocess_process_group_contract>

<threading_contracts>
  <description>
    In-process concurrency contracts that differ from the textbook expectation, plus the lock
    discipline that keeps a worker pool from deadlocking on its own error path. common-lisp-ecosystem
    shows make-thread and with-mutex as basic forms; these are the caveats that apply once the
    code actually has contention.
  </description>

  <principle name="condition_wait_with_timeout_may_return_without_the_mutex">
    <statement>sb-thread:condition-wait with :timeout may return without having reacquired the mutex, when reacquisition itself cannot complete before the deadline expires. This violates the usual condition-variable contract — that the wait always returns holding the lock — and the damage surfaces later: exiting the surrounding sb-thread:with-mutex signals a mutex ownership error at a frame that has nothing to do with the timeout.</statement>
    <why>The symptom is maximally misleading. Nobody reads "not the owner of the mutex" at a with-mutex exit as "a condition-wait timeout three lines up returned early", so the investigation starts in the wrong place.</why>
    <how_to_apply>
      Do not use :timeout to implement blocking semantics. Implement a blocking operation as a
      timeout-free predicate loop — wait, re-test the predicate, wait again — and make every state
      change that can satisfy the predicate signal the condition variable explicitly. That includes
      the non-obvious ones: a dispatcher freeing capacity must wake blocked producers, and a
      cancellation that changes the predicate must wake them too, or the loop sleeps through the
      event it was waiting for.
    </how_to_apply>
    <example>
      ;; blocking enqueue without :timeout — the predicate loop is the contract
      (sb-thread:with-mutex (lock)
        (loop until (or cancelled (&lt; count capacity))
              do (sb-thread:condition-wait space-available lock))
        (unless cancelled (push item queue) (incf count)))

      ;; every predicate-changing site must wake the waiters, including cancellation
      (sb-thread:with-mutex (lock)
        (setf cancelled t)
        (sb-thread:condition-broadcast space-available))
    </example>
  </principle>

  <principle name="never_call_a_user_callback_under_the_state_lock">
    <statement>Update the shared state while holding its mutex, release the mutex, and only then invoke the user callback. If the callback must be recorded as having failed, reacquire the mutex after it unwinds. Invoking a callback under the state lock hands arbitrary user code the power to block all state synchronization, and — the failure people actually hit — deadlocks on a recursive lock attempt when the callback signals and the handler tries to record the condition in the same state.</statement>
    <why>The deadlock arrives through the error-recording path, not the happy path. Every test with a well-behaved callback passes; the first callback that signals hangs the pool. That asymmetry is why this survives review.</why>
    <how_to_apply>Apply the same rule to any outward call from under a lock: joining a dispatcher thread, calling a logging hook, signalling a condition whose handler is user-supplied. The invariant is "no lock is held across a call whose implementation the module does not own."</how_to_apply>
    <example>
      ;; state mutation under the lock; the callback strictly outside it
      (let ((snapshot nil))
        (sb-thread:with-mutex (task-lock)
          (setf (task-state task) :finished)
          (setf snapshot (task-result task)))
        (handler-case (funcall (task-callback task) snapshot)
          (error (c)
            (sb-thread:with-mutex (task-lock)
              (setf (task-callback-error task) c)))))
    </example>
  </principle>
</threading_contracts>

<coverage_measurement_bias>
  <principle name="sb_cover_definition_bias">
    <statement>sb-cover reports low expression coverage for files dominated by top-level defining forms and metadata side effects (defpackage, define-condition, top-level documentation/table assignments), even when the runtime contracts they establish are fully tested. These forms are counted as expressions but are not all attributed as executed by ordinary test runs.</statement>
    <how_to_apply>Separate genuine runtime gaps from instrumentation bias by comparing a low-coverage file against its shape: definition-heavy files may warrant a few explicit contract tests but need not reach 100%; logic-heavy files are the higher-value targets for additional tests or refactoring. Do not distort public API design solely to satisfy sb-cover on top-level metadata; prefer explicit tests plus a documented exception.</how_to_apply>
    <note>sb-cover does not clean its own HTML output directory; after splitting or renaming source files, clear the stale report before reading a new one.</note>
  </principle>

  <principle name="coverage_instrumentation_is_process_global">
    <statement>SB-COVER counters live in process-global mutable state. Running the suite across concurrent workers in one image produces nondeterministic per-file undercounts while every test still passes, so the coverage number moves run to run for reasons unrelated to the tests. Run coverage single-worker even when the ordinary suite runs in parallel.</statement>
    <how_to_apply>Treat the coverage run as a distinct execution mode with its own runner settings, not as the normal run with a flag added. If parallelism is needed for suite runtime, keep it in the correctness run and accept a slower serial coverage run.</how_to_apply>
  </principle>

  <principle name="load_instrumented_sources_through_the_build_system">
    <statement>After resetting SB-COVER, load the system under measurement through (asdf:load-system :proj :force t). Manually compiling and loading copied sources detaches the counters from the source identity SB-COVER reports against, and the affected files come back as a confident 0% instead of an error.</statement>
    <why>A 0% file reads as "untested" and sends people to write tests for code that is already covered. The distinguishing symptom is that the 0% files are exactly the ones the runner handled specially — a copy step, a staging directory, a hand-rolled compile loop.</why>
  </principle>

  <principle name="gate_coverage_against_a_source_manifest">
    <statement>An aggregate percentage is computed over the files that appear in the report, so it says nothing about files that never made it in. A report showing 100% across nine files when the system has twelve is still 100%. The gate must compare normalized report source filenames against a declared manifest of production components and reject the run when a row is missing, malformed, or has a zero total, before it accepts the percentage at all.</statement>
    <how_to_apply>Derive the manifest from the ASDF component list rather than a hand-maintained second list, so a newly added component is covered by the gate on the commit that adds it. Normalize both sides (truename, case, store-path prefixes) before comparing, or the check fails open on path formatting alone.</how_to_apply>
    <note>The manifest rule is language-neutral and applies to any coverage or lint report consumed as a gate; the SB-COVER specifics above are what make it easy to lose rows here.</note>
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

  <note name="methodology_lives_elsewhere">
    The patterns above are tool invocations: how to obtain a number from SBCL. They do not tell you
    whether the number means anything. Paired A/B protocols, warmup and full-GC discipline, measuring
    the noise floor before claiming a delta, gating on a confidence interval rather than a point
    estimate, and proving you are measuring your working tree rather than a pre-registered store build
    all belong to performance-benchmarking. Consult it before reporting any before/after comparison.
  </note>
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
  <practice priority="high">Run (asdf:load-system "asdf") as a control before blaming a project file for an ASDF hang; it separates an environment-level plan-layer stall from a project stall.</practice>
  <practice priority="high">Escalate every external watchdog to SIGKILL after a grace period (timeout --foreground -k); a stall inside an interrupt-disabled region ignores TERM entirely.</practice>
  <practice priority="high">Verify a spawned child's process group after run-program instead of assuming it; :input t leaves the child in the caller's group, so a later group kill targets your own process.</practice>
  <practice priority="critical">Release the state mutex before invoking any user callback, and reacquire it to record failures; the deadlock arrives through the error-recording path, not the happy path.</practice>
  <practice priority="high">Implement blocking waits as timeout-free predicate loops with explicit wakeups; sb-thread:condition-wait with :timeout can return without the mutex.</practice>
  <practice priority="medium">Run coverage single-worker, load instrumented sources through asdf:load-system :force t, and gate the report against a component manifest rather than an aggregate percentage.</practice>
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

  <avoid name="bisecting_project_files_for_a_plan_layer_hang">
    <description>Splitting and re-splitting project sources to find an ASDF hang that is actually in system discovery or source-registry flattening, before running any control experiment.</description>
    <instead>Probe the layers first: (require :asdf), (asdf:load-system "asdf"), find-system, load-asd, operate. The first probe that hangs names the layer; a direct (load ...) that returns promptly proves the sources are innocent.</instead>
  </avoid>

  <avoid name="term_only_watchdog">
    <description>Bounding a run with plain timeout or a TERM-only kill, so a child stalled in an interrupt-disabled region survives its deadline and leaks past the job budget.</description>
    <instead>timeout --foreground -k &lt;grace&gt; &lt;limit&gt;, and budget the enclosing CI step against limit plus grace.</instead>
  </avoid>

  <avoid name="assumed_child_process_group">
    <description>Killing "the child's process group" using a pgid inferred from the spawn rather than verified after it — which signals the caller's own group when the child was spawned with :input t, and can hit an unrelated group once the leader has been reaped.</description>
    <instead>Verify getpgid(pid) equals pid after spawn, keep the verified pgid in an opaque handle, and reject the handle once its leader reaches a terminal state.</instead>
  </avoid>

  <avoid name="callback_under_the_state_lock">
    <description>Invoking a user-supplied callback while holding the mutex that guards the state it reports on; a callback that signals deadlocks on the recursive lock taken to record the failure.</description>
    <instead>Mutate state under the lock, release it, call the callback, then reacquire only to record the outcome.</instead>
  </avoid>

  <avoid name="condition_wait_timeout_as_blocking_semantics">
    <description>Implementing a bounded blocking operation with sb-thread:condition-wait :timeout, which may return without the mutex and surface as an ownership error at the enclosing with-mutex exit.</description>
    <instead>Use a timeout-free predicate loop and signal the condition variable from every site that can change the predicate, including cancellation.</instead>
  </avoid>

  <avoid name="aggregate_coverage_without_a_manifest">
    <description>Accepting a 100% aggregate coverage total without checking that every production component actually has a row in the report.</description>
    <instead>Compare normalized report filenames against the ASDF component manifest and reject missing, malformed, or zero-total rows before reading the percentage.</instead>
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

<error_escalation>
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
  <skill name="performance-benchmarking">Benchmark methodology: paired protocols, noise floor, interval-based gating — the discipline behind the profiling tools above</skill>
  <skill name="test-integrity">False-green testing: suites that report success without exercising the contract</skill>
  <skill name="state-transactions">Atomic publish and cleanup discipline for the process/resource lifecycles spawned here</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate code patterns and references in this skill domain</agent>
  <agent name="quality-assurance">Review implementation quality against this skill guidance</agent>
  <agent name="code-quality">Analyze code complexity and suggest refactoring improvements</agent>
</related_agents>
