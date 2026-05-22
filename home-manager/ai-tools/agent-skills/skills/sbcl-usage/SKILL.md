---
name: sbcl-usage
description: Practical SBCL (Steel Bank Common Lisp) operations guide. Use this skill whenever the user mentions SBCL execution/debugging, --script usage, REPL workflows, backtraces, ASDF loading, save-lisp-and-die, profiling, or SLY-based Common Lisp development.
version: 2.0.0
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
