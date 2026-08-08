---
argument-hint: [task-description]
description: Full task execution with feedback loop
---

<purpose>
Execute tasks with automatic feedback collection and conditional fix phase. Runs execute -> feedback -> fix issues (only if issues found) in a single automated flow.
</purpose>
<rules priority="critical">
  <rule>Exactly one fix iteration. A second pass means the initial implementation was too fragmented,
    which is a scope decision for the user rather than something more automation fixes.</rule>
  <rule>Write tests for all implemented functionality and run them; failures are issues that feed the
    fix phase, never a reason to complete quietly.</rule>
</rules>
<rules priority="important">
  <rule>Complete the full cycle — execute, feedback, fix (conditional) — flowing between phases without
    user confirmation, since eliminating those hand-offs is what this command buys over /execute.</rule>
  <rule>Skip the fix phase when feedback found nothing, and say so explicitly rather than running it as
    a no-op.</rule>
  <rule>Fix only the issues feedback identified. A broad rewrite during the fix phase indicates a
    planning failure and destroys the review that justified it.</rule>
  <rule>Check Serena memories before implementation.</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Running the six feedback agents (quality, security, design, docs, performance, test) sequentially — all six evaluate independent dimensions of the same output and must run in parallel</practice>
    <practice>Triggering the fix phase unconditionally after feedback — it is conditional, and a no-issue run ends with an explicit skip confirmation</practice>
    <practice>Performing full re-implementation in the fix phase — fixes are targeted to identified issues</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Treat test execution failures from the execute phase as first-class issues feeding the fix_issues phase; they are not separate from the feedback loop</principle>
    <principle>Limit the cycle to one fix iteration; escalate scope to the user rather than iterating</principle>
    <principle>Flow automatically between phases without user confirmation</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load the execution-workflow skill with the Skill tool. It governs the delegation contract,
        the definition of done, and the review criteria all three phases of this command rest on. A
        skill that is named but never loaded contributes nothing to this run.</action>
      <tool>Skill (execution-workflow)</tool>
      <output>Skill loaded</output>
    </step>
    <step order="2">
      <action>Activate the Serena project and call list_memories.</action>
      <tool>Serena activate_project, list_memories</tool>
      <output>Project activated; full memory index</output>
    </step>
    <step order="3">
      <action>Classify the task as "implementation" and filter the memory index to {feature}-patterns,
        {language}-conventions, testing-patterns, and any completion-checklist or canonical-gate memory
        for this project. Load only the matches with read_memory. The last category is what tells you
        which commands constitute "done" here, and what they deliberately leave uncovered, without
        re-deriving it from the build files each session.</action>
      <tool>Serena read_memory</tool>
      <output>Matched memory names, and the ones loaded</output>
    </step>
  </phase>

  <phase name="analyze_execute">
    <step order="1">
      <action>Establish the task inventory: what needs doing, split into atomic units, each with a stated
        boundary.</action>
      <output>Atomic task list with boundaries</output>
    </step>
    <step order="2">
      <action>If the task adds one more member of a family that already exists — a module, an entity, a
        test, a command — enumerate its registration surfaces before the first edit. Take the nearest
        existing sibling, grep its identifier across the whole repository, and treat every hit outside
        its own module as a required touch point; sites that name no sibling are convention-discovered
        and need no edit. A missed explicit list produces the failure that looks like success:
        everything compiles and the feature is unreachable at runtime.</action>
      <tool>Grep</tool>
      <output>Required touch points, and the sites confirmed convention-discovered</output>
    </step>
    <step order="3">
      <action>Assign a sub-agent per unit, mark which units are independent, and order the rest by the
        specific output each waits on.</action>
      <output>Agent assignments, parallel groups, and the dependency behind each sequential step</output>
    </step>
    <step order="4">
      <action>Define what will prove the work done, in two separate lists: items a command discharges
        (name the command) and items an artifact discharges (name the file:line it will point at). An
        item that carries neither is a discussion point rather than a checklist entry — a prose checkbox
        gets ticked by impression, and a wall of ticked boxes has approved a defective diff before.</action>
      <output>Verification checklist, split by what discharges each item</output>
    </step>
    <step order="5">
      <action>Before implementation begins, send the planned placement to the design agent: which module
        or layer each new symbol lands in, and what it will depend on. Placement is the one finding whose
        repair costs as much as the implementation, because the fix is moving code and its dependencies
        rather than rewriting it — and with one fix iteration available, discovering a layering violation
        in the feedback wave leaves no budget to correct it.</action>
      <tool>Task (design)</tool>
      <output>Placement approved, or the layer violation named before any code is written</output>
    </step>
    <step order="6">
      <action>Delegate each unit with its scope, target paths, expected deliverable, and the command that
        verifies it. When a unit removes or migrates an existing definition, instruct the assignee to
        grep the identifier itself rather than the shape it is usually called in: forward declarations,
        differently-shaped call sites, comments, and test doubles share only the name.</action>
      <tool>Task</tool>
      <output>Delegated units with full context</output>
    </step>
    <step order="7">
      <action>Verify sub-agent outputs and combine them into a single implementation state.</action>
      <output>Consolidated implementation</output>
    </step>
    <step order="8">
      <action>Establish what the verification command actually covers before running it. Its name is not
        its scope: the configuration may exclude part of the tree, the editor or language server may read
        a different configuration than the command does, and a narrowed filter may still pull in shared
        fixtures so a failure need not belong to this change. Confirm the command includes what was
        changed, and that its input set saw files created this session — a tool that snapshots from
        version control or reads an explicit entry manifest silently skips an untracked new file.</action>
      <output>Covered scope, the configuration that defines it, and confirmation the new work was included</output>
    </step>
    <step order="9">
      <action>Confirm the tree is quiescent before compiling or running the suite: every write-capable
        agent has returned, and no fix is still in flight. A compile that overlaps an edit produces a
        mixed-generation artifact set, and the suite then exercises a stale unit while isolated
        source-preferred runs pass — the two results come from the same source and disagree. Freeze
        edits, compile to completion, then run the suite in a fresh process and record the results.</action>
      <tool>Bash (test runner)</tool>
      <output>Test execution results: pass/fail status and failing test names, feeding collect_feedback and fix_issues</output>
    </step>
  </phase>

  <phase name="collect_feedback">
    <step order="1">
      <action>Launch quality agent: syntax, type, format verification</action>
      <tool>Task (quality-assurance)</tool>
      <output>Quality evaluation report</output>
    </step>
    <step order="2">
      <action>Launch security agent: vulnerability detection</action>
      <tool>Task (security)</tool>
      <output>Security evaluation report</output>
    </step>
    <step order="3">
      <action>Launch design agent: architecture consistency</action>
      <tool>Task (design)</tool>
      <output>Design evaluation report</output>
    </step>
    <step order="4">
      <action>Launch docs agent: documentation completeness</action>
      <tool>Task (docs)</tool>
      <output>Documentation evaluation report</output>
    </step>
    <step order="5">
      <action>Launch performance agent: performance implications</action>
      <tool>Task (performance)</tool>
      <output>Performance evaluation report</output>
    </step>
    <step order="6">
      <action>Launch test agent: test coverage analysis</action>
      <tool>Task (test)</tool>
      <output>Test evaluation report</output>
    </step>
  </phase>

  <reflection_checkpoint id="feedback_quality" after="collect_feedback">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name all six agents and what each returned. Name any that timed out or died without
      returning — a missing report is not an absence of issues.</check>
    <check>For each reported issue, name the file:line or the command output it cites. An agent whose
      findings cite nothing checkable is a retry condition, not a clean result.</check>
    <check>Name the issues classified critical and the runtime impact that makes each one critical,
      or state that none are.</check>
    <on_unmet>Re-run the named agent once with a narrower prompt naming the specific files. If it fails
      again, review that dimension here and report that the delegation failed.</on_unmet>
  </reflection_checkpoint>

  <phase name="fix_issues">
    <step order="1">
      <action>Consolidate the agents' findings with the test failures from analyze_execute, then confirm
        each finding still holds before acting on it: open the file:line it cites and check the condition
        is present now. A parallel review reports the tree as it stood when that agent read it, and any
        write between the read and the fix makes the report stale without making it wrong — the citation
        was accurate when written, so scrutinizing the evidence will not reveal the drift. The check
        costs one Read per finding; fixing what is already fixed and reporting it costs more.</action>
      <output>Consolidated issue list, each marked still-present or already-resolved with the line that shows it</output>
    </step>
    <step order="2">
      <action>Prioritize the still-present issues by severity (critical > warning > info).</action>
      <output>Prioritized issue list</output>
    </step>
    <step order="3">
      <action>Delegate fixes to the agents matching each issue category.</action>
      <tool>Task</tool>
      <output>Fix assignments</output>
    </step>
    <step order="4">
      <action>Verify each fix against the issue it was meant to address, and re-run the verification
        commands.</action>
      <output>Verification results</output>
    </step>
    <step order="5">
      <action>Consolidate the fix results.</action>
      <output>Fixed implementation</output>
    </step>
  </phase>

  <iteration_limit>1</iteration_limit>
  <reflection_checkpoint id="fix_complete" after="fix_issues">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name each critical issue from the consolidated list and the file:line of the change that
      addresses it, or the reason it was deferred.</check>
    <check>Name the commands re-run after the fixes and their exit status.</check>
    <check>For any symptom that stopped appearing, name the change that stopped it. "It does not happen
      any more" is equally consistent with the fix working, with a rebuild clearing a stale artifact,
      and with the observation being unreliable; without a named diff hunk, a cache clear gets recorded
      as an engineering result while the defect stays open.</check>
    <check>For any test added to guard a fix, state that it was run against the pre-fix state and failed
      there. A regression test that has never been red is an assertion about the fix rather than a guard
      on it, and an arrange step that steers the system away from the condition under test looks like
      careful setup on inspection.</check>
    <check>Name every issue left unaddressed — including warnings judged infeasible — with its location
      and the reason, in a form the next review can reconcile against. A finding that is neither fixed
      nor carried forward is rediscovered as new, or not at all, and one fix iteration leaves no other
      mechanism for tracking it.</check>
    <on_unmet>Report the unaddressed issues to the user as deferred, with the reason. Do not open a
      second fix iteration (EXECF-P002).</on_unmet>
  </reflection_checkpoint>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Answer each check with a concrete artifact.</gate>
  <check>Name any required section that is absent or out of order, or state that all are present.</check>
  <check>Name the branch or worktree the work will happen in, and confirm it is not the default branch.</check>
  <on_unmet>Stop and resolve the structural gap before executing any phase.</on_unmet>
</reflection_checkpoint>
<agents>
  <agent name="quality" subagent_type="quality-assurance" readonly="false">
    <role>Verify syntax correctness, type safety, and code format compliance for implemented changes</role>
    <receives>file_paths[], change_description, project_language, style_config_path</receives>
    <produces>issues[]{severity: critical|warning|info, location: file:line, message, suggestion, evidence: file:line or command output}</produces>
    <done_when>All modified files checked; no critical issues remain or all critical issues documented</done_when>
  </agent>
  <agent name="security" subagent_type="security" readonly="false">
    <role>Detect security vulnerabilities introduced by the implementation</role>
    <receives>file_paths[], change_description, threat_context</receives>
    <produces>vulnerabilities[]{severity: critical|high|medium|low, cwe, location: file:line, description, remediation, evidence: file:line or command output}</produces>
    <done_when>All security-relevant code paths analyzed; OWASP top-10 categories checked for applicable patterns</done_when>
  </agent>
  <agent name="test" subagent_type="test" readonly="false">
    <role>Create comprehensive tests covering all implemented functionality and edge cases</role>
    <receives>implementation_files[], acceptance_criteria[], existing_test_paths[], test_framework</receives>
    <produces>test_files_created[], test_cases[]{name, type: unit|integration|e2e, coverage_target}, test_run_command</produces>
    <done_when>Tests written for all acceptance criteria; test_run_command confirmed executable</done_when>
    <constraint>Never write an always-passing test alongside a comment explaining that the behavior
      cannot be verified in this environment. Read the existing test helpers before concluding it
      cannot: the harness usually already has the capability, and the rationale comment left behind
      suppresses the next attempt too. A stub of this shape is removed, not kept.</constraint>
    <constraint>A test written to guard a specific fix must be run against the pre-fix state and observed
      to fail there before it counts as a regression test.</constraint>
  </agent>
  <agent name="refactor" subagent_type="general-purpose" readonly="false">
    <role>Improve code structure and reduce tech debt without changing observable behavior</role>
    <receives>file_paths[], tech_debt_notes[], refactoring_scope</receives>
    <produces>refactored_files[], changes_summary[], behavior_invariants_preserved: bool</produces>
    <done_when>Targeted refactoring applied; all behavior invariants preserved and documented</done_when>
    <constraint>When removing or migrating a definition, grep the identifier itself across every file
      rather than the usage shape it typically appears in. The identifier is the only invariant shared
      by forward declarations, differently-shaped call sites, comments, and test doubles.</constraint>
  </agent>
  <agent name="docs" subagent_type="docs" readonly="false">
    <role>Update documentation to accurately reflect implementation changes</role>
    <receives>changed_files[], change_summary, doc_paths[], api_changes[]</receives>
    <produces>updated_doc_files[], new_doc_sections[], coverage_report{updated: int, missing: int}</produces>
    <done_when>All public interfaces and behavior changes documented; no stale references remain</done_when>
  </agent>
  <agent name="review" subagent_type="quality-assurance" readonly="false">
    <role>Perform holistic post-implementation review across all quality dimensions</role>
    <receives>all_changed_files[], implementation_summary, test_results, agent_reports[]</receives>
    <produces>review_summary{critical_findings[], warnings[], commendations[]}, go_no_go: bool</produces>
    <done_when>All implementation artifacts reviewed; go/no-go decision made with rationale</done_when>
  </agent>
  <agent name="debug" subagent_type="general-purpose" readonly="false">
    <role>Diagnose and resolve failures encountered during implementation or test execution</role>
    <receives>error_message, stack_trace, failing_test_names[], file_paths[]</receives>
    <produces>root_cause{location: file:line, description}, fix_applied{file, change}, verification_command</produces>
    <done_when>Root cause identified and fix applied; verification command confirmed passing</done_when>
    <constraint>A symptom that stopped appearing closes only when the change that stopped it is named.
      Report the diff hunk, not the absence.</constraint>
  </agent>
  <agent name="performance" subagent_type="performance" readonly="false">
    <role>Optimize performance-critical paths and eliminate unnecessary overhead</role>
    <receives>file_paths[], performance_targets, profiling_data</receives>
    <produces>optimizations[]{location: file:line, before, after, expected_improvement}, benchmark_command</produces>
    <done_when>Performance targets met or improvement quantified; no regressions introduced</done_when>
  </agent>
  <agent name="clean" subagent_type="code-quality" readonly="false">
    <role>Eliminate dead code, unused imports, and unreachable paths safely</role>
    <receives>file_paths[], scope_boundary</receives>
    <produces>removed_symbols[], cleaned_files[], impact_assessment{safe_to_remove: bool, reason}</produces>
    <done_when>All confirmed-dead code removed; no live code deleted</done_when>
  </agent>
  <agent name="error-handling" subagent_type="general-purpose" readonly="false">
    <role>Implement consistent error handling, recovery patterns, and user-facing error messages</role>
    <receives>file_paths[], error_scenarios[], handling_policy</receives>
    <produces>error_handlers_added[]{location: file:line, pattern}, unhandled_paths_remaining[]</produces>
    <done_when>All identified error paths handled; handling consistent with project policy</done_when>
  </agent>
  <agent name="migration" subagent_type="general-purpose" readonly="false">
    <role>Plan and execute data or code migrations with verified rollback safety</role>
    <receives>migration_scope, current_state, target_state, rollback_requirements</receives>
    <produces>migration_steps[], rollback_plan, pre_conditions[], post_conditions[]</produces>
    <done_when>Migration plan verified safe; rollback path confirmed; pre/post conditions testable</done_when>
  </agent>
  <agent name="database" subagent_type="database" readonly="false">
    <role>Design and optimize database schema changes and queries</role>
    <receives>schema_changes[], query_patterns[], performance_requirements</receives>
    <produces>migration_sql[], index_recommendations[], query_optimizations[], impact_analysis</produces>
    <done_when>Schema changes migration-safe; queries optimized to meet performance targets</done_when>
  </agent>
  <agent name="infrastructure" subagent_type="devops" readonly="false">
    <role>Design infrastructure changes and validate deployment configurations</role>
    <receives>service_requirements, current_infra_config, scaling_targets</receives>
    <produces>infra_config_changes[], deployment_steps[], rollback_procedure</produces>
    <done_when>Infrastructure changes validated in non-production; rollback procedure confirmed</done_when>
  </agent>
  <agent name="ci-cd" subagent_type="devops" readonly="false">
    <role>Design and optimize CI/CD pipelines and deployment workflows</role>
    <receives>pipeline_config_paths[], build_requirements, deployment_targets[]</receives>
    <produces>pipeline_changes[], stage_definitions[], expected_run_time_minutes: int</produces>
    <done_when>Pipeline changes validated; all required stages present and ordered correctly</done_when>
  </agent>
  <agent name="observability" subagent_type="devops" readonly="false">
    <role>Instrument code with logging, metrics, and tracing for production observability</role>
    <receives>file_paths[], observability_requirements, existing_instrumentation</receives>
    <produces>instrumentation_added[]{type: log|metric|trace, location: file:line}, dashboard_updates[]</produces>
    <done_when>All critical code paths instrumented; log levels consistent with project policy</done_when>
  </agent>
  <agent name="memory" subagent_type="general-purpose" readonly="false">
    <role>Capture significant architectural decisions and novel patterns to persistent memory; verify freshness of memories consulted during this task</role>
    <receives>implementation_summary, novel_patterns[], architectural_decisions[], deferred_decisions[], memories_read_this_task[]</receives>
    <produces>memory_entries_created[], memory_paths[], memories_verified[]</produces>
    <done_when>All non-obvious decisions and patterns captured; memory entries verified writable; memories_read_this_task checked for staleness</done_when>
    <constraint>Prepend the memory_content_format frontmatter (serena-usage skill) on write_memory; when
      editing a memory that lacks it, add it and update last-verified. Replace superseded content in
      place rather than appending. Apply memory_staleness_verification to memories_read_this_task.</constraint>
    <constraint>Also capture, because each is expensive to re-derive and none is discoverable by grep:
      the project's canonical verification command together with what it deliberately does not cover;
      the exact invocation form that exited zero, including any environment prefix and path flags; and
      any abstraction deliberately not built, paired with the condition that should re-open it. A
      deferral recorded without its trigger is re-argued from scratch next session with less
      information than this one had.</constraint>
  </agent>
  <agent name="design" subagent_type="design" readonly="true">
    <role>Evaluate placement and architecture consistency. Runs twice: before implementation on the
      planned placement, and in the feedback wave on what was built</role>
    <receives>planned_placement{symbol, target_module, dependencies} before implementation; changed_files[], architecture_context, design_principles[] afterwards</receives>
    <produces>design_assessment{violations[], improvements[]}, consistency_report</produces>
    <done_when>Placement decided before code is written; architecture consistency verified afterwards, with every violation documented</done_when>
  </agent>
  <agent name="validator" subagent_type="validator" readonly="true" dispatch="on_demand">
    <role>Independently re-derive a disputed claim. Dispatch only when two agents disagree and their
      cited evidence does not settle it, or when a consequential claim carries no citation at all — not
      as a routine phase, since an independent pass costs materially more than the report it checks</role>
    <receives>the disputed claim and its cited file:line or command output, without the originating agent's reasoning</receives>
    <produces>independent verdict with the evidence it rests on</produces>
    <done_when>The claim is confirmed, overturned, or reported unresolved with both positions carried to the user</done_when>
  </agent>
</agents>
<execution_graph>
  <sequential_phase id="execute" depends_on="none">
    <sequential_step id="placement_review">
      <agent>design</agent>
      <reason>Placement is decided before implementation, because relocating a symbol and its dependencies later costs as much as writing it and the single fix iteration cannot absorb that</reason>
    </sequential_step>
    <parallel_group id="quality_assurance">
      <agent>quality</agent>
      <agent>security</agent>
    </parallel_group>
    <parallel_group id="implementation">
      <agent>test</agent>
      <agent>docs</agent>
    </parallel_group>
    <sequential_step id="consolidation">
      <action>Wait for every write-capable agent to return, then consolidate and run the suite in a fresh process</action>
    </sequential_step>
  </sequential_phase>
  <sequential_phase id="feedback" depends_on="execute">
    <parallel_group id="feedback_agents">
      <agent>quality</agent>
      <agent>security</agent>
      <agent>design</agent>
      <agent>docs</agent>
      <agent>performance</agent>
      <agent>test</agent>
    </parallel_group>
  </sequential_phase>
  <conditional_phase id="fix" depends_on="feedback">
    <condition>issues_found == true</condition>
    <skip_action>Output skip confirmation with no issues message</skip_action>
    <handoff from="collect_feedback" to="fix_issues">
      <trigger>Any feedback agent produces issues[].severity containing "critical" OR "warning"</trigger>
      <pass_forward>consolidated_issues[] with the file:line each cites, agent_reports[], test_failures[]</pass_forward>
      <skip_when>All feedback agents report issues[].length == 0</skip_when>
    </handoff>
    <parallel_group id="fix_agents">
      <agent>Agents matching issue categories</agent>
    </parallel_group>
  </conditional_phase>
  <sequential_step id="persist_phase" depends_on="fix">
    <agent>memory</agent>
    <reason>Capture novel patterns and architectural decisions discovered during execute-feedback-fix cycle</reason>
  </sequential_step>
</execution_graph>
<delegation>
  <requirement>Specific scope and expected deliverables</requirement>
  <requirement>Target file paths</requirement>
  <requirement>Reference implementations (specific paths)</requirement>
  <requirement>Memory check: list_memories for patterns</requirement>
  <requirement>For fix phase: specific issue references from feedback</requirement>
</delegation>
<decision_criteria>
  <factor name="task_clarity" precedence="1">
    <unmet>The request admits two readings that would produce different implementations. Ask with
      AskUserQuestion before delegating; do not implement the cheaper reading.</unmet>
  </factor>
  <factor name="implementation_quality" precedence="2">
    <unmet>No test command was run against the change, or a test failed. Run it, or record the failing
      test names as issues feeding the fix phase. An unverified implementation is not a candidate for
      completion however clean the feedback reports look.</unmet>
  </factor>
  <factor name="feedback_severity" precedence="3">
    <unmet>Any feedback agent reported a critical or warning issue. Enter the fix phase; skipping it is
      permitted only when every agent reported zero issues (EXECF-P005).</unmet>
  </factor>
  <factor name="fix_completeness" precedence="4">
    <unmet>A critical issue is neither fixed-and-re-verified nor recorded as deferred with a reason.
      Report it to the user as an open blocker rather than closing the cycle.</unmet>
  </factor>
  <resolution>Apply in precedence order. The first factor whose `unmet` condition holds decides what
    happens next; later factors are not consulted.</resolution>
</decision_criteria>
<output>
  <format>
    <execution_results>
      <summary>Brief description of implemented changes</summary>
      <files_modified>
        <file>
          <path>Absolute file path</path>
          <changes>Summary of changes</changes>
        </file>
      </files_modified>
      <tests_status>
        <command>test command used, or "none run" with the reason</command>
        <status>Pass/Fail</status>
        <failures>failing test names if any</failures>
        <scope>What the command covered, and confirmation it saw the files created this session</scope>
      </tests_status>
      <verification>Every command run and its exit status, or "none run" — never omitted</verification>
    </execution_results>
    <feedback_summary>
      <agent_reports>
        <report agent="quality|security|design|docs|performance|test">
          <returned>completed / timed out / returned nothing checkable</returned>
          <evidence_tier>verified|inferred|assumed, per the tiers in CLAUDE.md</evidence_tier>
        </report>
      </agent_reports>
      <issues_found>
        <critical>
          <issue>
            <category>Category</category>
            <description>Issue description</description>
            <location>File and line reference</location>
            <still_present>Confirmed present at fix time, or already resolved — with the line that shows it</still_present>
          </issue>
        </critical>
        <warnings>
          <issue>
            <category>Category</category>
            <description>Issue description</description>
            <recommendation>Suggested fix</recommendation>
          </issue>
        </warnings>
        <info>
          <issue>
            <category>Category</category>
            <description>Minor observation</description>
          </issue>
        </info>
      </issues_found>
      <good_practices>
        <practice>
          <category>Category</category>
          <description>Commendable aspects</description>
        </practice>
      </good_practices>
    </feedback_summary>
    <fix_results condition="if issues found">
      <issues_addressed>
        <issue>
          <original>Original issue from feedback</original>
          <fix>How it was fixed, naming the change — not the disappearance of the symptom</fix>
          <status>Fixed/Deferred</status>
        </issue>
      </issues_addressed>
      <deferred_issues>
        <issue>
          <description>Issue not fixed</description>
          <location>Where it is, so the next review can reconcile against it</location>
          <reason>Justification for deferral</reason>
        </issue>
      </deferred_issues>
    </fix_results>
    <skip_confirmation condition="if no issues found">
      <message>No issues requiring fixes were identified in feedback phase</message>
      <status>Fix phase skipped</status>
    </skip_confirmation>
    <final_status>
      <status>success|warning|error</status>
      <summary>What was asked, what was implemented, and what remains unchecked</summary>
      <verification>The enumerated commands that must exit zero for this change, and which of them
        actually ran with its exit status. A completion claim naming no command is not one.</verification>
      <weakest_claim>The claim resting on the thinnest evidence, and what would confirm it</weakest_claim>
      <gaps>Anything asked for that was not done, and why — not attempted, blocked, or out of scope</gaps>
      <next_steps>Recommended follow-up actions if any</next_steps>
    </final_status>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="EXECF-B001" priority="important">
      <trigger>Before implementation</trigger>
      <action>Check Serena memories for existing patterns</action>
      <verification>Pattern check in output</verification>
    </behavior>
    <behavior id="EXECF-B002" priority="important">
      <trigger>After initial execution</trigger>
      <action>Run the feedback collection phase automatically</action>
      <verification>Feedback results in output</verification>
    </behavior>
    <behavior id="EXECF-B003" priority="important">
      <trigger>After feedback collection</trigger>
      <action>Evaluate whether the issues require the fix phase</action>
      <verification>Issue evaluation in output</verification>
    </behavior>
    <behavior id="EXECF-B004" priority="important">
      <trigger>When issues are found</trigger>
      <action>Run the fix phase against those issues only</action>
      <verification>Fix results in output</verification>
    </behavior>
    <behavior id="EXECF-B005" priority="advisory">
      <trigger>When no issues are found</trigger>
      <action>Skip the fix phase and say so, rather than running it as a no-op</action>
      <verification>Skip confirmation in output</verification>
    </behavior>
    <behavior id="EXECF-B006" priority="important">
      <trigger>During the feedback phase</trigger>
      <action>Dispatch all six feedback agents in one message, since they evaluate independent
        dimensions of the same output and serializing them only costs wall time</action>
      <verification>Parallel dispatch confirmed</verification>
    </behavior>
    <behavior id="EXECF-B007" priority="critical">
      <trigger>During the execute phase</trigger>
      <action>Delegate test creation to the test agent for all implemented functionality, using the
        acceptance criteria from /define output as targets</action>
      <verification>Test files created and listed in output</verification>
    </behavior>
    <behavior id="EXECF-B008" priority="critical">
      <trigger>After test creation, at the verification step of analyze_execute</trigger>
      <action>Run all test commands once the tree is quiescent; failures are treated as issues and feed
        the fix_issues phase</action>
      <verification>Test execution results recorded in output; failures appear in fix_results</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="EXECF-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Implementing without sub-agent delegation</action>
      <response>Block operation, delegate to specialized agents</response>
    </behavior>
    <behavior id="EXECF-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Opening a second fix iteration</action>
      <response>Block. Report what remains as deferred and let the user decide on scope.</response>
    </behavior>
    <behavior id="EXECF-P003" priority="important">
      <trigger>Between phases</trigger>
      <action>Requesting user confirmation to proceed</action>
      <response>Proceed automatically between phases</response>
    </behavior>
    <behavior id="EXECF-P004" priority="critical">
      <trigger>In the fix phase</trigger>
      <action>Full re-implementation instead of targeted fixes</action>
      <response>Fix only the identified issues; a rewrite discards the review that justified it</response>
    </behavior>
    <behavior id="EXECF-P005" priority="advisory">
      <trigger>When no issues are found</trigger>
      <action>Running the fix phase anyway</action>
      <response>Skip it and output the confirmation</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation>
  <examples>
    <example severity="low">Minor code style inconsistency</example>
    <example severity="medium">Test failure or unclear implementation approach</example>
    <example severity="high">Breaking change or major implementation blocker</example>
    <example severity="critical">Security vulnerability or data loss risk</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="execute">Basic execution without feedback loop</command>
  <command name="feedback">Standalone feedback command for reviewing work</command>
  <command name="define">When implementation reveals unclear requirements</command>
  <command name="define-full">When detailed requirements definition is needed</command>
  <command name="ask">When implementation requires investigation</command>
  <command name="bug">When implementation encounters unexpected errors</command>
  <command name="upstream">When preparing changes for upstream OSS contribution</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
</related_agents>
<related_skills>
  <skill name="execution-workflow">Core delegation and orchestration patterns; loaded in the prepare phase</skill>
  <skill name="serena-usage">Check memories for existing patterns before implementation</skill>
  <skill name="testing-patterns">Ensure proper test coverage</skill>
  <skill name="test-integrity">Load when a suite reports green and the question is whether it proves anything</skill>
</related_skills>
<constraints>
  <must>Delegate detailed work to sub-agents and run independent tasks in parallel</must>
  <must>Complete all phases: execute, feedback, fix (conditional), proceeding between them without user confirmation</must>
  <must>Skip the fix phase when no issues were found, and limit the cycle to one fix iteration</must>
  <must>Write tests for all implemented functionality; skipping tests is not acceptable</must>
  <must>Run all test commands after test creation; failures are fix-phase issues</must>
  <must>Define done as an enumerated set of commands that exit zero — the project's test command, plus its lint, build, or type-check command where one exists — and report which of them actually ran with its exit status. A completion claim naming no command is not a completion claim</must>
  <must>Commit only under the git rules in CLAUDE.md: never without the user's instruction in the current message, never to the default branch</must>
  <avoid>Implementing detailed logic directly</avoid>
  <avoid>Multiple fix iterations (exactly one allowed when needed)</avoid>
  <avoid>Sequential execution of independent feedback agents</avoid>
  <avoid>Full re-implementation in fix phase</avoid>
  <avoid>Marking implementation complete without corresponding tests</avoid>
</constraints>
