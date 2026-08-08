---
argument-hint: [task-description]
description: Task execution command
---

<purpose>
Execute tasks by delegating detailed work to sub-agents while focusing on policy decisions and orchestration. Includes test self-healing: if written tests fail, one targeted fix attempt is made before completion. For comprehensive multi-agent quality review across all dimensions, use /execute-full.
</purpose>
<rules priority="critical">
  <rule>Write tests for all implemented functionality and run them. An implementation whose tests were
    never executed is not complete, however clean it reads.</rule>
  <rule>At most one fix iteration for failing tests. Report what still fails as a blocker — a second
    automated pass hides a scope problem the user needs to decide on.</rule>
</rules>
<rules priority="important">
  <rule>Delegate detailed work to specialized sub-agents; run independent units in parallel and
    dependent units in order.</rule>
  <rule>Verify sub-agent outputs before integrating them, because a report citing nothing checkable is
    not a result.</rule>
  <rule>Check Serena memories before implementing, so an existing utility or convention is reused
    rather than re-created beside itself.</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Implementing every specialized concern in a single pass — quality, security, tests, and docs each go to their agent with full context and an explicit deliverable</practice>
    <practice>Treating test creation as deferrable — no implementation is complete without corresponding tests</practice>
    <practice>Completing silently when tests still fail after the fix attempt — remaining failures belong in follow_up as blockers</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Decompose into atomic units with clear boundaries before delegating; never serialize what can be parallelized</principle>
    <principle>Check Serena memories for existing patterns before every implementation</principle>
    <principle>Run the inferred test command immediately after test creation; on failure, attempt exactly one targeted fix and re-run</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <objective>Load the governing skill, initialize Serena, and read task-appropriate patterns</objective>
    <step order="1">
      <action>Load the execution-workflow skill with the Skill tool. It governs the delegation contract,
        the definition of done, and the review criteria this command depends on. A skill that is named
        but not loaded contributes nothing to this run.</action>
      <tool>Skill (execution-workflow)</tool>
      <output>Skill loaded</output>
    </step>
    <step order="2">
      <action>Activate the Serena project and call list_memories.</action>
      <tool>Serena activate_project, list_memories</tool>
      <output>Project activated; full memory index</output>
    </step>
    <step order="3">
      <action>Classify the task as "implementation" and filter the memory index to the categories that
        matter here: {feature}-patterns, {language}-conventions, testing-patterns, and any
        completion-checklist or verification-command memory for this project. Load only the matches with
        read_memory. The completion-checklist category is what tells you which commands constitute
        "done" here without re-deriving it from build files.</action>
      <tool>Serena read_memory</tool>
      <output>Matched memory names, and the ones loaded</output>
    </step>
  </phase>
  <phase name="analyze">
    <objective>Establish the task inventory, its touch points, and what will prove it done</objective>
    <step order="1">
      <action>Identify the concrete tasks to be completed, split them into atomic units, and state the
        boundary of each.</action>
      <output>Task inventory with boundaries</output>
    </step>
    <step order="2">
      <action>If the task adds one more of something that already exists — a module, an entity, a test,
        a command — enumerate its registration surfaces before the first edit. Pick the nearest existing
        sibling, grep its identifier across the whole repository, and treat every hit outside its own
        module as a required touch point; the sites that name no sibling are convention-discovered and
        need no edit. Skipping this produces the failure that looks like success: everything compiles
        and the feature is unreachable because one explicit list was never updated.</action>
      <tool>Grep</tool>
      <output>Required touch points, and the sites confirmed convention-discovered</output>
    </step>
    <step order="3">
      <action>Select the best-fit sub-agent per task, mark which tasks are independent, and order the
        rest by the specific output each waits on.</action>
      <output>Delegation map with parallel groups and the dependency behind each sequential step</output>
    </step>
    <step order="4">
      <action>Define what will prove the work done, in two separate lists: items a command discharges
        (name the command) and items an artifact discharges (name the file:line it will point at). An
        item that can carry neither is a discussion point, not a checklist entry — a prose checkbox gets
        ticked by impression rather than by evidence.</action>
      <output>Verification checklist, split by what discharges each item</output>
    </step>
  </phase>
  <reflection_checkpoint id="analysis_quality" after="analyze">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name each task in the inventory and the agent it is going to.</check>
    <check>Name the registration surfaces found in step 2, or state that this task adds no new member of
      an existing family.</check>
    <check>Name which tasks run in parallel and the dependency forcing the rest to be sequential.</check>
    <on_unmet>Obtain the missing item before delegating.</on_unmet>
  </reflection_checkpoint>
  <phase name="assign">
    <objective>Delegate with enough context that the assignee does not have to guess</objective>
    <step order="1">
      <action>For tasks that modify existing symbols: call find_referencing_symbols to assess blast
        radius, and embed the reference count and affected file list in the delegation prompt (EXEC-B005).
        When a definition is being removed or migrated, grep the identifier itself rather than the shape
        it is usually called in — forward declarations, differently-shaped call sites, comments, and test
        doubles share only the name.</action>
      <tool>Serena find_referencing_symbols, Grep</tool>
      <output>Blast radius: N references in M files, included in the delegation context</output>
    </step>
    <step order="2">
      <action>Delegate each task with its scope, its target file paths, the expected deliverable, and any
        reference implementation to follow.</action>
      <tool>Task</tool>
      <output>Delegation requests issued</output>
    </step>
  </phase>
  <reflection_checkpoint id="assignment_complete" after="assign">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name every task in the inventory and the agent it was dispatched to, or state it is being
      done here and why. A task on neither list was dropped.</check>
    <check>Quote the file paths and the expected deliverable given to each agent. A prompt naming no
      path is not a delegation (EXEC-P001).</check>
    <check>Name the tasks that must wait, and the specific output each one waits on.</check>
    <on_unmet>Do not dispatch. Supply the missing item, or ask with AskUserQuestion if only the user
      can resolve it.</on_unmet>
  </reflection_checkpoint>
  <phase name="consolidate">
    <objective>Integrate sub-agent outputs and establish that the verification actually verified this change</objective>
    <step order="1">
      <action>Check each agent's output for completeness, then call get_diagnostics_for_file
        (min_severity=2) on every modified file to catch language-server errors before running tests.</action>
      <tool>Serena get_diagnostics_for_file</tool>
      <output>Verified sub-agent results; any diagnostics reported as blockers</output>
    </step>
    <step order="2">
      <action>Before running the project's verification command, establish what it actually covers. A
        command's name is not its scope, and it diverges in three ways beyond the baseline check: the
        configuration excludes part of the tree, the editor or language server reads a different
        configuration than the command does so the two disagree about the same file, and a narrowed
        filter still pulls in shared fixtures so a failure need not belong to this change. Answer one
        question before running it — does this command include what was changed?</action>
      <output>The command's covered scope, and which configuration file defines it</output>
    </step>
    <step order="3">
      <action>Name what the gate itself writes into the working tree — build output, coverage data,
        generated loaders — before reading its result. An artifact it drops makes the later change
        report unreadable, and an ignored one will not even appear in a diff, so route generated output
        to a temporary directory. On the reading side, confirm the run started its own service or
        fixture rather than attaching to one an earlier session left running.</action>
      <output>What the gate writes, and whether it started its own dependencies</output>
    </step>
    <step order="4">
      <action>Run the test commands for all written tests. Infer the command from the project's language
        and framework; if it cannot be inferred, check the package, build, or project manifest; if it
        still cannot, report that as a blocker. Confirm the run's input set included the new work — a
        tool that snapshots from version control, honors an ignore file, or reads an explicit entry
        manifest silently skips a file created this session and not yet tracked. Check that the new file
        appears in the tool's own file list, or that the new test appears in the run count.</action>
      <tool>Bash (test runner)</tool>
      <output>Test results with the command run, and confirmation the run saw the new files</output>
    </step>
    <step order="5">
      <action>If tests fail, delegate one targeted fix for the specific failing tests and re-run once.
        If failures remain after that single attempt, report them as blockers in follow_up and set the
        status to FAIL.</action>
      <tool>Task (test agent, or general-purpose)</tool>
      <output>All tests passing, or a blocker report listing what still fails</output>
    </step>
    <step order="6">
      <action>Before reporting that something could not be verified in this environment, grep the
        environment variables the application reads and its scripts directory for a substitute backend,
        an in-memory adapter, or a recorded-fixture mode. A codebase mature enough to have a test suite
        usually has a runnable driver behind that seam, and an unverifiable claim reported as a gap is
        rarely revisited.</action>
      <output>The substitute mode found and exercised, or confirmation that none exists</output>
    </step>
    <step order="7">
      <action>Combine the verified results and test outcomes into the final output.</action>
      <output>Consolidated result including verification status</output>
    </step>
  </phase>
  <!-- persist phase: orchestrator-synthesized insights visible only after all agents complete.
       The memory agent in execution_graph captures patterns sourced from implementation sub-agents.
       These two mechanisms are complementary: agent-sourced findings → memory agent; orchestrator-level synthesis → this phase. -->
  <phase name="persist">
    <objective>Capture orchestrator-level synthesis to Serena memory</objective>
    <step order="1">
      <action>Evaluate the memory_auto_creation_triggers (serena-usage skill): architectural pattern,
        feature pattern, user-stated convention, refactoring approach. Add three triggers that this
        command produces and that are expensive to re-derive:
        (a) the verification command in the exact form that exited zero, including any environment
        prefix and path flags, since a bare tool name costs the next session the same trial and error;
        (b) the project's canonical gate and what it deliberately does not cover, as a
        completion-checklist memory;
        (c) an abstraction deliberately not built, together with the condition that should re-open it —
        a deferral without its trigger gets re-argued from scratch with less information than the first
        time.
        Call list_memories to check whether the topic already has an entry.</action>
      <tool>Serena list_memories</tool>
      <output>Trigger match per item; existing memory yes/no</output>
    </step>
    <step order="2">
      <action>On a match, use edit_memory for an existing topic or write_memory for a new one, following
        the memory_lifecycle naming convention. Prepend the memory_content_format frontmatter
        (serena-usage skill); when editing a memory that lacks it, add it and update last-verified.
        Replace superseded content rather than appending to it, so the entry stays a statement of fact
        rather than becoming a changelog. If nothing matched, output "persist: no triggers matched — skip".</action>
      <tool>Serena edit_memory or write_memory</tool>
      <output>Memory names written or edited, or the explicit skip</output>
    </step>
    <step order="3">
      <action>Apply memory_staleness_verification to the memories loaded in prepare: bump last-verified
        if still accurate, correct it if partly outdated, or rename with an -archived suffix if fully
        superseded. Do not read further memories only to check their freshness.</action>
      <tool>Serena edit_memory, rename_memory</tool>
      <output>Memories verified, updated, or archived — or "none read this task required verification"</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Answer each check with a concrete artifact.</gate>
  <check>Name any required section that is absent or out of order, or state that all are present.</check>
  <check>Name the branch or worktree the work will happen in, and confirm it is not the default branch.</check>
  <on_unmet>Stop and resolve the structural gap before executing any step.</on_unmet>
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
    <constraint>Never write a test that always passes together with a comment explaining why the
      behavior cannot be verified here. Before concluding it cannot, read the existing test helpers:
      the harness usually already has the capability, and the stale rationale left behind suppresses
      the next attempt as well.</constraint>
  </agent>
  <agent name="refactor" subagent_type="general-purpose" readonly="false">
    <role>Improve code structure and reduce tech debt without changing observable behavior</role>
    <receives>file_paths[], tech_debt_notes[], refactoring_scope</receives>
    <produces>refactored_files[], changes_summary[], behavior_invariants_preserved: bool</produces>
    <done_when>Targeted refactoring applied; all behavior invariants preserved and documented</done_when>
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
    <role>Capture significant architectural decisions and novel patterns to persistent memory</role>
    <receives>implementation_summary, novel_patterns[], architectural_decisions[]</receives>
    <produces>memory_entries_created[], memory_paths[]</produces>
    <done_when>All non-obvious decisions and patterns captured; memory entries verified writable</done_when>
  </agent>
  <agent name="validator" subagent_type="validator" readonly="true" dispatch="on_demand">
    <role>Independently re-derive a disputed claim. Dispatch only when two agents disagree and their
      evidence does not settle it, or when a consequential claim rests on no citation — not as a
      routine step, since an independent pass costs materially more than the report it checks.</role>
    <receives>the disputed claim and its cited file:line or command output, without the originating agent's reasoning</receives>
    <produces>independent verdict with the evidence it rests on</produces>
    <done_when>The disputed claim is confirmed, overturned, or reported as unresolved with both positions</done_when>
  </agent>
</agents>
<execution_graph>
  <parallel_group id="quality_assurance" depends_on="none">
    <agent>quality</agent>
    <agent>security</agent>
  </parallel_group>
  <parallel_group id="implementation" depends_on="none">
    <agent>test</agent>
    <agent>docs</agent>
  </parallel_group>
  <sequential_step id="review_phase" depends_on="quality_assurance,implementation">
    <agent>review</agent>
    <reason>Requires completion of quality checks and implementation</reason>
  </sequential_step>
  <sequential_step id="persist_phase" depends_on="review_phase">
    <agent>memory</agent>
    <reason>Capture novel patterns and architectural decisions after all other phases complete</reason>
  </sequential_step>
</execution_graph>
<delegation>
  <requirement>Specific scope and expected deliverables</requirement>
  <requirement>Target file paths</requirement>
  <requirement>Reference implementations (specific paths)</requirement>
  <requirement>Memory check: `list_memories` for patterns</requirement>
</delegation>
<decision_criteria>
  <factor name="task_clarity" precedence="1">
    <unmet>The request admits two readings that would produce different implementations. Ask with
      AskUserQuestion before delegating; do not implement the cheaper reading.</unmet>
  </factor>
  <factor name="verification_completeness" precedence="2">
    <unmet>No test command was run against the change. Run it before claiming completion. If no command
      can be inferred from the project's manifests, report that as a blocker rather than completing
      unverified.</unmet>
  </factor>
  <factor name="implementation_quality" precedence="3">
    <unmet>A test failed, or get_diagnostics_for_file reports an error on a modified file. Delegate one
      targeted fix and re-run once; if it still fails, report the failure as a blocker in follow_up
      rather than completing.</unmet>
  </factor>
  <resolution>Apply in precedence order. The first factor whose `unmet` condition holds decides what
    happens next; later factors are not consulted.</resolution>
</decision_criteria>
<output>
  <format>
    <execution_result>
      <summary>What was implemented and why</summary>
      <changes>
        <change path="path/to/file">Description of targeted change</change>
      </changes>
      <verification>
        <check command="exact command run">Exit status and observed result</check>
        <test_execution>
          <command>test command used, or "none run" with the reason</command>
          <status>PASS / FAIL</status>
          <failures>failing test names if any</failures>
          <scope>What the command covered, and confirmation it saw the files created this session</scope>
        </test_execution>
      </verification>
      <follow_up>Remaining risks or next actions, if any</follow_up>
      <self_feedback>
        <evidence>Each claim above tagged per the evidence tiers in CLAUDE.md</evidence>
        <weakest_claim>The finding resting on the thinnest evidence, and what would confirm it</weakest_claim>
        <gaps>Anything asked for that was not done, and why — not attempted, blocked, or out of scope</gaps>
      </self_feedback>
    </execution_result>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="EXEC-B001" priority="important">
      <trigger>Before implementation</trigger>
      <action>Check Serena memories for existing patterns, so an existing convention is reused rather
        than duplicated</action>
      <verification>Pattern check in output</verification>
    </behavior>
    <behavior id="EXEC-B002" priority="important">
      <trigger>After implementation</trigger>
      <action>Delegate verification to the quality and security agents</action>
      <verification>Agent reports in output</verification>
    </behavior>
    <behavior id="EXEC-B003" priority="critical">
      <trigger>During implementation</trigger>
      <action>Delegate test creation to the test agent for all implemented functionality, using the
        acceptance criteria from /define output as targets</action>
      <verification>Test files created and listed in output</verification>
    </behavior>
    <behavior id="EXEC-B004" priority="critical">
      <trigger>After test creation, in the consolidate phase</trigger>
      <action>Run all test commands. On failure, delegate one targeted fix and re-run once; if failures
        remain, report them as blockers rather than completing silently</action>
      <verification>Test execution results in output: an all-pass confirmation or an explicit blocker list</verification>
    </behavior>
    <behavior id="EXEC-B005" priority="important">
      <trigger>Before modifying any existing symbol</trigger>
      <action>Use find_referencing_symbols to assess blast radius and include the reference count and
        affected files in the delegation prompt, because a caller missed here surfaces as a failure the
        fix iteration has no budget for</action>
      <verification>Blast radius assessment included in sub-agent instructions</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="EXEC-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Implementing without sub-agent delegation</action>
      <response>Block operation, delegate to specialized agents</response>
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
  <command name="define">When implementation reveals unclear requirements</command>
  <command name="ask">When implementation requires investigation</command>
  <command name="bug">When implementation encounters unexpected errors</command>
  <command name="feedback">Review work after execution completion</command>
  <command name="upstream">When preparing changes for upstream OSS contribution</command>
  <command name="execute-full">Full version with comprehensive multi-agent feedback loop (quality, security, design, docs, performance) plus fix phase; use when broad quality review beyond test self-healing is needed</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
</related_agents>
<related_skills>
  <skill name="execution-workflow">Core delegation and orchestration patterns; loaded in the prepare phase</skill>
  <skill name="serena-usage">Check memories for existing patterns before implementation</skill>
  <skill name="testing-patterns">Ensure proper test coverage</skill>
</related_skills>
<constraints>
  <must>Delegate detailed work to sub-agents and run independent tasks in parallel</must>
  <must>Write tests for all implemented functionality; skipping tests is not acceptable</must>
  <must>Run all test commands after test creation; attempt one fix for failures; report any remaining failures as blockers rather than silently completing</must>
  <must>Define done as an enumerated set of commands that exit zero — the project's test command, plus its lint, build, or type-check command where one exists — and report which of them actually ran</must>
  <must>When a mechanical gate rejects an edit — an additive-only check, a formatter, a lint rule — add a new sibling element rather than rewording the existing one. The original stays byte-identical and the new behavior stops depending on the old wording; reword only when a sibling would be genuinely redundant</must>
  <must>When aligning something with a reference implementation, treat alignment as one-directional for anything that fails closed: security gates, verification strictness, and fail-closed defaults move from the looser side to the stricter one and never the reverse. A strictness the reference lacks is an asset, not a divergence to erase</must>
  <avoid>Implementing detailed logic directly</avoid>
  <avoid>Unnecessary comments about past implementations</avoid>
  <avoid>Marking implementation complete without corresponding tests</avoid>
</constraints>
