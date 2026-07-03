---
argument-hint: [task-description]
description: Task execution command
---

<purpose>
Execute tasks by delegating detailed work to sub-agents while focusing on policy decisions and orchestration. Includes test self-healing: if written tests fail, one targeted fix attempt is made before completion. For comprehensive multi-agent quality review across all dimensions, use /execute-full.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">execution-workflow</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<rules priority="critical">
  <rule>Delegate detailed work to specialized sub-agents</rule>
  <rule>Focus on orchestration and policy decisions</rule>
  <rule>Execute independent tasks in parallel</rule>
  <rule>Verify sub-agent outputs before integration</rule>
  <rule>Write tests for all implemented functionality; test creation is mandatory, not optional</rule>
  <rule>Maximum one fix iteration for test failures in consolidate phase; report remaining failures as blockers</rule>
</rules>
<rules priority="standard">
  <rule>Use execution-workflow skill for delegation patterns</rule>
  <rule>Check Serena memories before implementation</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_orchestration" />
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Implementing all logic in a single agent pass without delegation — every specialized concern (quality, security, tests, docs) must be delegated to the appropriate sub-agent with full context and explicit deliverables</practice>
    <practice>Treating test creation as optional or deferrable — tests are mandatory for all implemented functionality; no implementation is considered complete without corresponding tests</practice>
    <practice>Silently completing when tests fail after a fix attempt — remaining test failures after one targeted fix must be surfaced as explicit blockers in the follow_up section, not hidden</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Decompose tasks into atomic units with clear boundaries before delegating; independent units run in parallel while dependent units run sequentially — never serialize what can be parallelized</principle>
    <principle>Check Serena memories for existing patterns before every implementation; implementing what already exists as a utility or convention wastes effort and creates divergence</principle>
    <principle>After test creation, run the inferred test command immediately; if tests fail, attempt exactly one targeted fix and re-run; report any remaining failures as blockers rather than suppressing them</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <objective>Initialize Serena and load task-type-appropriate patterns</objective>
    <step order="1">
      <action>Activate Serena project with activate_project</action>
      <tool>Serena activate_project</tool>
      <output>Project activated</output>
    </step>
    <step order="2">
      <action>Check list_memories for relevant patterns</action>
      <tool>Serena list_memories</tool>
      <output>Full memory index</output>
    </step>
    <step order="3">
      <action>Classify task type as "implementation". Apply memory_reading_by_task_type filter
        (serena-usage skill): prioritize {feature}-patterns → {language}-conventions → testing-patterns.
        Filter the memory index from step 2 against these categories; record matched names.</action>
      <tool>serena-usage#memory_reading_by_task_type (reference only)</tool>
      <output>Filtered priority memory list for implementation tasks</output>
    </step>
    <step order="4">
      <action>Load only memories matching the prioritized categories with read_memory;
        skip categories absent from the index</action>
      <tool>Serena read_memory</tool>
      <output>Prioritized patterns loaded</output>
    </step>
  </phase>
  <phase name="analyze">
    <objective>Understand the task scope and identify required resources</objective>
    <step order="1">
      <action>Identify concrete tasks that need to be completed</action>
      <tool>Task decomposition analysis</tool>
      <output>Task inventory</output>
    </step>
    <step order="2">
      <action>Select best-fit sub-agents for each task</action>
      <tool>Agent capability mapping</tool>
      <output>Delegation map</output>
    </step>
    <step order="3">
      <action>Identify which tasks can run in parallel</action>
      <tool>Dependency analysis</tool>
      <output>Parallelization plan</output>
    </step>
    <step order="4">
      <action>Determine task dependencies and execution order</action>
      <tool>Dependency graphing</tool>
      <output>Ordered dependency chain</output>
    </step>
    <step order="5">
      <action>Define verification requirements for completion</action>
      <tool>Quality gate planning</tool>
      <output>Verification checklist</output>
    </step>
  </phase>
  <phase name="decompose">
    <objective>Break down complex tasks into manageable units</objective>
    <step order="1">
      <action>Split work into manageable units</action>
      <tool>Task decomposition</tool>
      <output>Atomic task units</output>
    </step>
    <step order="2">
      <action>Define boundaries for each task unit</action>
      <tool>Scope boundary analysis</tool>
      <output>Task boundaries</output>
    </step>
  </phase>
  <phase name="structure">
    <objective>Organize tasks for optimal execution</objective>
    <step order="1">
      <action>Classify tasks as parallel or sequential</action>
      <tool>Execution strategy analysis</tool>
      <output>Execution classification</output>
    </step>
    <step order="2">
      <action>Define dependencies between structured tasks</action>
      <tool>Dependency mapping</tool>
      <output>Dependency matrix</output>
    </step>
  </phase>
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
  <phase name="assign">
    <objective>Delegate tasks to appropriate sub-agents with clear instructions</objective>
    <step order="0">
      <action>For tasks that modify existing symbols: call find_referencing_symbols (Serena) to assess blast radius;
        embed reference count and affected file list into the delegation prompt (EXEC-B005)</action>
      <tool>Serena find_referencing_symbols</tool>
      <output>Blast radius: N references in M files; included in delegation context</output>
    </step>
    <step order="1">
      <action>Delegate tasks with detailed instructions</action>
      <tool>Task orchestration</tool>
      <output>Delegation requests issued</output>
    </step>
    <step order="2">
      <action>Provide complete context and constraints to assignees</action>
      <tool>Context packaging</tool>
      <output>Execution-ready delegation context</output>
    </step>
  </phase>
  <reflection_checkpoint id="assignment_complete" after="assign">
    <questions>
      <question weight="0.4">Have all tasks been properly delegated?</question>
      <question weight="0.3">Are the sub-agent instructions clear?</question>
      <question weight="0.3">Are dependencies between tasks handled?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Refine task assignments or ask user</below_threshold>
    </threshold>
  </reflection_checkpoint>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Handle execution errors and apply fallback strategy</action>
      <tool>Error analysis and retry policy</tool>
      <output>Recovered execution path or documented blocker</output>
    </step>
  </phase>
  <phase name="consolidate">
    <objective>Integrate sub-agent outputs into cohesive result</objective>
    <step order="1">
      <action>Verify sub-agent outputs for completeness; call get_diagnostics_for_file (min_severity=2)
        on each modified file to catch LSP errors before test execution</action>
      <tool>Output validation, Serena get_diagnostics_for_file</tool>
      <output>Verified sub-agent results; any LSP errors reported as blockers</output>
    </step>
    <step order="2">
      <action>Run test commands for all written tests; infer command from project language/framework (pytest, go test, npm test, etc.); if command cannot be inferred, check package.json/Makefile/pyproject.toml/go.mod; if still undetermined, report as blocker</action>
      <tool>Bash (test runner)</tool>
      <output>Test execution results: pass/fail status, failing test names if any</output>
    </step>
    <step order="3">
      <action>If tests fail: delegate one targeted fix to test/general-purpose agent for specific failing tests; re-run once to confirm; if still failing after one attempt, report remaining failures as blockers in follow_up and mark status FAIL</action>
      <tool>Sub-agent delegation (conditional)</tool>
      <output>All tests passing, or blocker report listing remaining failures</output>
    </step>
    <step order="4">
      <action>Combine all verified results and test outcomes into a cohesive final output</action>
      <tool>Synthesis</tool>
      <output>Consolidated result including test execution status</output>
    </step>
  </phase>
  <!-- persist phase: orchestrator-synthesized insights visible only after all agents complete.
       The memory agent in execution_graph captures patterns sourced from implementation sub-agents.
       These two mechanisms are complementary: agent-sourced findings → memory agent; orchestrator-level synthesis → this phase. -->
  <phase name="persist">
    <objective>Capture orchestrator-level synthesis insights to Serena memory</objective>
    <step order="1">
      <action>Evaluate each trigger in memory_auto_creation_triggers (serena-usage skill):
        architectural pattern / feature pattern / user-stated convention / refactoring approach.
        Call list_memories to check if a memory for this topic already exists.</action>
      <tool>Serena list_memories, evaluation against trigger list</tool>
      <output>Trigger match: yes/no for each; existing memory: yes/no</output>
    </step>
    <step order="2">
      <action>If trigger matched: use edit_memory (existing topic) or write_memory (new topic)
        following memory_lifecycle convention ({topic}-YYYY-MM or {topic}-patterns).
        For write_memory: prepend memory_content_format frontmatter (serena-usage skill)
        with domain, status=active, created=YYYY-MM, last-verified=YYYY-MM.
        For edit_memory on a memory lacking frontmatter: add it at that time, updating last-verified.
        If no triggers matched: output "persist: no triggers matched — skip"</action>
      <tool>Serena edit_memory or write_memory</tool>
      <output>Memory entries updated with frontmatter (names listed), or explicit skip reason</output>
    </step>
    <step order="3">
      <action>Apply memory_staleness_verification (serena-usage skill) to memories loaded in the prepare phase (step 4): bump last-verified if still accurate, correct if partially outdated, or rename_memory with an -archived suffix if fully superseded. Do not read additional memories solely for this check.</action>
      <tool>Serena edit_memory, rename_memory</tool>
      <output>Verified/updated/archived memories noted, or "no memories read this task required verification"</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="quality" subagent_type="quality-assurance" readonly="false">
    <role>Verify syntax correctness, type safety, and code format compliance for implemented changes</role>
    <receives>file_paths[], change_description, project_language, style_config_path</receives>
    <produces>issues[]{severity: critical|warning|info, location: file:line, message, suggestion}, confidence: 0-100</produces>
    <done_when>All modified files checked; no critical issues remain or all critical issues documented</done_when>
  </agent>
  <agent name="security" subagent_type="security" readonly="false">
    <role>Detect security vulnerabilities introduced by the implementation</role>
    <receives>file_paths[], change_description, threat_context</receives>
    <produces>vulnerabilities[]{severity: critical|high|medium|low, cwe, location: file:line, description, remediation}, risk_score: 0-100</produces>
    <done_when>All security-relevant code paths analyzed; OWASP top-10 categories checked for applicable patterns</done_when>
  </agent>
  <agent name="test" subagent_type="test" readonly="false">
    <role>Create comprehensive tests covering all implemented functionality and edge cases</role>
    <receives>implementation_files[], acceptance_criteria[], existing_test_paths[], test_framework</receives>
    <produces>test_files_created[], test_cases[]{name, type: unit|integration|e2e, coverage_target}, test_run_command</produces>
    <done_when>Tests written for all acceptance criteria; test_run_command confirmed executable</done_when>
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
    <produces>review_summary{score: 0-100, critical_findings[], warnings[], commendations[]}, go_no_go: bool</produces>
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
  <agent name="git" subagent_type="git" readonly="false">
    <role>Design branching strategy, commit structure, and merge workflows</role>
    <receives>change_scope, team_workflow, target_branches[], parallel_isolation_required: true</receives>
    <produces>branch_strategy, commit_plan[], pr_description_template</produces>
    <done_when>Branch strategy aligned with team workflow; commit history logical and reviewable</done_when>
    <constraint>Never use git stash, git checkout [branch], git reset --hard, or git clean.
      For branch isolation use git worktree add. Follow core-patterns#parallel_project_isolation.</constraint>
  </agent>
  <agent name="memory" subagent_type="general-purpose" readonly="false">
    <role>Capture significant architectural decisions and novel patterns to persistent memory</role>
    <receives>implementation_summary, novel_patterns[], architectural_decisions[]</receives>
    <produces>memory_entries_created[], memory_paths[]</produces>
    <done_when>All non-obvious decisions and patterns captured; memory entries verified writable</done_when>
  </agent>
  <agent name="validator" subagent_type="validator" readonly="true">
    <role>Cross-validate findings from multiple agents to detect contradictions and confirm consensus</role>
    <receives>agent_reports[], implementation_claims[], expected_outcomes[]</receives>
    <produces>consensus_report{agreed: [], disputed: [], confidence: 0-100}, contradiction_flags[]</produces>
    <done_when>All agent outputs cross-checked; contradictions resolved or flagged for user review</done_when>
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
<decision_criteria inherits="core-patterns#decision_criteria">
  <criterion name="confidence_calculation">
    <factor name="task_clarity" weight="0.3">
      <score range="90-100">Clear requirements with acceptance criteria</score>
      <score range="70-89">Clear requirements</score>
      <score range="50-69">Some ambiguity</score>
      <score range="0-49">Unclear requirements</score>
    </factor>
    <factor name="implementation_quality" weight="0.4">
      <score range="90-100">All tests pass, code reviewed</score>
      <score range="70-89">Tests pass</score>
      <score range="50-69">Some issues remain</score>
      <score range="0-49">Major issues</score>
    </factor>
    <factor name="verification_completeness" weight="0.3">
      <score range="90-100">Full verification by sub-agents</score>
      <score range="70-89">Core verification done</score>
      <score range="50-69">Partial verification</score>
      <score range="0-49">Minimal verification</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="success_case">
      <input>task_clarity=95, implementation_quality=90, verification_completeness=90</input>
      <calculation>(95*0.3)+(90*0.4)+(90*0.3) = 91.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>task_clarity=80, implementation_quality=80, verification_completeness=80</input>
      <calculation>(80*0.3)+(80*0.4)+(80*0.3) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>task_clarity=79, implementation_quality=79, verification_completeness=79</input>
      <calculation>(79*0.3)+(79*0.4)+(79*0.3) = 79</calculation>
      <expected_status>warning</expected_status>
      <reasoning>79 is below success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>task_clarity=59, implementation_quality=59, verification_completeness=59</input>
      <calculation>(59*0.3)+(59*0.4)+(59*0.3) = 59</calculation>
      <expected_status>error</expected_status>
      <reasoning>59 is at error threshold</reasoning>
    </test>
    <test name="error_case">
      <input>task_clarity=40, implementation_quality=50, verification_completeness=45</input>
      <calculation>(40*0.3)+(50*0.4)+(45*0.3) = 45.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores yield error status</reasoning>
    </test>
  </validation_tests>
</decision_criteria>
<output>
  <format>
    <execution_result>
      <summary>What was implemented and why</summary>
      <changes>
        <change path="path/to/file">Description of targeted change</change>
      </changes>
      <verification>
        <check command="command run">Observed result</check>
        <test_execution>
          <command>test command used</command>
          <status>PASS / FAIL</status>
          <failures>failing test names if any</failures>
        </test_execution>
      </verification>
      <follow_up>Remaining risks or next actions, if any</follow_up>
      <self_feedback>
        <confidence>XX/100</confidence>
        <dimension name="task_clarity">XX/100: one-line rationale</dimension>
        <dimension name="implementation_quality">XX/100: one-line rationale</dimension>
        <dimension name="verification_completeness">XX/100: one-line rationale</dimension>
        <gaps>What additional verification or context would raise confidence</gaps>
      </self_feedback>
    </execution_result>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="EXEC-B001" priority="critical">
      <trigger>Before implementation</trigger>
      <action>Check Serena memories for existing patterns</action>
      <verification>Pattern check in output</verification>
    </behavior>
    <behavior id="EXEC-B002" priority="critical">
      <trigger>After implementation</trigger>
      <action>Delegate verification to quality and security agents</action>
      <verification>Agent reports in output</verification>
    </behavior>
    <behavior id="EXEC-B003" priority="critical">
      <trigger>During implementation</trigger>
      <action>Delegate test creation to test agent for all implemented functionality; use acceptance criteria from /define output as test targets</action>
      <verification>Test files created and listed in output</verification>
    </behavior>
    <behavior id="EXEC-B004" priority="critical">
      <trigger>After test creation in consolidate phase (step 2)</trigger>
      <action>Run all test commands; if any fail, delegate one targeted fix then re-run once; if still failing, report as blockers — do not silently complete with failing tests</action>
      <verification>Test execution results in output; either all-pass confirmation or explicit blocker list</verification>
    </behavior>
    <behavior id="EXEC-B005" priority="critical">
      <trigger>Before modifying any existing symbol (function, class, method)</trigger>
      <action>Use Serena find_referencing_symbols to assess blast radius;
        include reference count and affected files in delegation prompt to sub-agents</action>
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
<error_escalation inherits="core-patterns#error_escalation">
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
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="execution-workflow">Core delegation and orchestration patterns</skill>
  <skill name="serena-usage">Check memories for existing patterns before implementation</skill>
  <skill name="testing-patterns">Ensure proper test coverage</skill>
</related_skills>
<constraints>
  <must>Delegate detailed work to sub-agents</must>
  <must>Execute independent tasks in parallel</must>
  <must>Verify outputs before integration</must>
  <must>Write tests for all implemented functionality; skipping tests is not acceptable</must>
  <must>Run all test commands after test creation; attempt one fix for failures; report any remaining failures as blockers rather than silently completing</must>
  <avoid>Implementing detailed logic directly</avoid>
  <avoid>Unnecessary comments about past implementations</avoid>
  <avoid>Marking implementation complete without corresponding tests</avoid>
</constraints>
