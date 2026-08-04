---
argument-hint: [task-description]
description: Full task execution with feedback loop
---

<purpose>
Execute tasks with automatic feedback collection and conditional fix phase. Runs execute -> feedback -> fix issues (only if issues found) in a single automated flow.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">execution-workflow</skill>
  <skill use="tools">serena-usage</skill>
</refs>
<rules priority="critical">
  <rule>Delegate detailed work to specialized sub-agents</rule>
  <rule>Complete full cycle: execute -> feedback -> fix (conditional)</rule>
  <rule>Maximum one fix iteration (no infinite loops)</rule>
  <rule>Automatic flow between phases (no user confirmation)</rule>
  <rule>Skip fix phase if no issues found in feedback</rule>
  <rule>Write tests for all implemented functionality; test creation is mandatory, not optional</rule>
</rules>
<rules priority="standard">
  <rule>Use execution-workflow skill for delegation patterns</rule>
  <rule>Check Serena memories before implementation</rule>
  <rule>Fix only issues identified in feedback, not full re-implementation</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_orchestration" />
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Running the six feedback agents (quality, security, design, docs, performance, test) sequentially after implementation — all six evaluate independent dimensions of the same output and must run in parallel</practice>
    <practice>Triggering the fix phase unconditionally after feedback — the fix phase is conditional; if no issues are found, it must be skipped with an explicit skip confirmation, not executed as a no-op</practice>
    <practice>Performing full re-implementation in the fix phase — fixes must be targeted to the specific issues identified in feedback; broad rewrites during the fix phase indicate a planning failure</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Treat test execution failures from the execute phase as first-class issues that feed directly into the fix_issues phase; failures are not separate from the feedback loop</principle>
    <principle>Limit the entire cycle to exactly one fix iteration; a second pass signals that the initial implementation was too fragmented and requires user-level scope reduction, not more automation</principle>
    <principle>Flow automatically between all phases (execute → feedback → fix) without user confirmation; the value of execute-full over execute is the elimination of manual hand-off steps between phases</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
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

  <phase name="analyze_execute">
    <step order="1">
      <action>What tasks need to be done?</action>
      <output>Task list</output>
    </step>
    <step order="2">
      <action>Which sub-agents are best suited?</action>
      <output>Agent assignments</output>
    </step>
    <step order="3">
      <action>Which tasks can run in parallel?</action>
      <output>Parallel task groups</output>
    </step>
    <step order="4">
      <action>What dependencies exist between tasks?</action>
      <output>Dependency graph</output>
    </step>
    <step order="5">
      <action>What verification is needed?</action>
      <output>Verification checklist</output>
    </step>
    <step order="6">
      <action>Split into manageable units</action>
      <output>Atomic task list</output>
    </step>
    <step order="7">
      <action>Identify task boundaries</action>
      <output>Clear task scopes</output>
    </step>
    <step order="8">
      <action>Identify parallel vs sequential tasks</action>
      <output>Execution order</output>
    </step>
    <step order="9">
      <action>Define task dependencies</action>
      <output>Task dependency map</output>
    </step>
    <step order="10">
      <action>Delegate tasks with detailed instructions</action>
      <tool>Task</tool>
      <output>Delegated tasks</output>
    </step>
    <step order="11">
      <action>Provide context and constraints</action>
      <output>Contextual guidance</output>
    </step>
    <step order="12">
      <action>Verify sub-agent outputs</action>
      <output>Verification results</output>
    </step>
    <step order="13">
      <action>Combine results</action>
      <output>Consolidated implementation</output>
    </step>
    <step order="14">
      <action>Run test commands for all written tests; infer from project language/framework (pytest, go test, npm test, etc.); record pass/fail results</action>
      <tool>Bash (test runner)</tool>
      <output>Test execution results: pass/fail status, failing test names — feeds into collect_feedback and fix_issues</output>
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
      findings cite nothing checkable is a retry condition, not a clean result
      (parallelization-patterns#retry_policy).</check>
    <check>Name the issues classified critical and the runtime impact that makes each one critical,
      or state that none are.</check>
    <on_unmet>Re-run the named agent once with a narrower prompt naming the specific files. If it fails
      again, review that dimension here and report that the delegation failed.</on_unmet>
  </reflection_checkpoint>

  <phase name="fix_issues">
    <step order="1">
      <action>Synthesize feedback from all agents; include test execution failures from analyze_execute step 14 as issues in the consolidated list</action>
      <output>Consolidated issue list including test failures from execution phase</output>
    </step>
    <step order="2">
      <action>Prioritize issues by severity (critical > warning > info)</action>
      <output>Prioritized issue list</output>
    </step>
    <step order="3">
      <action>Delegate fixes to appropriate sub-agents</action>
      <tool>Task</tool>
      <output>Fix assignments</output>
    </step>
    <step order="4">
      <action>Verify fixes address the identified issues</action>
      <output>Verification results</output>
    </step>
    <step order="5">
      <action>Consolidate fix results</action>
      <output>Fixed implementation</output>
    </step>
  </phase>

  <iteration_limit>1</iteration_limit>
  <reflection_checkpoint id="fix_complete" after="fix_issues">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name each critical issue from the consolidated list and the file:line of the change that
      addresses it, or the reason it was deferred.</check>
    <check>Name the commands re-run after the fixes and their exit status. A fix that was never
      re-verified is not a fix.</check>
    <check>Name every issue left unaddressed, including warnings judged infeasible, and why.</check>
    <on_unmet>Report the unaddressed issues to the user as deferred, with the reason. Do not open a
      second fix iteration (EXECF-P002).</on_unmet>
  </reflection_checkpoint>

  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Handle execution errors and apply fallback strategy</action>
      <output>Recovered execution path or documented blocker</output>
    </step>
  </phase>
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
  <agent name="git" subagent_type="git" readonly="false">
    <role>Design branching strategy, commit structure, and merge workflows</role>
    <receives>change_scope, team_workflow, target_branches[], parallel_isolation_required: true</receives>
    <produces>branch_strategy, commit_plan[], pr_description_template</produces>
    <done_when>Branch strategy aligned with team workflow; commit history logical and reviewable</done_when>
    <constraint>Never use git stash, git checkout [branch], git reset --hard, or git clean.
      For branch isolation use git worktree add. Follow core-patterns#parallel_project_isolation.</constraint>
  </agent>
  <agent name="memory" subagent_type="general-purpose" readonly="false">
    <role>Capture significant architectural decisions and novel patterns to persistent memory; verify freshness of memories consulted during this task</role>
    <receives>implementation_summary, novel_patterns[], architectural_decisions[], memories_read_this_task[]</receives>
    <produces>memory_entries_created[], memory_paths[], memories_verified[]</produces>
    <done_when>All non-obvious decisions and patterns captured; memory entries verified writable; memories_read_this_task checked for staleness</done_when>
    <constraint>For each write_memory call: prepend memory_content_format frontmatter (serena-usage skill)
      with domain, status=active, created=YYYY-MM, last-verified=YYYY-MM.
      For edit_memory on a memory lacking frontmatter: add it, updating last-verified.
      Apply memory_staleness_verification (serena-usage skill) to memories_read_this_task: bump last-verified
      if still accurate, correct if partially outdated, or rename_memory with an -archived suffix if superseded.</constraint>
  </agent>
  <agent name="validator" subagent_type="validator" readonly="true">
    <role>Cross-validate findings from multiple agents to detect contradictions and confirm consensus</role>
    <receives>agent_reports[], implementation_claims[], expected_outcomes[]</receives>
    <produces>consensus_report{agreed: [], disputed: [], unevidenced: []}, contradiction_flags[]</produces>
    <done_when>All agent outputs cross-checked; contradictions resolved or flagged for user review</done_when>
  </agent>
  <agent name="design" subagent_type="design" readonly="true">
    <role>Evaluate architecture consistency and design quality of implemented changes</role>
    <receives>changed_files[], architecture_context, design_principles[]</receives>
    <produces>design_assessment{violations[], improvements[]}, consistency_report</produces>
    <done_when>Architecture consistency verified; all design principle violations documented</done_when>
  </agent>
</agents>
<execution_graph>
  <sequential_phase id="execute" depends_on="none">
    <parallel_group id="quality_assurance">
      <agent>quality</agent>
      <agent>security</agent>
    </parallel_group>
    <parallel_group id="implementation">
      <agent>test</agent>
      <agent>docs</agent>
    </parallel_group>
    <sequential_step id="consolidation">
      <action>Consolidate initial execution results</action>
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
<decision_criteria inherits="core-patterns#decision_criteria">
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
      </tests_status>
      <verification>Every command run and its exit status, or "none run" — never omitted</verification>
    </execution_results>
    <feedback_summary>
      <agent_reports>
        <report agent="quality|security|design|docs|performance|test">
          <returned>completed / timed out / returned nothing checkable</returned>
          <evidence_tier>verified|inferred|assumed — verified requires a file:line or command output</evidence_tier>
        </report>
      </agent_reports>
      <issues_found>
        <critical>
          <issue>
            <category>Category</category>
            <description>Issue description</description>
            <location>File and line reference</location>
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
          <fix>How it was fixed</fix>
          <status>Fixed/Deferred</status>
        </issue>
      </issues_addressed>
      <deferred_issues>
        <issue>
          <description>Issue not fixed</description>
          <reason>Justification for deferral</reason>
        </issue>
      </deferred_issues>
    </fix_results>
    <skip_confirmation condition="if no issues found">
      <message>No issues requiring fixes were identified in feedback phase</message>
      <status>Fix phase skipped</status>
    </skip_confirmation>
    <final_status>
      <status>success|warning|error per core-patterns#status_determination</status>
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
    <behavior id="EXECF-B001" priority="critical">
      <trigger>Before implementation</trigger>
      <action>Check Serena memories for existing patterns</action>
      <verification>Pattern check in output</verification>
    </behavior>
    <behavior id="EXECF-B002" priority="critical">
      <trigger>After initial execution</trigger>
      <action>Execute feedback collection phase automatically</action>
      <verification>Feedback results in output</verification>
    </behavior>
    <behavior id="EXECF-B003" priority="critical">
      <trigger>After feedback collection</trigger>
      <action>Evaluate if issues require fix phase</action>
      <verification>Issue evaluation in output</verification>
    </behavior>
    <behavior id="EXECF-B004" priority="critical">
      <trigger>When issues found</trigger>
      <action>Execute fix phase for identified issues only</action>
      <verification>Fix results in output</verification>
    </behavior>
    <behavior id="EXECF-B005" priority="critical">
      <trigger>When no issues found</trigger>
      <action>Skip fix phase with confirmation message</action>
      <verification>Skip confirmation in output</verification>
    </behavior>
    <behavior id="EXECF-B006" priority="critical">
      <trigger>During feedback phase</trigger>
      <action>Launch all feedback agents in parallel</action>
      <verification>Parallel execution confirmed</verification>
    </behavior>
    <behavior id="EXECF-B007" priority="critical">
      <trigger>During execute phase</trigger>
      <action>Delegate test creation to test agent for all implemented functionality; use acceptance criteria from /define output as test targets</action>
      <verification>Test files created and listed in output</verification>
    </behavior>
    <behavior id="EXECF-B008" priority="critical">
      <trigger>After test creation in execute phase (analyze_execute step 14)</trigger>
      <action>Run all test commands; failures are treated as issues and feed into fix_issues phase</action>
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
      <action>Multiple fix iterations</action>
      <response>Block operation, maximum one fix iteration allowed</response>
    </behavior>
    <behavior id="EXECF-P003" priority="critical">
      <trigger>Between phases</trigger>
      <action>Requesting user confirmation to proceed</action>
      <response>Proceed automatically between phases</response>
    </behavior>
    <behavior id="EXECF-P004" priority="critical">
      <trigger>In fix phase</trigger>
      <action>Full re-implementation instead of targeted fixes</action>
      <response>Fix only identified issues from feedback</response>
    </behavior>
    <behavior id="EXECF-P005" priority="critical">
      <trigger>When no issues found</trigger>
      <action>Executing fix phase unnecessarily</action>
      <response>Skip fix phase when feedback shows no issues</response>
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
  <must>Complete all phases: execute, feedback, fix (conditional)</must>
  <must>Automatically proceed between phases without user confirmation</must>
  <must>Skip fix phase when no issues found</must>
  <must>Limit to maximum one fix iteration</must>
  <must>Write tests for all implemented functionality; skipping tests is not acceptable</must>
  <must>Run all test commands after test creation; failures are treated as fix-phase issues</must>
  <must>Define done as an enumerated set of commands that exit zero — the project's test command, plus its lint, build, or type-check command where one exists — and report which of them actually ran with its exit status. A completion claim naming no command is not a completion claim</must>
  <must>Resolve disagreement between feedback agents by what each one examined (parallelization-patterns#agent_precedence): a report citing a file:line or command output outranks one reasoning from convention, whatever the specialty. Retry an agent whose report cites nothing checkable</must>
  <avoid>Implementing detailed logic directly</avoid>
  <avoid>Multiple fix iterations (exactly one allowed when needed)</avoid>
  <avoid>Sequential execution of independent feedback agents</avoid>
  <avoid>Full re-implementation in fix phase</avoid>
  <avoid>Requesting user confirmation between phases</avoid>
  <avoid>Marking implementation complete without corresponding tests</avoid>
</constraints>
