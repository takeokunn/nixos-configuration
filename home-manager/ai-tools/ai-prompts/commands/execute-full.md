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
      <tool>Task analysis</tool>
      <output>Task list</output>
    </step>
    <step order="2">
      <action>Which sub-agents are best suited?</action>
      <tool>Agent selection</tool>
      <output>Agent assignments</output>
    </step>
    <step order="3">
      <action>Which tasks can run in parallel?</action>
      <tool>Dependency analysis</tool>
      <output>Parallel task groups</output>
    </step>
    <step order="4">
      <action>What dependencies exist between tasks?</action>
      <tool>Dependency mapping</tool>
      <output>Dependency graph</output>
    </step>
    <step order="5">
      <action>What verification is needed?</action>
      <tool>Verification planning</tool>
      <output>Verification checklist</output>
    </step>
    <step order="6">
      <action>Split into manageable units</action>
      <tool>Task decomposition</tool>
      <output>Atomic task list</output>
    </step>
    <step order="7">
      <action>Identify task boundaries</action>
      <tool>Boundary analysis</tool>
      <output>Clear task scopes</output>
    </step>
    <step order="8">
      <action>Identify parallel vs sequential tasks</action>
      <tool>Execution planning</tool>
      <output>Execution order</output>
    </step>
    <step order="9">
      <action>Define task dependencies</action>
      <tool>Dependency definition</tool>
      <output>Task dependency map</output>
    </step>
    <step order="10">
      <action>Delegate tasks with detailed instructions</action>
      <tool>Sub-agent delegation</tool>
      <output>Delegated tasks</output>
    </step>
    <step order="11">
      <action>Provide context and constraints</action>
      <tool>Context provision</tool>
      <output>Contextual guidance</output>
    </step>
    <step order="12">
      <action>Verify sub-agent outputs</action>
      <tool>Output verification</tool>
      <output>Verification results</output>
    </step>
    <step order="13">
      <action>Combine results</action>
      <tool>Result integration</tool>
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
      <tool>Sub-agent delegation (quality-assurance)</tool>
      <output>Quality evaluation report</output>
    </step>
    <step order="2">
      <action>Launch security agent: vulnerability detection</action>
      <tool>Sub-agent delegation (security)</tool>
      <output>Security evaluation report</output>
    </step>
    <step order="3">
      <action>Launch design agent: architecture consistency</action>
      <tool>Sub-agent delegation (design)</tool>
      <output>Design evaluation report</output>
    </step>
    <step order="4">
      <action>Launch docs agent: documentation completeness</action>
      <tool>Sub-agent delegation (docs)</tool>
      <output>Documentation evaluation report</output>
    </step>
    <step order="5">
      <action>Launch performance agent: performance implications</action>
      <tool>Sub-agent delegation (performance)</tool>
      <output>Performance evaluation report</output>
    </step>
    <step order="6">
      <action>Launch test agent: test coverage analysis</action>
      <tool>Sub-agent delegation (test)</tool>
      <output>Test evaluation report</output>
    </step>
  </phase>

  <reflection_checkpoint id="feedback_quality" after="collect_feedback">
      <questions>
        <question weight="0.4">Did all feedback agents complete successfully?</question>
        <question weight="0.3">Is the feedback specific and actionable?</question>
        <question weight="0.3">Are there critical issues requiring fixes?</question>
      </questions>
      <threshold min="70" action="proceed">
        <below_threshold>Re-run failed agents or gather additional context</below_threshold>
      </threshold>
    </reflection_checkpoint>

  <phase name="fix_issues">
    <step order="1">
      <action>Synthesize feedback from all agents; include test execution failures from analyze_execute step 14 as issues in the consolidated list</action>
      <tool>Feedback synthesis</tool>
      <output>Consolidated issue list including test failures from execution phase</output>
    </step>
    <step order="2">
      <action>Prioritize issues by severity (critical > warning > info)</action>
      <tool>Issue prioritization</tool>
      <output>Prioritized issue list</output>
    </step>
    <step order="3">
      <action>Delegate fixes to appropriate sub-agents</action>
      <tool>Sub-agent delegation</tool>
      <output>Fix assignments</output>
    </step>
    <step order="4">
      <action>Verify fixes address the identified issues</action>
      <tool>Fix verification</tool>
      <output>Verification results</output>
    </step>
    <step order="5">
      <action>Consolidate fix results</action>
      <tool>Result consolidation</tool>
      <output>Fixed implementation</output>
    </step>
  </phase>

  <iteration_limit>1</iteration_limit>
  <reflection_checkpoint id="fix_complete" after="fix_issues">
      <questions>
        <question weight="0.5">Have all critical issues been addressed?</question>
        <question weight="0.3">Have warning issues been addressed where feasible?</question>
        <question weight="0.2">Are any issues deferred with clear justification?</question>
      </questions>
      <threshold min="70" action="complete">
        <below_threshold>Flag remaining issues for user review</below_threshold>
      </threshold>
    </reflection_checkpoint>

  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Handle execution errors and apply fallback strategy</action>
      <tool>Error analysis and retry policy</tool>
      <output>Recovered execution path or documented blocker</output>
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
    <constraint>For each write_memory call: prepend memory_content_format frontmatter (serena-usage skill)
      with domain, status=active, created=YYYY-MM, last-verified=YYYY-MM.
      For edit_memory on a memory lacking frontmatter: add it, updating last-verified.</constraint>
  </agent>
  <agent name="validator" subagent_type="validator" readonly="true">
    <role>Cross-validate findings from multiple agents to detect contradictions and confirm consensus</role>
    <receives>agent_reports[], implementation_claims[], expected_outcomes[]</receives>
    <produces>consensus_report{agreed: [], disputed: [], confidence: 0-100}, contradiction_flags[]</produces>
    <done_when>All agent outputs cross-checked; contradictions resolved or flagged for user review</done_when>
  </agent>
  <agent name="design" subagent_type="design" readonly="true">
    <role>Evaluate architecture consistency and design quality of implemented changes</role>
    <receives>changed_files[], architecture_context, design_principles[]</receives>
    <produces>design_assessment{score: 0-100, violations[], improvements[]}, consistency_report</produces>
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
      <pass_forward>consolidated_issues[], confidence_scores{agent: score}, test_failures[]</pass_forward>
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
  <criterion name="confidence_calculation">
    <factor name="task_clarity" weight="0.2">
      <score range="90-100">Clear requirements with acceptance criteria</score>
      <score range="70-89">Clear requirements</score>
      <score range="50-69">Some ambiguity</score>
      <score range="0-49">Unclear requirements</score>
    </factor>
    <factor name="implementation_quality" weight="0.3">
      <score range="90-100">All tests pass, code reviewed</score>
      <score range="70-89">Tests pass</score>
      <score range="50-69">Some issues remain</score>
      <score range="0-49">Major issues</score>
    </factor>
    <factor name="feedback_severity" weight="0.25">
      <score range="90-100">No issues found</score>
      <score range="70-89">Info-level issues only</score>
      <score range="50-69">Warning-level issues</score>
      <score range="0-49">Critical issues</score>
    </factor>
    <factor name="fix_completeness" weight="0.25">
      <score range="90-100">All issues fixed and verified</score>
      <score range="70-89">Critical issues fixed</score>
      <score range="50-69">Some issues remain</score>
      <score range="0-49">Major issues unfixed</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="no_issues_found">
      <input>task_clarity=90, implementation_quality=95, feedback_severity=95, fix_completeness=100</input>
      <calculation>(90*0.2)+(95*0.3)+(95*0.25)+(100*0.25) = 95.25</calculation>
      <expected_status>success</expected_status>
      <expected_fix_phase>skipped</expected_fix_phase>
      <reasoning>No issues found, fix phase skipped, high confidence</reasoning>
    </test>
    <test name="issues_fixed_success">
      <input>task_clarity=85, implementation_quality=80, feedback_severity=60, fix_completeness=90</input>
      <calculation>(85*0.2)+(80*0.3)+(60*0.25)+(90*0.25) = 78.5</calculation>
      <expected_status>success</expected_status>
      <expected_fix_phase>executed</expected_fix_phase>
      <reasoning>Issues found and fixed, acceptable confidence</reasoning>
    </test>
    <test name="boundary_success_70">
      <input>task_clarity=70, implementation_quality=70, feedback_severity=70, fix_completeness=70</input>
      <calculation>(70*0.2)+(70*0.3)+(70*0.25)+(70*0.25) = 70</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly at success threshold</reasoning>
    </test>
    <test name="boundary_warning_69">
      <input>task_clarity=69, implementation_quality=69, feedback_severity=69, fix_completeness=69</input>
      <calculation>(69*0.2)+(69*0.3)+(69*0.25)+(69*0.25) = 69</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Below success threshold, flag for review</reasoning>
    </test>
    <test name="critical_issues_unfixed">
      <input>task_clarity=80, implementation_quality=70, feedback_severity=30, fix_completeness=40</input>
      <calculation>(80*0.2)+(70*0.3)+(30*0.25)+(40*0.25) = 54.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Critical issues remain unfixed</reasoning>
    </test>
  </validation_tests>
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
        <command>test command used</command>
        <status>Pass/Fail</status>
        <failures>failing test names if any</failures>
      </tests_status>
    </execution_results>
    <feedback_summary>
      <evaluation_scores>
        <metric name="quality">XX/100</metric>
        <metric name="security">XX/100</metric>
        <metric name="design">XX/100</metric>
        <metric name="documentation">XX/100</metric>
        <metric name="performance">XX/100</metric>
        <metric name="test_coverage">XX/100</metric>
        <metric name="overall">XX/100</metric>
      </evaluation_scores>
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
      <confidence>XX/100</confidence>
      <summary>Final status summary</summary>
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
  <avoid>Implementing detailed logic directly</avoid>
  <avoid>Multiple fix iterations (exactly one allowed when needed)</avoid>
  <avoid>Sequential execution of independent feedback agents</avoid>
  <avoid>Full re-implementation in fix phase</avoid>
  <avoid>Requesting user confirmation between phases</avoid>
  <avoid>Marking implementation complete without corresponding tests</avoid>
</constraints>
