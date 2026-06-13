---
name: execute
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
  </phase>
  <phase name="consolidate">
    <objective>Integrate sub-agent outputs into cohesive result</objective>
    <step order="1">
      <action>Verify sub-agent outputs for completeness</action>
      <tool>Output validation</tool>
      <output>Verified sub-agent results</output>
    </step>
    <step order="2">
      <action>Run test commands for all written tests; infer command from project language/framework</action>
      <tool>Bash (test runner)</tool>
      <output>Test execution results: pass/fail status, failing test names if any</output>
    </step>
    <step order="3">
      <action>If tests fail: delegate one targeted fix to test/general-purpose agent; re-run once to confirm; if still failing report as blockers</action>
      <tool>Sub-agent delegation (conditional)</tool>
      <output>All tests passing, or blocker report listing remaining failures</output>
    </step>
    <step order="4">
      <action>Combine all verified results and test outcomes into a cohesive final output</action>
      <tool>Synthesis</tool>
      <output>Consolidated result including test execution status</output>
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
  <agent name="docs" subagent_type="docs" readonly="false">
    <role>Update documentation to accurately reflect implementation changes</role>
    <receives>changed_files[], change_summary, doc_paths[], api_changes[]</receives>
    <produces>updated_doc_files[], new_doc_sections[], coverage_report{updated: int, missing: int}</produces>
    <done_when>All public interfaces and behavior changes documented; no stale references remain</done_when>
  </agent>
</agents>
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
    </execution_result>
  </format>
</output>
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
