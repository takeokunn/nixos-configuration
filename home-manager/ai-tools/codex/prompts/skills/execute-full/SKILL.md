---
name: execute-full
description: Full task execution with feedback loop
---

<purpose>
Extended task execution that includes a comprehensive parallel feedback review after implementation, followed by a conditional targeted fix. Produces higher-confidence implementations than /execute by incorporating multi-dimensional review before declaring completion. For simple tasks, use /execute.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="patterns">parallelization-patterns</skill>
  <skill use="workflow">execution-workflow</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<rules priority="critical">
  <rule>Delegate detailed work to specialized sub-agents</rule>
  <rule>Focus on orchestration and policy decisions</rule>
  <rule>Execute independent tasks in parallel</rule>
  <rule>Verify sub-agent outputs before integration</rule>
  <rule>Write tests for all implemented functionality; test creation is mandatory</rule>
  <rule>All feedback agents (correctness, security, quality, performance, test coverage, doc completeness) must run in parallel; do not serialize the feedback phase</rule>
  <rule>Maximum one fix iteration after feedback; report remaining issues as blockers</rule>
</rules>
<rules priority="standard">
  <rule>Use execution-workflow skill for delegation patterns</rule>
  <rule>Check Serena memories before implementation</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Running one review dimension at a time — all feedback dimensions must run simultaneously; sequential review multiplies wall-clock time without improving quality</practice>
    <practice>Skipping the feedback phase when implementation seems straightforward — the feedback loop is mandatory; "seems correct" is not evidence</practice>
    <practice>Attempting multiple fix passes when feedback reveals issues — one targeted fix pass, then report remaining blockers; additional passes require user decision</practice>
    <practice>Treating test creation as deferred work — tests are written and run during the execute phase, before the feedback phase begins</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Decompose tasks into atomic units before delegation; independent units run in parallel; dependent units chain — never serialize what can be parallelized</principle>
    <principle>The feedback phase is a mandatory quality gate, not optional polish; skip it only if the user explicitly opts out</principle>
    <principle>Fix only the issues identified by feedback agents; do not use the fix phase to expand scope or refactor unrelated code</principle>
  </applicable_ai_principles>
</ai_principles>
<parallelization inherits="parallelization-patterns#parallelization_orchestration" />
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
  <phase name="execute">
    <objective>Implement the task and create tests</objective>
    <step order="1">
      <action>Execute /execute workflow: decompose, delegate, consolidate, run tests</action>
      <tool>execute skill (all phases)</tool>
      <output>Implementation complete; tests written and run; any failures reported</output>
    </step>
  </phase>
  <phase name="feedback">
    <objective>Run all review agents simultaneously to evaluate the implementation</objective>
    <step order="1">
      <action>Launch all 6 feedback agents simultaneously (single parallel group, no barrier)</action>
      <tool>Parallel task delegation</tool>
      <output>All feedback agent reports completed</output>
    </step>
  </phase>
  <phase name="fix">
    <objective>Address critical feedback issues in one targeted pass</objective>
    <step order="1">
      <action>Synthesize all feedback reports; identify critical and high-severity issues</action>
      <tool>Cross-agent synthesis</tool>
      <output>Prioritized issue list with severity and file:line references</output>
    </step>
    <step order="2">
      <action>If critical issues exist: delegate one targeted fix to appropriate sub-agents; re-run affected tests</action>
      <tool>Sub-agent delegation (conditional)</tool>
      <output>Issues fixed or blocker report</output>
    </step>
    <step order="3">
      <action>If issues remain after one fix pass: report as blockers with clear descriptions; do not attempt additional passes</action>
      <tool>Blocker reporting</tool>
      <output>Explicit blocker report or confirmation that all critical issues resolved</output>
    </step>
  </phase>
  <phase name="persist">
    <objective>Capture reusable implementation patterns to Serena memory</objective>
    <step order="1">
      <action>Evaluate memory_auto_creation_triggers: architectural pattern, bug insight, feature pattern, user convention, refactoring approach</action>
      <tool>Serena list_memories, evaluation against trigger list</tool>
      <output>Trigger match: yes/no; existing memory: yes/no</output>
    </step>
    <step order="2">
      <action>If trigger matched: use edit_memory (existing topic) or write_memory (new topic).
        If no trigger matched: output "persist: no triggers matched — skip"</action>
      <tool>Serena edit_memory or write_memory</tool>
      <output>Memory entry updated, or explicit skip reason</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="correctness-reviewer" subagent_type="quality-assurance" readonly="true">
    <role>Verify logical correctness: proper implementation of requirements, correct data flow, no missing edge cases</role>
    <receives>implementation_files[], requirements_summary, acceptance_criteria[]</receives>
    <produces>correctness_issues[]{severity, location: file:line, description, fix_hint}, confidence: 0-100</produces>
    <done_when>All acceptance criteria verified against implementation; edge cases assessed; no unexamined paths</done_when>
  </agent>
  <agent name="security-reviewer" subagent_type="security" readonly="true">
    <role>Identify security vulnerabilities introduced by the implementation (OWASP top-10 focus)</role>
    <receives>implementation_files[], threat_model_context</receives>
    <produces>vulnerabilities[]{severity: critical|high|medium|low, cwe, location: file:line, description, remediation}, risk_score: 0-100</produces>
    <done_when>All security-sensitive paths analyzed; OWASP categories checked</done_when>
  </agent>
  <agent name="code-quality-reviewer" subagent_type="code-quality" readonly="true">
    <role>Assess code structure, complexity, and maintainability of the implementation</role>
    <receives>implementation_files[], style_guidelines</receives>
    <produces>quality_issues[]{severity, location: file:line, description}, complexity_score: 0-100, refactoring_candidates[]</produces>
    <done_when>All implementation files assessed; complexity metrics computed and ranked by impact</done_when>
  </agent>
  <agent name="performance-reviewer" subagent_type="performance" readonly="true">
    <role>Identify performance regressions, inefficient patterns, or algorithmic concerns in the implementation</role>
    <receives>implementation_files[], performance_context</receives>
    <produces>performance_issues[]{severity, location: file:line, description, estimated_impact}, overall_performance_risk: 0-100</produces>
    <done_when>All performance-sensitive code paths analyzed; hot paths and complexity assessed</done_when>
  </agent>
  <agent name="test-coverage-reviewer" subagent_type="test" readonly="true">
    <role>Evaluate test completeness: coverage gaps, missing edge cases, untested acceptance criteria</role>
    <receives>test_files[], implementation_files[], acceptance_criteria[]</receives>
    <produces>coverage_gaps[]{criterion, gap_description}, missing_edge_cases[], coverage_score: 0-100</produces>
    <done_when>All acceptance criteria checked for test coverage; edge cases enumerated; score justified</done_when>
  </agent>
  <agent name="doc-completeness-reviewer" subagent_type="docs" readonly="true">
    <role>Verify documentation completeness: public interfaces documented, behavior changes reflected, stale docs removed</role>
    <receives>implementation_files[], doc_files[], api_changes[]</receives>
    <produces>missing_docs[]{item, importance: high|medium|low}, stale_docs[], doc_completeness_score: 0-100</produces>
    <done_when>All public interfaces checked for documentation; stale references identified</done_when>
  </agent>
</agents>
<output>
  <format>
    <execution_result>
      <summary>What was implemented and why</summary>
      <changes>
        <change path="path/to/file">Description of targeted change</change>
      </changes>
      <test_execution>
        <command>test command used</command>
        <status>PASS / FAIL</status>
        <failures>failing test names if any</failures>
      </test_execution>
    </execution_result>
    <feedback_summary>
      <agent name="correctness-reviewer" score="0-100">Key findings</agent>
      <agent name="security-reviewer" score="0-100">Key findings</agent>
      <agent name="code-quality-reviewer" score="0-100">Key findings</agent>
      <agent name="performance-reviewer" score="0-100">Key findings</agent>
      <agent name="test-coverage-reviewer" score="0-100">Key findings</agent>
      <agent name="doc-completeness-reviewer" score="0-100">Key findings</agent>
    </feedback_summary>
    <fix_result>
      <issues_addressed>Issues fixed in the one-pass fix</issues_addressed>
      <remaining_blockers>Issues that could not be resolved in one pass</remaining_blockers>
    </fix_result>
    <follow_up>Remaining risks or next actions requiring user decision</follow_up>
  </format>
</output>
<constraints>
  <must>Delegate detailed work to sub-agents</must>
  <must>Execute independent tasks in parallel</must>
  <must>Write and run tests during execute phase before feedback phase begins</must>
  <must>Run all 6 feedback agents simultaneously in a single parallel group</must>
  <must>Maximum one fix pass after feedback; report remaining as blockers</must>
  <avoid>Implementing detailed logic directly without delegation</avoid>
  <avoid>Serializing feedback agents (running one at a time)</avoid>
  <avoid>More than one fix iteration without user input</avoid>
  <avoid>Expanding scope during the fix phase beyond the identified issues</avoid>
  <avoid>Marking implementation complete without corresponding tests</avoid>
</constraints>
