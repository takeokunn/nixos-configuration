---
name: simplify
description: Review changed code for reuse, quality, and efficiency, then fix any issues found
---

<purpose>
Analyze recently changed code across three dimensions simultaneously (reuse opportunities, quality issues, and efficiency improvements), then apply fixes directly. Combines review and fix in one command.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="patterns">parallelization-patterns</skill>
  <skill use="workflow">investigation-patterns</skill>
  <skill use="tools">serena-usage</skill>
</refs>
<rules priority="critical">
  <rule>All three review agents (reuse, quality, efficiency) must run in parallel; never serialize</rule>
  <rule>Fix issues directly after review; do not propose-and-wait unless the change is irreversible or high-risk</rule>
  <rule>Provide file:line references for every finding and every fix</rule>
  <rule>Scope to the provided file/diff argument; do not expand to unrelated code</rule>
</rules>
<rules priority="standard">
  <rule>If no argument is provided, use git diff HEAD to determine the current change scope</rule>
  <rule>Prefer removing code over adding code when both achieve the same goal</rule>
  <rule>Do not introduce new abstractions unless three concrete cases already exist</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Sequential review dimensions — reuse, quality, and efficiency analyses are independent and must run in parallel</practice>
    <practice>Reporting issues without fixing — this command is designed to fix, not just to report</practice>
    <practice>Over-abstracting — three similar cases must already exist before introducing a new abstraction; speculative reuse creates complexity without benefit</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Scope discipline: analyze exactly the changed code, not the surrounding unchanged context — don't use simplification as an excuse for unrelated refactoring</principle>
    <principle>Prefer subtraction over addition: if something can be removed or inlined without loss of clarity, do it; AI can verify that removal is safe by tracing all usages</principle>
    <principle>Evidence-first fixing: every fix must reference the specific finding that motivated it; no unrequested "improvements"</principle>
  </applicable_ai_principles>
</ai_principles>
<parallelization inherits="parallelization-patterns#parallelization_orchestration" />
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Determine scope: if argument provided, use it; otherwise run git diff HEAD to find changed files</action>
      <tool>Bash (git diff HEAD or argument parsing)</tool>
      <output>Scope: list of changed files with line ranges</output>
    </step>
    <step order="2">
      <action>Activate Serena; load task-type memories for "refactoring"</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>Relevant patterns loaded</output>
    </step>
  </phase>
  <phase name="review">
    <step order="1">
      <action>Launch all 3 review agents simultaneously</action>
      <tool>Parallel task delegation (single message)</tool>
      <output>All 3 review reports completed</output>
    </step>
  </phase>
  <phase name="fix">
    <step order="1">
      <action>Synthesize findings; prioritize by impact (blocking > high > medium > low)</action>
      <tool>Cross-agent synthesis</tool>
      <output>Prioritized fix list with file:line references</output>
    </step>
    <step order="2">
      <action>Apply fixes; confirm test suite still passes after changes</action>
      <tool>Edit/Write tools; Bash (test runner)</tool>
      <output>All fixes applied; test results</output>
    </step>
    <step order="3">
      <action>Evaluate memory_auto_creation_triggers; update Serena if new pattern found</action>
      <tool>Serena list_memories, edit_memory or write_memory</tool>
      <output>Memory updated or "persist: no triggers matched — skip"</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="reuse-reviewer" subagent_type="code-quality" readonly="true">
    <role>Identify reuse opportunities: duplicate logic, utility extractions, pattern consolidations</role>
    <receives>changed_files[], existing_utilities[], scope_description</receives>
    <produces>reuse_opportunities[]{severity, location: file:line, description, existing_alternative?}</produces>
    <done_when>All changed code analyzed for duplication; existing utilities checked; three-case rule applied</done_when>
  </agent>
  <agent name="quality-reviewer" subagent_type="quality-assurance" readonly="true">
    <role>Identify code quality issues: naming, clarity, dead code, unnecessary complexity</role>
    <receives>changed_files[], style_guidelines</receives>
    <produces>quality_issues[]{severity, location: file:line, description, fix_hint}</produces>
    <done_when>All changed files assessed for quality; dead code and naming issues identified</done_when>
  </agent>
  <agent name="efficiency-reviewer" subagent_type="performance" readonly="true">
    <role>Identify efficiency improvements: algorithmic complexity, unnecessary allocations, redundant operations</role>
    <receives>changed_files[], performance_context</receives>
    <produces>efficiency_issues[]{severity, location: file:line, description, estimated_impact}</produces>
    <done_when>All changed code analyzed for efficiency; hot paths and complexity assessed</done_when>
  </agent>
</agents>
<output>
  <format>
    <review_findings>
      <dimension name="Reuse">Issues with file:line and description</dimension>
      <dimension name="Quality">Issues with file:line and description</dimension>
      <dimension name="Efficiency">Issues with file:line and description</dimension>
    </review_findings>
    <fixes_applied>
      <fix location="file:line" finding="which finding motivated this">Description of change</fix>
    </fixes_applied>
    <test_result>
      <command>test command used</command>
      <status>PASS / FAIL</status>
    </test_result>
    <remaining>Issues not fixed and reason (e.g., requires user decision, too high risk)</remaining>
  </format>
</output>
<constraints>
  <must>Run all 3 review agents simultaneously</must>
  <must>Provide file:line references for all findings and fixes</must>
  <must>Scope strictly to the provided argument or git diff output</must>
  <must>Run tests after applying fixes</must>
  <avoid>Sequential agent execution</avoid>
  <avoid>Fixing issues not identified in the review phase</avoid>
  <avoid>Introducing new abstractions without three existing concrete cases</avoid>
  <avoid>Expanding scope to unrelated code</avoid>
</constraints>
