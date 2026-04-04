---
argument-hint: [file-or-diff-scope]
description: Review changed code for reuse, quality, and efficiency, then fix any issues found
---

<purpose>
Code review and cleanup command that identifies changes via git diff, launches three specialized review agents in parallel (code reuse, code quality, efficiency), aggregates findings, and fixes issues directly.
</purpose>

<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
  <skill use="workflow">execution-workflow</skill>
</refs>

<rules priority="critical">
  <rule>Launch all three review agents simultaneously in one message (timeout avoidance)</rule>
  <rule>Provide the full diff to every agent</rule>
  <rule>Fix issues directly after aggregation; do not just report</rule>
  <rule>Skip false positives with a brief note explaining why</rule>
</rules>

<rules priority="standard">
  <rule>Use git diff for unstaged changes, git diff HEAD for staged changes</rule>
  <rule>If no git changes, review recently modified files mentioned by user</rule>
  <rule>Check Serena memories for existing utilities before flagging reuse issues</rule>
  <rule>Prefer existing abstractions over new ones</rule>
</rules>

<parallelization inherits="parallelization-patterns#parallelization_orchestration" />

<workflow>
  <phase name="prepare">
    <objective>Initialize Serena and gather existing patterns</objective>
    <step>1. Activate Serena project with activate_project</step>
    <step>2. Check list_memories for existing utility and pattern references</step>
    <step>3. Load applicable memories with read_memory</step>
  </phase>
  <phase name="identify_changes">
    <objective>Collect the diff that all agents will review</objective>
    <step>1. Run git diff to get unstaged changes</step>
    <step>2. Run git diff HEAD to get staged changes</step>
    <step>3. If both are empty, ask user for files or scope to review</step>
    <step>4. Combine into a single diff payload for agents</step>
  </phase>
  <reflection_checkpoint id="scope_quality" after="identify_changes" inherits="workflow-patterns#reflection_checkpoint">
    <question>Is the diff non-empty and well-scoped?</question>
    <question>Are there related files outside the diff that agents should know about?</question>
    <threshold>If confidence less than 70, ask user to clarify scope</threshold>
  </reflection_checkpoint>
  <phase name="review">
    <objective>Launch three review agents in parallel with the full diff</objective>
    <step>1. Launch Agent 1 (Code Reuse), Agent 2 (Code Quality), Agent 3 (Efficiency) simultaneously</step>
    <step>2. Each agent receives the full diff and relevant codebase context</step>
    <step>3. Collect all agent results</step>
  </phase>
  <reflection_checkpoint id="review_quality" after="review">
    <question>Did all three agents complete successfully?</question>
    <question>Are findings specific with file:line references?</question>
    <question>Are there contradictions between agents?</question>
    <threshold>If confidence less than 70, re-run failed agents or gather additional context</threshold>
    <serena_validation>
      <tool>think_about_collected_information</tool>
      <trigger>After review agents complete</trigger>
    </serena_validation>
  </reflection_checkpoint>
  <phase name="aggregate">
    <objective>Deduplicate and prioritize findings across all agents</objective>
    <step>1. Merge findings from all three agents</step>
    <step>2. Deduplicate overlapping issues</step>
    <step>3. Prioritize by severity: critical, warning, suggestion</step>
    <step>4. Identify false positives and mark for skipping</step>
  </phase>
  <phase name="fix">
    <objective>Apply fixes for all confirmed issues</objective>
    <step>1. Fix each confirmed issue directly in the codebase</step>
    <step>2. For false positives, note the reason and skip</step>
    <step>3. Verify fixes do not introduce new issues</step>
  </phase>
  <reflection_checkpoint id="completion_validation" after="fix">
    <serena_validation>
      <tool>think_about_whether_you_are_done</tool>
      <trigger>Before reporting task completion to user</trigger>
    </serena_validation>
  </reflection_checkpoint>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
</workflow>

<agents>
  <agent name="reuse" subagent_type="code-quality" readonly="true">
    <objective>Find opportunities to reuse existing code instead of writing new code</objective>
    <checklist>
      <item>Search for existing utilities and helpers that could replace newly written code</item>
      <item>Flag new functions that duplicate existing functionality and suggest the existing function</item>
      <item>Flag inline logic that could use existing utilities: hand-rolled string manipulation, manual path handling, custom env checks, ad-hoc type guards</item>
    </checklist>
  </agent>
  <agent name="quality" subagent_type="quality-assurance" readonly="true">
    <objective>Identify code quality issues in the changed code</objective>
    <checklist>
      <item>Redundant state: state duplicating existing, cached values derivable from other state, observers/effects replaceable by direct calls</item>
      <item>Parameter sprawl: adding parameters instead of generalizing the interface</item>
      <item>Copy-paste with slight variation: near-duplicate blocks that need a shared abstraction</item>
      <item>Leaky abstractions: exposing internals, breaking abstraction boundaries</item>
      <item>Stringly-typed code: raw strings where constants, enums, or branded types already exist</item>
      <item>Unnecessary nesting: wrapper elements or containers adding no value</item>
      <item>Unnecessary comments: comments explaining WHAT (delete them), keep only non-obvious WHY comments</item>
    </checklist>
  </agent>
  <agent name="efficiency" subagent_type="performance" readonly="true">
    <objective>Find performance and efficiency issues in the changed code</objective>
    <checklist>
      <item>Unnecessary work: redundant computations, repeated file reads, duplicate API calls, N+1 queries</item>
      <item>Missed concurrency: independent operations run sequentially that could be parallelized</item>
      <item>Hot-path bloat: blocking work added to startup or per-request hot paths</item>
      <item>Recurring no-op updates: unconditional state updates in loops without change-detection guards</item>
      <item>Unnecessary existence checks: TOCTOU anti-pattern; operate directly and handle error instead</item>
      <item>Memory issues: unbounded data structures, missing cleanup, event listener leaks</item>
      <item>Overly broad operations: reading entire files or datasets when only a portion is needed</item>
    </checklist>
  </agent>
</agents>

<decision_criteria inherits="core-patterns#decision_criteria">
  <criterion name="issue_severity">
    <factor name="correctness_impact" weight="0.4">
      <score range="90-100">Issue causes incorrect behavior or data loss</score>
      <score range="70-89">Issue causes degraded behavior</score>
      <score range="50-69">Issue is a code smell without runtime impact</score>
      <score range="0-49">Stylistic preference only</score>
    </factor>
    <factor name="reuse_opportunity" weight="0.3">
      <score range="90-100">Exact duplicate of existing utility</score>
      <score range="70-89">Near-duplicate with minor differences</score>
      <score range="50-69">Partial overlap with existing code</score>
      <score range="0-49">No existing code to reuse</score>
    </factor>
    <factor name="efficiency_gain" weight="0.3">
      <score range="90-100">Eliminates redundant I/O or N+1</score>
      <score range="70-89">Reduces unnecessary computation</score>
      <score range="50-69">Minor optimization opportunity</score>
      <score range="0-49">Negligible performance difference</score>
    </factor>
  </criterion>
</decision_criteria>

<output>
  <format>
    <simplify_results>
      <summary>
- Files reviewed: {count}
- Issues found: {count}
- Issues fixed: {count}
- False positives skipped: {count}</summary>
      <reuse_findings>
- [Severity] {file}:{line} - {description}
  Existing: {existing_function_or_utility}
  Action: {replaced|skipped with reason}</reuse_findings>
      <quality_findings>
- [Severity] {file}:{line} - {category}: {description}
  Action: {fixed|skipped with reason}</quality_findings>
      <efficiency_findings>
- [Severity] {file}:{line} - {category}: {description}
  Action: {fixed|skipped with reason}</efficiency_findings>
    </simplify_results>
  </format>
</output>

<enforcement>
  <mandatory_behaviors>
    <behavior id="SIM-B001" priority="critical">
      <trigger>Before launching agents</trigger>
      <action>Collect the full diff and provide it to all three agents</action>
      <verification>All agents received identical diff context</verification>
    </behavior>
    <behavior id="SIM-B002" priority="critical">
      <trigger>When launching review agents</trigger>
      <action>Launch all three agents simultaneously in a single message</action>
      <verification>Three parallel Task tool calls in one message</verification>
    </behavior>
    <behavior id="SIM-B003" priority="critical">
      <trigger>When reporting issues</trigger>
      <action>Include specific file:line references for every finding</action>
      <verification>References present in all findings</verification>
    </behavior>
    <behavior id="SIM-B004" priority="critical">
      <trigger>After aggregating findings</trigger>
      <action>Fix confirmed issues directly; do not just list them</action>
      <verification>Fixes applied or false positive noted for each finding</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="SIM-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Running agents sequentially</action>
      <response>Launch all three agents in parallel</response>
    </behavior>
    <behavior id="SIM-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Reporting issues without fixing them</action>
      <response>Fix each confirmed issue or explicitly note why it is a false positive</response>
    </behavior>
    <behavior id="SIM-P003" priority="critical">
      <trigger>Always</trigger>
      <action>Suggesting new abstractions when existing ones suffice</action>
      <response>Use existing utilities, helpers, and patterns from the codebase</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <low>Minor style issue or marginal reuse opportunity</low>
    <medium>Duplicated logic that should use existing utility</medium>
    <high>N+1 query, unbounded data structure, or significant code duplication</high>
    <critical>Data loss risk, security vulnerability, or correctness bug in changed code</critical>
  </examples>
</error_escalation>

<related_commands>
  <command name="feedback">General review of Claude Code's work across multiple dimensions</command>
  <command name="execute">Primary target for simplify after implementation</command>
</related_commands>

<related_skills>
  <skill name="execution-workflow">Understanding code review and fix methodology</skill>
  <skill name="investigation-patterns">Evidence-based code analysis</skill>
  <skill name="testing-patterns">Verifying fixes do not break tests</skill>
</related_skills>

<constraints>
  <must>Launch all three agents simultaneously (no sequential execution)</must>
  <must>Fix confirmed issues directly in the codebase</must>
  <must>Search for existing utilities before suggesting new abstractions</must>
  <must>Provide file:line references for every finding</must>
  <avoid>Reporting issues without fixing them</avoid>
  <avoid>Sequential agent execution (causes timeout)</avoid>
  <avoid>Suggesting new code when existing utilities already handle the case</avoid>
  <avoid>Flagging stylistic preferences as issues</avoid>
</constraints>
