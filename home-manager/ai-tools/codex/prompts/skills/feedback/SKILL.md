---
name: feedback
description: Review command for recent work
---

<purpose>
Review recently implemented work through multi-dimensional parallel analysis. Evaluates correctness, security, quality, performance, test coverage, and documentation completeness. All agents run simultaneously; never serialized.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="patterns">parallelization-patterns</skill>
  <skill use="workflow">investigation-patterns</skill>
  <skill use="tools">serena-usage</skill>
</refs>
<rules priority="critical">
  <rule>All review agents must launch simultaneously in a single parallel group; never serialize</rule>
  <rule>Provide file:line references for all findings</rule>
  <rule>Auto-select review mode based on context; do not ask the user unless scope is ambiguous</rule>
  <rule>Review only; never modify files</rule>
</rules>
<rules priority="standard">
  <rule>Review most recent changes unless a specific scope is provided</rule>
  <rule>Score each dimension 0-100 with evidence</rule>
  <rule>Distinguish critical issues from informational observations</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Running one review dimension at a time — all dimensions must run in parallel; sequential review multiplies latency</practice>
    <practice>Reviewing only the files explicitly mentioned — AI should trace the full change impact including files transitively affected</practice>
    <practice>Separating "what the code does" from "whether it does what was intended" — both must be evaluated against the stated requirements</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Parallel dimension coverage: correctness, security, quality, performance, test coverage, and documentation assessed simultaneously</principle>
    <principle>All findings must be anchored to specific file:line references — no abstract or general impressions</principle>
    <principle>Distinguish between regressions introduced by recent changes and pre-existing issues; flag both but prioritize regressions</principle>
  </applicable_ai_principles>
</ai_principles>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Determine scope: if a specific scope is provided (file, PR, commit, diff), use it; otherwise, use git diff HEAD to identify recent changes</action>
      <tool>Bash (git diff), Serena read/search</tool>
      <output>Review scope: list of changed files with file:line ranges</output>
    </step>
    <step order="2">
      <action>Activate Serena project; load task-type memories for "review" (prioritize {project}-conventions, code-quality-*, architecture-*)</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>Relevant patterns loaded</output>
    </step>
  </phase>
  <phase name="review">
    <step order="1">
      <action>Launch all 6 review agents simultaneously in a single parallel group</action>
      <tool>Parallel task delegation (all agents in one message)</tool>
      <output>All 6 review reports completed</output>
    </step>
  </phase>
  <phase name="synthesize">
    <step order="1">
      <action>Synthesize all agent reports; separate critical issues from warnings and informational findings</action>
      <tool>Cross-agent synthesis</tool>
      <output>Consolidated review report with prioritized issues</output>
    </step>
    <step order="2">
      <action>Evaluate memory_auto_creation_triggers: did this review reveal a reusable pattern or convention?
        Call list_memories; use edit_memory or write_memory if trigger matched.</action>
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
  <agent name="correctness-reviewer" subagent_type="quality-assurance" readonly="true">
    <role>Verify logical correctness against requirements and detect behavioral regressions</role>
    <receives>changed_files[], requirements_summary, acceptance_criteria[]</receives>
    <produces>correctness_issues[]{severity: critical|high|medium|low, location: file:line, description}, confidence: 0-100</produces>
    <done_when>All changed code paths verified against requirements; regressions explicitly flagged</done_when>
  </agent>
  <agent name="security-reviewer" subagent_type="security" readonly="true">
    <role>Identify security vulnerabilities introduced by the changes (OWASP top-10 focus)</role>
    <receives>changed_files[], threat_model_context</receives>
    <produces>vulnerabilities[]{severity: critical|high|medium|low, cwe, location: file:line, description, remediation}, risk_score: 0-100</produces>
    <done_when>All security-sensitive paths in changed files analyzed</done_when>
  </agent>
  <agent name="code-quality-reviewer" subagent_type="code-quality" readonly="true">
    <role>Assess code structure, complexity, and maintainability of changed files</role>
    <receives>changed_files[], style_guidelines</receives>
    <produces>quality_issues[]{severity, location: file:line, description}, complexity_score: 0-100</produces>
    <done_when>All changed files assessed; complexity metrics computed</done_when>
  </agent>
  <agent name="performance-reviewer" subagent_type="performance" readonly="true">
    <role>Identify performance regressions or inefficient patterns introduced by the changes</role>
    <receives>changed_files[], performance_context</receives>
    <produces>performance_issues[]{severity, location: file:line, description, estimated_impact}, risk_score: 0-100</produces>
    <done_when>All performance-sensitive paths in changed files analyzed</done_when>
  </agent>
  <agent name="test-coverage-reviewer" subagent_type="test" readonly="true">
    <role>Evaluate test completeness for the changed code: coverage gaps, missing edge cases</role>
    <receives>changed_files[], test_files[], acceptance_criteria[]</receives>
    <produces>coverage_gaps[]{criterion, gap_description}, missing_edge_cases[], coverage_score: 0-100</produces>
    <done_when>All acceptance criteria checked for test coverage; edge cases enumerated</done_when>
  </agent>
  <agent name="doc-completeness-reviewer" subagent_type="docs" readonly="true">
    <role>Verify documentation completeness for changed interfaces and behavior</role>
    <receives>changed_files[], doc_files[], api_changes[]</receives>
    <produces>missing_docs[]{item, importance: high|medium|low}, stale_docs[], doc_score: 0-100</produces>
    <done_when>All public interface changes checked for documentation; stale references identified</done_when>
  </agent>
</agents>
<output>
  <format>
    <review_summary>
      <scope>Files and commits reviewed</scope>
      <scores>
        <dimension name="Correctness" score="0-100">Key finding</dimension>
        <dimension name="Security" score="0-100">Key finding</dimension>
        <dimension name="Code Quality" score="0-100">Key finding</dimension>
        <dimension name="Performance" score="0-100">Key finding</dimension>
        <dimension name="Test Coverage" score="0-100">Key finding</dimension>
        <dimension name="Documentation" score="0-100">Key finding</dimension>
      </scores>
      <critical_issues>Issues requiring immediate attention (file:line references required)</critical_issues>
      <warnings>Issues that should be addressed but are not blocking</warnings>
      <informational>Observations that may be useful context</informational>
      <recommendations>Prioritized next actions</recommendations>
    </review_summary>
  </format>
</output>
<constraints>
  <must>Launch all 6 review agents simultaneously</must>
  <must>Provide file:line references for all findings</must>
  <must>Keep all operations read-only</must>
  <avoid>Sequential agent execution</avoid>
  <avoid>Modifying any files during review</avoid>
  <avoid>Reporting findings without file:line evidence</avoid>
</constraints>
