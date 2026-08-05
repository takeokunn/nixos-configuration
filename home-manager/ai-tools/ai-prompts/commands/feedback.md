---
argument-hint: [previous-command]
description: Review command for Claude Code's recent work
---

<purpose>
Multi-faceted review of Claude Code's work within the same session, automatically selecting appropriate review mode and executing efficiently in parallel.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">execution-workflow</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<rules priority="critical">
  <rule>Launch all Task tools simultaneously in one message (timeout avoidance). This scopes to a
    single dispatch wave — the selected mode's specialist agents, or the refute phase's
    per-critical-finding validator dispatches. The refute phase itself is a separate, later wave that
    runs after specialist findings exist; that sequencing is not the prohibited pattern.</rule>
  <rule>Auto-select mode based on previous command</rule>
  <rule>Review only changed code in execute mode, not existing issues</rule>
  <rule>Provide concrete fix proposals, not abstract theories</rule>
</rules>
<rules priority="standard">
  <rule>Use execution-workflow skill for code review methodology</rule>
  <rule>Check Serena memories for existing patterns</rule>
  <rule>Target session operations, not git diff</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Reviewing work sequentially across quality dimensions before synthesizing — AI can launch all review agents (quality, security, design, docs, performance, test) simultaneously in a single pass</practice>
    <practice>Providing general impressions without anchoring to code — every feedback item must cite a specific file:line reference, replacing vague commentary with precise evidence</practice>
    <practice>Waiting for user to triage and prioritize issues — AI should assign severity and priority levels automatically based on impact assessment</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Parallelize all review dimensions simultaneously; no dimension should block another since they operate on the same diff</principle>
    <principle>Anchor each finding to a specific location (file:line) and include a concrete fix proposal — never report issues without accompanying remediation</principle>
    <principle>Calibrate severity levels by actual runtime impact, not style preference; distinguish critical (data loss/security) from warning (degraded behavior) from info (style)</principle>
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
      <action>Classify task type as "review". Apply memory_reading_by_task_type filter
        (serena-usage skill): prioritize {project}-conventions → code-quality-* → architecture-*.
        Filter the memory index from step 2 against these categories; record matched names.</action>
      <tool>serena-usage#memory_reading_by_task_type (reference only)</tool>
      <output>Filtered priority memory list for review tasks</output>
    </step>
    <step order="4">
      <action>Load only memories matching the prioritized categories with read_memory;
        skip categories absent from the index</action>
      <tool>Serena read_memory</tool>
      <output>Prioritized patterns loaded</output>
    </step>
  </phase>
  <phase name="analyze">
    <step order="1">
      <action>What was the previous command? (/define, /execute, /bug, /ask, other)</action>
      <output>The command named; if none of these, mode is general</output>
    </step>
    <step order="2">
      <action>What files/work need to be reviewed?</action>
      <tool>Serena find_symbol, get_symbols_overview on the files touched this session</tool>
      <output>Explicit file list — paths, not "the recent changes"</output>
    </step>
    <step order="3">
      <action>Which agents should run in parallel?</action>
      <output>Named agents from the selected mode, all dispatched in one message</output>
    </step>
    <step order="4">
      <action>What metrics are relevant for this mode?</action>
      <output>The aspects the selected mode evaluates</output>
    </step>
  </phase>
  <phase name="select">
    <step order="1">
      <action>Determine mode based on previous command</action>
      <output>One mode selected from the modes section</output>
    </step>
    <step order="2">
      <action>After /define: Execution plan feedback</action>
      <output>Mode define; agents plan, estimation</output>
    </step>
    <step order="3">
      <action>After /execute: Work content feedback</action>
      <output>Mode execute; agents quality, security, design, docs, performance, test</output>
    </step>
    <step order="4">
      <action>After /bug: Investigation quality feedback</action>
      <output>Mode bug; agents quality-assurance, general-purpose, explore</output>
    </step>
    <step order="5">
      <action>After /ask: Answer accuracy feedback</action>
      <output>Mode ask; agents explore, quality-assurance, code-quality</output>
    </step>
    <step order="6">
      <action>Other: Recent work feedback</action>
      <output>Mode general; agents review, complexity, memory</output>
    </step>
  </phase>
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
  <phase name="execute">
    <step order="1">
      <action>Launch all agents in parallel</action>
      <tool>Task — every agent dispatched in a single message, never one per message</tool>
      <output>All agents of the selected mode dispatched together</output>
    </step>
    <step order="2">
      <action>Collect agent results</action>
      <output>One report per agent, or a named agent that returned nothing</output>
    </step>
  </phase>
  <reflection_checkpoint id="review_quality">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name every agent dispatched and what it returned. Name any that timed out or died without
      returning — a missing report is not a clean review.</check>
    <check>For each finding, name the file:line it cites and the concrete fix proposed. A finding with
      neither is a retry condition (parallelization-patterns#retry_policy), not a result.</check>
    <check>Name the severity assigned to each finding and the runtime impact justifying it — data loss
      or security for critical, degraded behavior for warning, style for info.</check>
    <on_unmet>Re-run the named agent once with a narrower prompt naming the specific files. If it fails
      again, review that dimension here and report that the delegation failed.</on_unmet>
  </reflection_checkpoint>
  <phase name="refute" inherits="core-patterns#adversarial_verification_escalation">
    <objective>Independently refute critical-severity findings before they reach the user</objective>
    <step order="1">
      <action>From the findings collected in execute, select only those classified critical — the
        severity taxonomy (critical/warning/info) already used by this command's issue_prioritization
        factor, distinct from the good_practice category reserved for positive observations. Warning
        and info findings are not sent for refutation; refutation is reserved for critical findings
        because an independent adversarial pass costs materially more than a single review pass
        (reports in the wild cite roughly 3-10x, treat as assumed not verified), so it must stay
        proportionate to what is actually consequential</action>
      <output>The critical-finding subset, or an explicit "no critical findings" result</output>
    </step>
    <step order="2">
      <action>For each critical finding, dispatch exactly one validator agent instance in a fresh
        context containing only that finding's text and its cited file:line or command output — never
        the specialist agent's full report or reasoning, so the refutation is genuinely independent of
        the agent that raised it. When multiple critical findings exist, dispatch all their validator
        instances together in one message, not sequentially. Frame the task explicitly as "attempt to
        refute this finding," not "review this finding" — a reviewer tends to confirm, a refuter is
        asked to find the flaw. Because validator's own default framing expects multiple existing
        reports to compare, state explicitly in the dispatch prompt that this is a single-claim
        independent-verification task: instruct validator to disregard its usual single-source-means-
        unvalidated framing for this dispatch and instead independently re-derive whether the finding
        holds, using Read/Grep/Bash as needed — the deliverable is a refutation attempt, not a
        single-source label</action>
      <tool>Task (validator)</tool>
      <output>One refutation attempt per critical finding, each independent of the others, or an
        explicit record that a dispatch failed or returned nothing checkable</output>
    </step>
    <step order="3">
      <action>If a validator dispatch fails, times out, or returns nothing checkable, do not retry
        silently and do not drop the finding: report it in refutation_results as "refutation attempted,
        outcome unavailable" so the finding still carries a visible trace rather than silently reaching
        the user unrefuted and unmarked</action>
      <output>Every critical finding accounted for in refutation_results, including failed attempts</output>
    </step>
    <step order="4">
      <action>If the refutation succeeds, downgrade the finding's evidence tier (e.g. verified to
        inferred) and carry both the original finding and the refuting evidence forward to the user —
        never drop the disagreement silently. If the refutation does not overturn the finding, keep it
        as reported and annotate that an independent refutation was attempted and did not succeed</action>
      <output>Each critical finding annotated with its refutation outcome</output>
    </step>
  </phase>
  <reflection_checkpoint id="refutation_quality" after="refute">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name every critical finding and whether it was sent for refutation, or state there were none.</check>
    <check>For each refutation dispatched, name the outcome — survived or downgraded — and the evidence
      the refuting agent cited.</check>
    <check>Confirm no warning- or info-severity finding was sent for refutation.</check>
    <on_unmet>Dispatch the missing refutation, or correct the scope before reporting.</on_unmet>
  </reflection_checkpoint>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Detect and classify failures during command execution</action>
      <output>Failure classification and impact summary</output>
    </step>
    <step order="2">
      <action>Apply recovery path or escalate with concrete blocker details</action>
      <output>Recovered flow or explicit blocker report</output>
    </step>
  </phase>
  <phase name="persist">
    <objective>Capture reusable review patterns to Serena memory</objective>
    <step order="1">
      <action>Evaluate memory_auto_creation_triggers: did this review reveal a recurring quality issue,
        a reusable review pattern, or a project convention worth recording?
        Call list_memories to check if a memory for this topic already exists.</action>
      <tool>Serena list_memories, evaluation against trigger list</tool>
      <output>Trigger match: yes/no; existing memory: yes/no</output>
    </step>
    <step order="2">
      <action>If trigger matched: use edit_memory (existing topic) or write_memory (new topic).
        For write_memory: prepend memory_content_format frontmatter (serena-usage skill)
        with domain, status=active, created=YYYY-MM, last-verified=YYYY-MM.
        For edit_memory on a memory lacking frontmatter: add it, updating last-verified.
        If no trigger matched: output "persist: no triggers matched — skip"</action>
      <tool>Serena edit_memory or write_memory</tool>
      <output>Memory entry updated with frontmatter (name listed), or explicit skip reason</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Answer each check with a concrete artifact.</gate>
  <check>Name any required section that is absent or out of order, or state that all are present.</check>
  <check>Confirm every dispatched agent is readonly — this command reviews work, it does not change it.</check>
  <on_unmet>Stop and resolve the structural gap before dispatching.</on_unmet>
</reflection_checkpoint>
<modes>
  <mode name="define">
    <target>Execution plan from conversation history</target>
    <aspects>Step granularity, dependencies, risk identification, completeness, feasibility</aspects>
    <agents>
      <agent name="plan" subagent_type="general-purpose" readonly="true">Execution plan review</agent>
      <agent name="estimation" subagent_type="general-purpose" readonly="true">Estimation validity review</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="execute">
    <target>Files modified via Edit/Write tools</target>
    <agents>
      <agent name="quality" subagent_type="quality-assurance" readonly="true">Naming, DRY, readability</agent>
      <agent name="security" subagent_type="security" readonly="true">OWASP Top 10, input validation, auth</agent>
      <agent name="design" subagent_type="design" readonly="true">Architecture consistency, patterns</agent>
      <agent name="docs" subagent_type="docs" readonly="true">Accuracy, structure, completeness</agent>
      <agent name="performance" subagent_type="performance" readonly="true">Performance review</agent>
      <agent name="test" subagent_type="test" readonly="true">Test coverage review</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="general">
    <target>Recent Claude Code work</target>
    <agents>
      <agent name="review" subagent_type="quality-assurance" readonly="true">Comprehensive work review</agent>
      <agent name="complexity" subagent_type="code-quality" readonly="true">Code complexity review</agent>
      <agent name="memory" subagent_type="general-purpose" readonly="true">Consistency check with existing patterns</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="bug">
    <target>Investigation results from conversation history</target>
    <aspects>Evidence collection, hypothesis validity, root cause accuracy, log utilization</aspects>
    <metrics>Evidence tier of the root-cause claim; which log lines were actually cited; which alternative hypotheses were ruled out and by what</metrics>
    <agents>
      <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">Investigation methodology evaluation</agent>
      <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Log analysis and dependency investigation evaluation</agent>
      <agent name="explore" subagent_type="explore" readonly="true">Code path coverage evaluation</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="ask">
    <target>Answer and evidence from conversation history</target>
    <aspects>Evidence citation quality, conclusion validity, reference accuracy, and whether any claim is tagged verified without a command or file:line behind it</aspects>
    <metrics>Evidence tier per claim; whether each cited file:line was read rather than inferred from naming</metrics>
    <note>Subset of ask.md agents focused on answer evaluation; design/performance agents omitted as they evaluate questions, not answers</note>
    <agents>
      <agent name="explore" subagent_type="explore" readonly="true">Evidence gathering evaluation</agent>
      <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">Answer accuracy assessment</agent>
      <agent name="code-quality" subagent_type="code-quality" readonly="true">Reference precision and conclusion validity</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
</modes>
<agents>
  <agent name="plan" subagent_type="general-purpose" readonly="true">Execution plan review</agent>
  <agent name="estimation" subagent_type="general-purpose" readonly="true">Estimation validity review</agent>
  <agent name="quality" subagent_type="quality-assurance" readonly="true">Naming, DRY, readability</agent>
  <agent name="security" subagent_type="security" readonly="true">OWASP Top 10, input validation, auth</agent>
  <agent name="design" subagent_type="design" readonly="true">Architecture consistency, patterns</agent>
  <agent name="docs" subagent_type="docs" readonly="true">Accuracy, structure, completeness</agent>
  <agent name="performance" subagent_type="performance" readonly="true">Performance review</agent>
  <agent name="test" subagent_type="test" readonly="true">Test coverage review</agent>
  <agent name="review" subagent_type="quality-assurance" readonly="true">Comprehensive work review</agent>
  <agent name="complexity" subagent_type="code-quality" readonly="true">Code complexity review</agent>
  <agent name="memory" subagent_type="general-purpose" readonly="true">Consistency check with existing patterns</agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Log analysis and dependency investigation evaluation</agent>
  <agent name="explore" subagent_type="explore" readonly="true">Evidence and code path coverage evaluation</agent>
  <agent name="code-quality" subagent_type="code-quality" readonly="true">Reference precision and conclusion validity</agent>
  <agent name="validator" subagent_type="validator" readonly="true">Independent refutation of critical-severity findings (refute phase)</agent>
</agents>
<execution_graph>
  <sequential_phase id="mode_selection" depends_on="none">
    <action>Select one review mode from the modes section based on the previous command</action>
  </sequential_phase>
  <parallel_group id="selected_review" depends_on="mode_selection">
    <agent>Agents listed in the selected mode</agent>
  </parallel_group>
  <sequential_phase id="refutation" depends_on="selected_review">
    <agent>validator</agent>
    <reason>Independently refute critical-severity findings before synthesis (one dispatch per critical finding, run in parallel with each other)</reason>
  </sequential_phase>
  <sequential_phase id="synthesis" depends_on="selected_review,refutation">
    <action>Compile the review report with metrics, findings, and recommended actions</action>
  </sequential_phase>
</execution_graph>
<decision_criteria inherits="core-patterns#decision_criteria">
  <factor name="review_depth" precedence="1">
    <unmet>A file in the review scope was never opened by any agent, or an agent reported on a file it
      did not read. Read it before reporting — a review of files nobody opened is not a review.</unmet>
  </factor>
  <factor name="feedback_actionability" precedence="2">
    <unmet>A finding names no file:line, or proposes no concrete change. Anchor it or drop it; an
      unanchored observation costs the reader more than it returns (FB-B001, FB-B002).</unmet>
  </factor>
  <factor name="issue_prioritization" precedence="3">
    <unmet>A finding carries no severity, or its severity is justified by style preference rather than
      runtime impact. Reclassify by impact: data loss and security are critical, degraded behavior is
      warning, style is info.</unmet>
  </factor>
  <factor name="refutation_scope" precedence="4">
    <unmet>A critical-severity finding reached the final report without an independent refutation
      attempt, or a warning/info finding was sent for refutation anyway. This factor governs the refute
      phase specifically, evaluated after review_quality has already gated severity classification —
      it is not bypassed by an earlier factor firing during a different phase. Dispatch the missing
      refutation, or narrow the scope back to critical-only, before reporting.</unmet>
  </factor>
  <resolution>Apply in precedence order. The first factor whose `unmet` condition holds decides what
    happens next; later factors are not consulted.</resolution>
</decision_criteria>
<output>
  <format>
    <feedback_results mode="{Mode}">
      <agents_run>Each agent dispatched and what it returned; name any that returned nothing</agents_run>
      <verification>Any command run during review and its exit status, or "none run"</verification>
      <critical>Immediate Fix Required
- [Category] Issue: Location
- Problem: Description
- Fix: Proposal</critical>
      <refutation_results>Per critical finding: attempted (yes/no), outcome (survived | downgraded |
        unavailable), and the refuting evidence — or "no critical findings this run" when the set was
        empty</refutation_results>
      <warning>Fix Recommended
- [Category] Issue: Location
- Problem: Description
- Recommendation: Proposal</warning>
      <good_practice>[Category] Commendable aspects</good_practice>
      <fact_check_results>
        <verified_claims>Claims confirmed against a named external source (Context7, WebSearch), each
          carrying the source and the passage that confirms it</verified_claims>
        <flagged_claims>Claims the source does not confirm, or confirms only by inference
- Claim: {claim}
- Source referenced: {source}
- Evidence tier: inferred | assumed, and the inferential step taken
- Recommendation: {correction}</flagged_claims>
        <unverifiable_claims>Claims that could not be checked due to unavailable sources</unverifiable_claims>
      </fact_check_results>
      <gaps>Anything in scope that was not reviewed, and why</gaps>
      <recommended_actions>
- [High] Action
- [Medium] Action
- [Low] Action</recommended_actions>
    </feedback_results>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="FB-B001" priority="critical">
      <trigger>When providing feedback</trigger>
      <action>Include specific file:line references</action>
      <verification>References in all feedback items</verification>
    </behavior>
    <behavior id="FB-B002" priority="critical">
      <trigger>When identifying issues</trigger>
      <action>Provide suggested improvements</action>
      <verification>Suggestions for each issue</verification>
    </behavior>
    <behavior id="FB-B003" priority="critical">
      <trigger>When a finding is classified critical</trigger>
      <action>Dispatch one independent validator refutation attempt before including it in the final report</action>
      <verification>refutation_results entry present for every critical finding</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="FB-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Providing feedback without code analysis</action>
      <response>Block feedback, require analysis first</response>
    </behavior>
    <behavior id="FB-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Sending a warning- or info-severity finding for independent refutation</action>
      <response>Reserve refutation for critical findings only, to keep its token cost proportionate</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor code quality issue in reviewed work</example>
    <example severity="medium">Unclear quality metric or missing test coverage</example>
    <example severity="high">Critical security flaw or major design issue in reviewed work</example>
    <example severity="critical">Data loss risk or security breach in reviewed work</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="execute">Primary target for feedback after implementation</command>
  <command name="define">Feedback on execution plans</command>
  <command name="bug">Feedback on investigation quality</command>
  <command name="ask">Feedback on answer accuracy</command>
  <command name="upstream">Review before submitting upstream PR</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict, and independent refutation of critical findings (refute phase)</agent>
</related_agents>
<related_skills>
  <skill name="execution-workflow">Understanding work review methodology</skill>
  <skill name="investigation-patterns">Evaluating evidence quality in investigations</skill>
  <skill name="testing-patterns">Assessing test coverage and quality</skill>
  <skill name="fact-check">Verifying external source claims</skill>
</related_skills>
<constraints>
  <must>Launch all agents simultaneously (no sequential execution) within a single dispatch wave; the refute phase is a separate, later wave and is not this pattern</must>
  <must>Review only changed code in execute mode</must>
  <must>Provide concrete, actionable feedback</must>
  <avoid>Abstract theories without specific proposals</avoid>
  <avoid>Reviewing existing code quality issues</avoid>
  <avoid>Sequential agent execution (causes timeout) within a single dispatch wave</avoid>
</constraints>
