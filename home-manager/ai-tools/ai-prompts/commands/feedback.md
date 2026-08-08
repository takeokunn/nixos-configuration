---
argument-hint: [previous-command]
description: Review command for Claude Code's recent work
---

<purpose>
Multi-faceted review of Claude Code's work within the same session, automatically selecting appropriate review mode and executing efficiently in parallel.
</purpose>
<rules priority="critical">
  <rule>Launch all Task tools of one dispatch wave in a single message. This scopes to a wave — the
    selected mode's specialist agents, or the refute phase's per-critical-finding validator dispatches.
    The refute phase is a separate, later wave that runs once specialist findings exist; that sequencing
    is not the prohibited pattern.</rule>
  <rule>Anchor every finding to a file:line and pair it with a concrete fix. An unanchored observation
    costs the reader more than it returns.</rule>
</rules>
<rules priority="important">
  <rule>Auto-select the review mode from the previous command.</rule>
  <rule>In execute mode, review the code this session changed rather than pre-existing issues, since the
    user asked about the work just done.</rule>
  <rule>Target session operations rather than the git diff, which includes work this session did not
    do.</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Reviewing dimensions sequentially before synthesizing — all review agents operate on the same diff and are launched together</practice>
    <practice>Providing general impressions without anchoring to code — every item cites a file:line</practice>
    <practice>Waiting for the user to triage — severity is assigned here, by impact</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Parallelize all review dimensions; no dimension blocks another</principle>
    <principle>Anchor each finding to a location and include a concrete fix proposal</principle>
    <principle>Calibrate severity by runtime impact rather than style preference: critical is data loss or security, warning is degraded behavior, info is style</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load the execution-workflow skill with the Skill tool: it holds the code review criteria
        and the distinction between a convention-conformance review and a behavior review, which decide
        what this command's agents are looking for. Also load fact-check when the work under review
        makes claims about an external library or API that the review will have to check.</action>
      <tool>Skill (execution-workflow; fact-check when external claims are in scope)</tool>
      <output>Skills loaded, and which of them</output>
    </step>
    <step order="2">
      <action>Activate the Serena project and call list_memories.</action>
      <tool>Serena activate_project, list_memories</tool>
      <output>Project activated; full memory index</output>
    </step>
    <step order="3">
      <action>Classify the task as "review" and filter the memory index to {project}-conventions,
        code-quality-*, architecture-*, and any stored review of this component. Load only the matches
        with read_memory.</action>
      <tool>Serena read_memory</tool>
      <output>Matched memory names, and the ones loaded</output>
    </step>
    <step order="4">
      <action>Retrieve the findings from the previous review of this work, from the stored review
        memories or from earlier in the session, and record each one's identifier and location. This
        command otherwise runs as a fresh diagnosis every time, so a finding that was raised and not
        fixed is either rediscovered as new or missed entirely — and an item raised twice and still
        standing is a decision the user needs to make, not a line to repeat.</action>
      <output>Prior findings with identifiers and locations, or "no prior review found"</output>
    </step>
    <step order="5">
      <action>Pin what is being reviewed: the branch and commit, or the explicit list of files this
        session touched. Without it, a later session cannot tell which state the findings describe.</action>
      <tool>Bash: git rev-parse --short HEAD, git branch --show-current</tool>
      <output>Branch and commit under review, plus the file list</output>
    </step>
  </phase>
  <phase name="analyze">
    <step order="1">
      <action>Identify the previous command (/define, /execute, /bug, /ask, other).</action>
      <output>The command named; if none of these, mode is general</output>
    </step>
    <step order="2">
      <action>Establish the files and work to review.</action>
      <tool>Serena find_symbol, get_symbols_overview on the files touched this session</tool>
      <output>Explicit file list — paths, not "the recent changes"</output>
    </step>
    <step order="3">
      <action>Select the agents to run and the aspects the mode evaluates.</action>
      <output>Named agents from the selected mode, to be dispatched in one message</output>
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
  <reflection_checkpoint id="analysis_quality" after="select">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name the mode selected and the command that selected it.</check>
    <check>Name the files in review scope, as paths.</check>
    <check>Name the prior findings carried into this run, or state that no prior review was found.</check>
    <on_unmet>Resolve the missing item before dispatching.</on_unmet>
  </reflection_checkpoint>
  <phase name="execute">
    <step order="1">
      <action>Launch all agents of the selected mode in one message. Instruct each that a finding is
        complete only when it carries four things: the defect's file:line, the existing test that should
        have caught it with the reason it does not (compares through the same implementation, uses a
        fresh identifier, exercises only same-kind input, counts calls but not allocations, runs a single
        instance), the concrete fix, and the follow-up test that would close the gap. When a suite is
        green and the code is still wrong, the missing piece is not the defect's location but the
        explanation of why the current tests miss it — and a finding that cannot name that location has
        not actually identified the reproducing condition.</action>
      <tool>Task — every agent dispatched in a single message, never one per message</tool>
      <output>All agents of the selected mode dispatched together</output>
    </step>
    <step order="2">
      <action>Collect agent results.</action>
      <output>One report per agent, or a named agent that returned nothing</output>
    </step>
    <step order="3">
      <action>Re-read each finding's own analysis before keeping it. An item whose body concludes that
        the code is acceptable is deleted rather than demoted: severity is normally assigned from the
        pattern that triggered the search, before the code was read, and the reading that follows can
        dissolve it — but the heading is already written and the item already numbered, so nothing in
        the structure forces the retraction to propagate. A self-refuting entry left in place lands at
        the top of a priority list.</action>
      <output>Findings retained, and the ones deleted because their own analysis voided them</output>
    </step>
    <step order="4">
      <action>Check every quantitative claim in the reports, whichever agent produced it. A number that
        was not measured on both sides is stated as a direction instead — the rule is usually attached
        only to the performance and database agents, and any agent reviewing a change that touches
        performance can generate a plausible percentage as fluently as a prose observation.</action>
      <output>Unmeasured figures converted to directions, naming the agent and claim</output>
    </step>
  </phase>
  <reflection_checkpoint id="review_quality">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name every agent dispatched and what it returned. Name any that timed out or died without
      returning — a missing report is not a clean review.</check>
    <check>For each finding, name the file:line it cites, the existing test that misses it, and the
      concrete fix proposed. A finding without them is a retry condition, not a result.</check>
    <check>Name the severity assigned to each finding and the runtime impact justifying it — data loss
      or security for critical, degraded behavior for warning, style for info.</check>
    <check>For each prior finding carried in, state its current status: resolved, still present, or
      superseded — checked against the tree rather than against the earlier report.</check>
    <on_unmet>Re-run the named agent once with a narrower prompt naming the specific files. If it fails
      again, review that dimension here and report that the delegation failed.</on_unmet>
  </reflection_checkpoint>
  <phase name="refute">
    <objective>Independently refute critical-severity findings before they reach the user</objective>
    <step order="0">
      <action>Load core-patterns with the Skill tool when this phase has anything to do — it holds the
        adversarial verification escalation pattern this phase implements, including its own failure
        modes (false positives, rubber-stamp validation, shared blindspots between identical models).
        Skip the load when there are no critical findings.</action>
      <tool>Skill (core-patterns)</tool>
      <output>Skill loaded, or "no critical findings — skipped"</output>
    </step>
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
  <phase name="persist">
    <objective>Capture reusable review patterns to Serena memory</objective>
    <step order="1">
      <action>Evaluate the memory_auto_creation_triggers: did this review reveal a recurring quality
        issue, a reusable review pattern, or a project convention worth recording? Call list_memories,
        searching by topic substring rather than exact name, to see whether the topic already has an
        entry.</action>
      <tool>Serena list_memories</tool>
      <output>Trigger match: yes/no; existing memory: yes/no</output>
    </step>
    <step order="2">
      <action>On a match, use edit_memory for an existing topic or write_memory for a new one. Prepend
        the memory_content_format frontmatter (serena-usage skill); when editing a memory that lacks it,
        add it and update last-verified. Record the finding as a rule the next session can apply,
        pinned to the revision it was found in. If nothing matched, output "persist: no triggers
        matched — skip".</action>
      <tool>Serena edit_memory or write_memory</tool>
      <output>Memory name written or edited, or the explicit skip</output>
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
    <fact_check>Load the fact-check skill and follow it for external source verification via Context7</fact_check>
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
    <fact_check>Load the fact-check skill and follow it for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="general">
    <target>Recent Claude Code work</target>
    <agents>
      <agent name="review" subagent_type="quality-assurance" readonly="true">Comprehensive work review</agent>
      <agent name="complexity" subagent_type="code-quality" readonly="true">Code complexity review</agent>
      <agent name="memory" subagent_type="general-purpose" readonly="true">Consistency check with existing patterns</agent>
    </agents>
    <fact_check>Load the fact-check skill and follow it for external source verification via Context7</fact_check>
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
    <fact_check>Load the fact-check skill and follow it for external source verification via Context7</fact_check>
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
    <fact_check>Load the fact-check skill and follow it for external source verification via Context7</fact_check>
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
<decision_criteria>
  <factor name="review_depth" precedence="1">
    <unmet>A file in the review scope was never opened by any agent, or an agent reported on a file it
      did not read. Read it before reporting — a review of files nobody opened is not a review.</unmet>
  </factor>
  <factor name="feedback_actionability" precedence="2">
    <unmet>A finding names no file:line, proposes no concrete change, or cannot say which existing test
      should have caught it. Anchor it or drop it (FB-B001, FB-B002).</unmet>
  </factor>
  <factor name="issue_prioritization" precedence="3">
    <unmet>A finding carries no severity, its severity rests on style preference rather than runtime
      impact, or its own body concludes the code is acceptable. Reclassify by impact — data loss and
      security are critical, degraded behavior is warning, style is info — and delete the item outright
      when its analysis dissolved it.</unmet>
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
      <reviewed>The branch and commit, or the explicit file list, the findings below describe</reviewed>
      <agents_run>Each agent dispatched and what it returned; name any that returned nothing</agents_run>
      <verification>Any command run during review and its exit status, or "none run"</verification>
      <carried_forward>Per prior finding: identifier, location, and current status — resolved, still
        present, or superseded — or "no prior review found". A finding standing after two reviews is
        flagged as needing an explicit decision rather than repeated a third time.</carried_forward>
      <critical>Immediate Fix Required
- [Category] Issue: Location
- Problem: Description
- Missed by: the existing test at file:line, and why it passes anyway
- Fix: Proposal
- Follow-up: the test that would close the gap</critical>
      <refutation_results>Per critical finding: attempted (yes/no), outcome (survived | downgraded |
        unavailable), and the refuting evidence — or "no critical findings this run" when the set was
        empty</refutation_results>
      <warning>Fix Recommended
- [Category] Issue: Location
- Problem: Description
- Missed by: the existing test at file:line, and why it passes anyway
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
      <action>Cite a specific file:line, and name the existing test that should have caught the defect
        with the reason it does not — a finding that cannot name that test has not identified the
        reproducing condition</action>
      <verification>References in all feedback items</verification>
    </behavior>
    <behavior id="FB-B002" priority="critical">
      <trigger>When identifying issues</trigger>
      <action>Provide a concrete fix and the follow-up test that would close the gap</action>
      <verification>Suggestions for each issue</verification>
    </behavior>
    <behavior id="FB-B003" priority="important">
      <trigger>When a finding is classified critical</trigger>
      <action>Dispatch one independent validator refutation attempt before including it in the final
        report, since a critical finding acted on in error is expensive in both directions</action>
      <verification>refutation_results entry present for every critical finding</verification>
    </behavior>
    <behavior id="FB-B004" priority="important">
      <trigger>At the start of every review</trigger>
      <action>Load the previous review's findings and report each one's current status, because this
        command otherwise re-diagnoses from scratch and an unfixed finding disappears silently</action>
      <verification>carried_forward section present, or an explicit "no prior review found"</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="FB-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Providing feedback without code analysis</action>
      <response>Block feedback, require analysis first</response>
    </behavior>
    <behavior id="FB-P002" priority="important">
      <trigger>Always</trigger>
      <action>Sending a warning- or info-severity finding for independent refutation</action>
      <response>Reserve refutation for critical findings only, to keep its token cost proportionate</response>
    </behavior>
    <behavior id="FB-P003" priority="important">
      <trigger>When a finding's own analysis concludes the code is acceptable</trigger>
      <action>Demoting the finding to a lower severity and keeping it</action>
      <response>Delete the item. A self-refuting entry retained at any severity still occupies the
        reader's attention and can lead a priority list.</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation>
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
  <agent name="validator">Independent refutation of critical findings (refute phase)</agent>
</related_agents>
<related_skills>
  <skill name="execution-workflow">Code review methodology; loaded in the prepare phase</skill>
  <skill name="core-patterns">Adversarial verification escalation; loaded when the refute phase has work</skill>
  <skill name="investigation-patterns">Evaluating evidence quality in investigations</skill>
  <skill name="testing-patterns">Assessing test coverage and quality</skill>
  <skill name="fact-check">Verifying external source claims</skill>
</related_skills>
<constraints>
  <must>Launch all agents of a dispatch wave in one message; the refute phase is a separate, later wave and is not this pattern</must>
  <must>Review only changed code in execute mode</must>
  <must>Provide concrete, actionable feedback, each item naming the existing test that misses the defect</must>
  <must>Pin the report to the branch and commit, or the file list, it describes</must>
  <must>State a quantitative claim as a direction unless it was measured on both sides, whichever agent produced it</must>
  <avoid>Abstract theories without specific proposals</avoid>
  <avoid>Reviewing existing code quality issues</avoid>
  <avoid>Sequential agent execution within a single dispatch wave (causes timeout)</avoid>
</constraints>
