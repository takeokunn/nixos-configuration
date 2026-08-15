---
argument-hint: [previous-command]
description: Review command for Claude Code's recent work
---

<purpose>
Review the work done earlier in this session: pick the mode from the previous command, run that mode's
specialists in parallel, refute the critical findings, and report what to do about them.
</purpose>

<rules priority="critical">
  <rule>Every agent this command dispatches is read-only. It reviews work; it does not change it.</rule>
  <rule>Launch all Task calls of one dispatch wave in a single message. A wave is the selected mode's
    specialists, or the refute phase's per-finding validator dispatches. The refute wave running after the
    specialist wave is sequencing, not the prohibited pattern.</rule>
  <rule>A finding is complete only when it carries four things: the defect's file:line; the existing test that
    should have caught it and why it does not — compares through the same implementation, uses a fresh
    identifier, exercises only same-kind input, counts calls but not allocations, runs a single instance; the
    concrete fix; and the follow-up test that would close the gap. When a suite is green and the code is still
    wrong, the missing piece is not the defect's location but the explanation of why the current tests miss it,
    and a finding that cannot name it has not identified the reproducing condition.</rule>
</rules>
<rules priority="important">
  <rule>Review the work this session did, targeting session operations rather than the git diff — the diff
    includes work this session did not do.</rule>
  <rule>Severity is calibrated by runtime impact, not style preference: critical is data loss or security,
    warning is degraded behavior, info is style.</rule>
  <rule>State a quantitative claim as a direction unless it was measured on both sides — whichever agent
    produced it. The rule usually gets attached only to the performance and database agents, and any agent
    reviewing a performance-adjacent change can generate a plausible percentage as fluently as prose.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load execution-workflow: it holds the review criteria and the distinction between a
        convention-conformance review and a behavior review, which decide what the agents look for. Load
        fact-check as well when the work under review makes external claims the review must check.</action>
      <tool>Skill</tool>
      <output>Skills loaded</output>
    </step>
    <step order="2">
      <action>Activate the Serena project, call list_memories, and read the entries matching this review —
        {project}-conventions, code-quality-*, architecture-*, and any stored review of this component.</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>Matched memory names and the ones loaded</output>
    </step>
    <step order="3">
      <action>Retrieve the previous review's findings, from the stored review memories or earlier in the
        session, with each identifier and location. This command otherwise re-diagnoses from scratch every
        time, so an unfixed finding is either rediscovered as new or missed entirely — and an item raised twice
        and still standing is a decision the user needs to make, not a line to repeat.</action>
      <output>Prior findings with identifiers and locations, or "no prior review found"</output>
    </step>
    <step order="4">
      <action>Pin what is under review: the branch and commit, or the explicit list of files this session
        touched. Without it a later session cannot tell which state the findings describe.</action>
      <tool>Bash: git rev-parse --short HEAD, git branch --show-current</tool>
      <output>Branch and commit, plus the file list as paths</output>
    </step>
  </phase>

  <phase name="select">
    <step order="1">
      <action>Identify the previous command and select its mode from the modes table. Establish the files in
        scope as explicit paths, not "the recent changes".</action>
      <tool>Serena find_symbol, get_symbols_overview</tool>
      <output>Mode, the command that selected it, and the file list</output>
    </step>
  </phase>
  <reflection_checkpoint id="analysis_quality" after="select">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The mode selected and the command that selected it.</check>
    <check>The files in scope, as paths.</check>
    <check>The prior findings carried in, or that no prior review was found.</check>
    <on_unmet>Resolve the missing item before dispatching.</on_unmet>
  </reflection_checkpoint>

  <phase name="execute">
    <step order="1">
      <action>Dispatch the selected mode's agents in one message, each carrying the four-part finding
        requirement from the critical rules.</action>
      <tool>Task</tool>
      <output>One report per agent, or a named agent that returned nothing</output>
    </step>
    <step order="2">
      <action>Re-read each finding's own analysis before keeping it. An item whose body concludes the code is
        acceptable is deleted, not demoted: severity is normally assigned from the pattern that triggered the
        search, before the code was read, and the reading that follows can dissolve it — but the heading is
        already written and the item already numbered, so nothing in the structure forces the retraction to
        propagate, and a self-refuting entry left in place lands at the top of a priority list.</action>
      <output>Findings retained, and the ones deleted because their own analysis voided them</output>
    </step>
    <step order="3">
      <action>Check every quantitative claim in the reports and convert any figure not measured on both sides
        into a direction, naming the agent and the claim.</action>
      <output>Converted figures</output>
    </step>
  </phase>
  <reflection_checkpoint id="review_quality">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>Every agent dispatched and what it returned. Name any that timed out or died — a missing report is
      not a clean review.</check>
    <check>Per finding: the file:line, the existing test that misses it, and the concrete fix. A finding
      without them is a retry condition, not a result.</check>
    <check>Per finding: the severity and the runtime impact justifying it.</check>
    <check>Per prior finding carried in: resolved, still present, or superseded — checked against the tree
      rather than against the earlier report.</check>
    <on_unmet>Re-run the named agent once with a narrower prompt naming the specific files. If it fails again,
      review that dimension here and report that the delegation failed.</on_unmet>
  </reflection_checkpoint>

  <phase name="refute">
    <objective>Independently attack the critical findings before they reach the user</objective>
    <step order="1">
      <action>Select only the critical-severity findings. Warning and info are never sent: an independent
        adversarial pass costs materially more than a review pass, so it stays proportionate to what is
        consequential. When there are none, skip this phase and say so. Otherwise load core-patterns, which
        holds this escalation pattern together with its own failure modes — false positives, rubber-stamp
        validation, shared blindspots between identical models.</action>
      <tool>Skill (core-patterns)</tool>
      <output>The critical subset, or "no critical findings — refute skipped"</output>
    </step>
    <step order="2">
      <action>Dispatch exactly one validator per critical finding, all in one message, each in a fresh context
        holding only that finding's text and its cited file:line or command output — never the specialist's
        full report or reasoning, so the refutation is genuinely independent of the agent that raised it. Frame
        the task as "attempt to refute this finding", not "review this finding": a reviewer tends to confirm, a
        refuter is asked to find the flaw. Validator's own default framing expects several reports to compare,
        so state in the prompt that this is a single-claim independent-verification task and that it should
        disregard its usual single-source-means-unvalidated framing and re-derive whether the finding holds.</action>
      <tool>Task (validator)</tool>
      <output>One refutation attempt per critical finding</output>
    </step>
    <step order="3">
      <action>If a dispatch fails, times out, or returns nothing checkable, do not retry silently and do not
        drop the finding: record it as "refutation attempted, outcome unavailable" so it carries a visible
        trace rather than reaching the user unrefuted and unmarked. Where a refutation succeeds, downgrade the
        finding's evidence tier and carry both the original and the refuting evidence forward — never drop the
        disagreement. Where it does not overturn the finding, keep it and annotate that refutation was
        attempted and did not succeed.</action>
      <output>Every critical finding accounted for, including failed attempts</output>
    </step>
  </phase>
  <reflection_checkpoint id="refutation_quality" after="refute">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>Every critical finding and whether it was sent for refutation, or that there were none.</check>
    <check>Per refutation: survived or downgraded, and the evidence the refuting agent cited.</check>
    <check>That no warning- or info-severity finding was sent for refutation.</check>
    <on_unmet>Dispatch the missing refutation, or correct the scope, before reporting.</on_unmet>
  </reflection_checkpoint>

  <phase name="persist">
    <step order="1">
      <action>Against the memory_policy triggers in CLAUDE.md, record a recurring quality issue, a reusable
        review pattern, or a project convention as a rule the next session can apply, pinned to the revision it
        was found in. Search list_memories by topic substring first. Output "persist: no triggers matched —
        skip" when none apply.</action>
      <tool>Serena list_memories, write_memory or edit_memory</tool>
      <output>Memory name written or edited, or the explicit skip</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Per gate_discipline in CLAUDE.md.</gate>
  <check>Any required section absent or out of order, or that all are present.</check>
  <check>That every dispatched agent was read-only.</check>
  <on_unmet>Resolve the gap before dispatching.</on_unmet>
</reflection_checkpoint>

<modes>
  Selected from the previous command. Every mode dispatches its agents in one message, and loads fact-check
  where the work under review makes external claims.

  <mode name="define" after="/define" target="the execution plan from the session">
    Step granularity, dependencies, risk identification, completeness, feasibility.
    <agents>general-purpose for the plan, general-purpose for the estimation's validity</agents>
  </mode>
  <mode name="execute" after="/execute" target="the files modified via Edit/Write this session">
    <agents>quality-assurance for naming, DRY, readability; security for input validation and auth;
      design for architectural consistency; docs for accuracy and completeness; performance; test for
      coverage</agents>
  </mode>
  <mode name="bug" after="/bug" target="the investigation from the session">
    Evidence collection, hypothesis validity, root-cause accuracy, log use. Report the evidence tier of the
    root-cause claim, which log lines were actually cited, and which alternative hypotheses were ruled out by
    what.
    <agents>quality-assurance for methodology; general-purpose for log and dependency analysis; explore for
      code-path coverage</agents>
  </mode>
  <mode name="ask" after="/ask" target="the answer and its evidence">
    Citation quality, conclusion validity, and whether any claim is tagged verified without a command or
    file:line behind it. Report the tier per claim and whether each cited file:line was read rather than
    inferred from naming. design and performance are omitted: they evaluate questions, not answers.
    <agents>explore for evidence gathering; quality-assurance for accuracy; code-quality for reference
      precision</agents>
  </mode>
  <mode name="general" after="anything else" target="recent work">
    <agents>quality-assurance for the overall review; code-quality for complexity; general-purpose for
      consistency with existing patterns</agents>
  </mode>
</modes>

<decision_criteria>
  <factor name="review_depth" precedence="1">
    <unmet>A file in scope was never opened by any agent, or an agent reported on a file it did not read. Read
      it — a review of files nobody opened is not a review.</unmet>
  </factor>
  <factor name="feedback_actionability" precedence="2">
    <unmet>A finding names no file:line, proposes no concrete change, or cannot say which existing test should
      have caught it. Anchor it or drop it.</unmet>
  </factor>
  <factor name="issue_prioritization" precedence="3">
    <unmet>A finding carries no severity, rests on style preference rather than runtime impact, or its own body
      concludes the code is acceptable. Reclassify by impact, and delete the item outright when its analysis
      dissolved it.</unmet>
  </factor>
  <factor name="refutation_scope" precedence="4">
    <unmet>A critical finding reached the report without a refutation attempt, or a warning/info finding was
      sent for one. This factor governs the refute phase specifically and is evaluated after review_quality has
      gated severity, so an earlier factor firing in a different phase does not bypass it.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md, headed by the mode. Add:

  <section name="reviewed">The branch and commit, or the file list, these findings describe.</section>
  <section name="agents_run">Each agent dispatched and what it returned; name any that returned nothing.</section>
  <section name="carried_forward">Per prior finding: identifier, location, and current status — resolved,
    still present, or superseded — or "no prior review found". A finding standing after two reviews is flagged
    as needing an explicit decision rather than repeated a third time.</section>
  <section name="findings">Grouped critical, warning, info. Each carries category, location, the problem, the
    existing test that misses it with why it passes anyway, the fix, and the follow-up test.</section>
  <section name="refutation_results">Per critical finding: attempted yes/no, outcome survived | downgraded |
    unavailable, and the refuting evidence — or "no critical findings this run".</section>
  <section name="good_practice">What the work did well, by category.</section>
  <section name="fact_check_results">When fact-check ran: claims confirmed against a named source with the
    passage confirming each; claims the source does not confirm or confirms only by inference, with the
    inferential step and the correction; and claims that could not be checked.</section>
  <section name="recommended_actions">Ordered high, medium, low.</section>
</output>
