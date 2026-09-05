---
argument-hint: [previous-command]
description: Review command for Claude Code's recent work
---

<purpose>
Review this session's work: pick the mode from the previous command, run its specialists in parallel, refute
  critical findings, and report what to do.
</purpose>

<rules priority="critical">
  <rule>Every dispatched agent is read-only: it reviews, not changes. Writing this review's memory isn't a
    change, so persist proceeds.</rule>
  <rule>Launch one wave's Agent calls together: a wave is the mode's specialists, or refute's per-finding
    validators. Refute after the specialist wave is sequencing, not the banned pattern.</rule>
  <rule>A complete finding has four parts: file:line; the test that should have caught it and why not (same
    implementation, fresh identifier, same-kind input, calls not allocations, single instance); the fix; the
    follow-up test. When green but wrong, the gap is why tests miss it, not the location: without that, no
    reproducing condition.</rule>
</rules>
<rules priority="important">
  <rule>Target this session's own operations, not the git diff, which includes non-session work. With no
    Edit/Write history, take the file list from the commit pinned in prepare.</rule>
  <rule>Calibrate severity by runtime impact: critical is data loss or security, warning is degraded behavior,
    info is style.</rule>
  <rule>State a quantitative claim as a direction unless measured on both sides: usually performance and
    database agents, but any agent near a performance change can invent a plausible percentage as fluently as
    prose.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load execution-workflow for the review criteria and the conformance-vs-behavior distinction
        deciding what agents look for. Load fact-check too for external claims.</action>
      <tool>Skill</tool>
      <output>Skills loaded</output>
    </step>
    <step order="2">
      <action>Read auto-memory's history first: MEMORY.md indexes every entry, including persist's ledger. Then
        read matching Serena entries ({project}-conventions, code-quality-*, architecture-*) for conventions
        anchored to a symbol or file; the stores differ per memory_policy.</action>
      <tool>Read (auto-memory MEMORY.md and the entries it names), Serena activate_project, list_memories,
        read_memory</tool>
      <output>Matched memory names per store, and the ones loaded</output>
    </step>
    <step order="3">
      <action>Retrieve prior findings (identifier, location) from the persist ledger or earlier this session;
        otherwise this re-diagnoses from scratch: an unfixed finding is rediscovered or missed, and one raised
        twice is the user's call, not a repeat. Serena holds no such ledger: wrong store, not absence of prior
        reviews.</action>
      <output>Prior findings with identifiers and locations, or "no prior review found"</output>
    </step>
    <step order="4">
      <action>Pin what's under review (branch/commit, or the file list touched) so later sessions know which
        state the findings describe.</action>
      <tool>Bash: git rev-parse --short HEAD, git branch --show-current</tool>
      <output>Branch and commit, plus the file list as paths</output>
    </step>
  </phase>

  <phase name="select">
    <action>Select the mode from the modes table, files in scope as explicit paths, not "recent changes." With
      no Edit/Write history (invoked fresh), take paths from the commit pinned in prepare via git show --stat,
      noting scope came from the commit, not session operations.</action>
    <tool>Serena find_symbol, get_symbols_overview, Bash: git show --stat</tool>
    <output>Mode, the command that selected it, the file list, and the source the list came from</output>
  </phase>
  <reflection_checkpoint id="analysis_quality" after="select">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The mode and the command that selected it.</check>
    <check>The files in scope, as paths.</check>
    <check>Prior findings carried in, or that none were found.</check>
    <on_unmet>Resolve the missing item before dispatching.</on_unmet>
  </reflection_checkpoint>

  <phase name="execute">
    <step order="1">
      <action>Dispatch the mode's agents in one message, each carrying the four-part finding requirement from
        the critical rules.</action>
      <tool>Agent</tool>
      <output>One report per agent, or a named agent that returned nothing</output>
    </step>
    <step order="2">
      <action>Re-read each finding before keeping it: one concluding the code is fine gets deleted, not
        demoted. Severity is assigned pre-read from the triggering pattern; the later read can dissolve it, but
        nothing forces retraction once numbered: a self-refuting entry left in place tops the list.</action>
      <output>Findings retained, and the ones deleted because their own analysis voided them</output>
    </step>
    <step order="3">
      <action>Convert any figure not measured on both sides into a direction, naming the agent and the
        claim.</action>
      <output>Converted figures</output>
    </step>
  </phase>
  <reflection_checkpoint id="review_quality">
    <check>Every agent dispatched and what it returned: name any timed out or dead; a missing report isn't
      clean.</check>
    <check>Per finding: file:line, the test that misses it, the fix; without them it's a retry, not a
      result.</check>
    <check>Per finding: severity and the runtime impact justifying it.</check>
    <check>Per prior finding: resolved, still present, or superseded; checked against the tree, not the earlier
      report.</check>
    <on_unmet>Re-run the named agent once with a narrower prompt naming the specific files. If it fails again,
      review that dimension here and report the delegation failed.</on_unmet>
  </reflection_checkpoint>

  <phase name="refute">
    <objective>Independently attack the critical findings before they reach the user</objective>
    <step order="1">
      <action>Select critical findings only; warning and info never go: an adversarial pass costs far more than
        review. Skip and say so when none exist; otherwise load core-patterns for the escalation pattern and its
        failure modes: false positives, rubber-stamping, shared blindspots between identical models.</action>
      <tool>Skill (core-patterns)</tool>
      <output>The critical subset, or "no critical findings, refute skipped"</output>
    </step>
    <step order="2">
      <action>Dispatch one validator per critical finding, one message, each in a fresh context holding only
        that finding's text and its cited file:line or command output: never the specialist's report, for
        genuine independence. Frame it "refute this finding," not "review" it: a reviewer confirms, a refuter
        hunts the flaw. State this is single-claim, overriding validator's default multi-report
        framing.</action>
      <tool>Agent (validator)</tool>
      <output>One refutation attempt per critical finding</output>
    </step>
    <step order="3">
      <action>On a failed, timed-out, or unchecked dispatch, don't retry silently or drop the finding: record
        "attempted, outcome unavailable" so it stays traced, not unmarked. Where refutation succeeds, downgrade
        the tier and keep both evidences: never drop the disagreement. Otherwise keep it, annotated
        attempted-not-succeeded.</action>
      <output>Every critical finding accounted for, including failed attempts</output>
    </step>
  </phase>
  <reflection_checkpoint id="refutation_quality" after="refute">
    <check>Every critical finding and whether it was sent for refutation, or that there were none.</check>
    <check>Per refutation: survived or downgraded, and the refuting evidence cited.</check>
    <check>That no warning- or info-severity finding was sent for refutation.</check>
    <on_unmet>Dispatch the missing refutation, or correct the scope, before reporting.</on_unmet>
  </reflection_checkpoint>

  <phase name="persist">
    <action>Write the ledger of unresolved findings to auto-memory: identifier, file:line, severity, per entry.
      Prepare step 3 reads this next time; skipping it starts the next review from nothing (memory_policy
      exempts location-ledgers from the verdict ban it otherwise enforces). Also record a recurring quality
      issue, reusable pattern, or convention as a rule for the next session, pinned to its revision. Search the
      auto-memory index and Serena's list_memories by topic substring first, so an existing entry is edited, not
      duplicated. Output "persist: no triggers matched, skip" only when the ledger is empty too.</action>
    <tool>Read and Write (auto-memory MEMORY.md and its entries), Serena list_memories, write_memory or
      edit_memory</tool>
    <output>The ledger entries written, the memory names written or edited, or the explicit skip</output>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <check>Any required section absent or out of order, or that all are present.</check>
  <check>That every dispatched agent was read-only.</check>
  <on_unmet>Resolve the gap before dispatching.</on_unmet>
</reflection_checkpoint>

<modes>
  Selected from the previous command. Every mode dispatches its agents together and loads fact-check when
    reviewed work makes external claims: this injects the method only, so name the agent applying it (docs for
    documented-interface claims, quality-assurance otherwise) to check each claim against Context7 or the
    vendored source. Skip that and fact_check_results comes back empty despite claims being read.

  <mode name="define" after="/define" target="the execution plan from the session">
    Step granularity, dependencies, risk identification, completeness, feasibility. <agents>general-purpose for
      the plan, general-purpose for the estimation's validity</agents>
  </mode>
  <mode name="execute" after="/execute" target="the files modified via Edit/Write this session">
    <agents>quality-assurance for naming, DRY, readability; security for input validation and auth; design for
      architectural consistency; docs for accuracy and completeness; performance; test for coverage</agents>
  </mode>
  <mode name="bug" after="/bug" target="the investigation from the session">
    Evidence collection, hypothesis validity, root-cause accuracy, log use. Report the root-cause claim's
      evidence tier, cited log lines, and which alternatives were ruled out by what. <agents>quality-assurance
      for methodology; general-purpose for log and dependency analysis; explore for code-path coverage</agents>
  </mode>
  <mode name="ask" after="/ask" target="the answer and its evidence">
    Citation quality, conclusion validity, and whether any claim is tagged verified without a command or
      file:line. Report the tier per claim and whether each cited file:line was read, not inferred: design and
      performance are omitted, since they evaluate questions, not answers. <agents>explore for evidence
      gathering; quality-assurance for accuracy; code-quality for reference precision</agents>
  </mode>
  <mode name="general" after="anything else" target="recent work">
    <agents>quality-assurance for the overall review; code-quality for complexity; general-purpose for
      consistency with existing patterns</agents>
  </mode>
</modes>

<decision_criteria>
  <factor name="review_depth" precedence="1">
    <unmet>A file in scope went unopened by every agent, or an agent reported on a file it didn't read. Read it,
      since a review of unopened files isn't a review.</unmet>
  </factor>
  <factor name="feedback_actionability" precedence="2">
    <unmet>A finding names no file:line, proposes no concrete change, or can't say which test should have caught
      it. Anchor it or drop it.</unmet>
  </factor>
  <factor name="issue_prioritization" precedence="3">
    <unmet>A finding carries no severity, rests on style over impact, or its own body concludes the code is
      fine. Reclassify by impact; delete outright when its analysis dissolved it.</unmet>
  </factor>
  <factor name="refutation_scope" precedence="4">
    <unmet>A critical finding reached the report unrefuted, or a warning/info finding got refuted. This governs
      refute specifically, evaluated after review_quality gates severity, so an earlier factor elsewhere doesn't
      bypass it.</unmet>
  </factor>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md, headed by the mode. Add:

  <section name="reviewed">The branch and commit, or the file list, these findings describe.</section>
  <section name="agents_run">Each agent dispatched and what it returned; name any that returned
    nothing.</section>
  <section name="carried_forward">Per prior finding: identifier, location, status (resolved, still present, or
    superseded), or "no prior review found." Standing after two reviews flags an explicit decision, not a third
    repeat.</section>
  <section name="findings">Grouped critical, warning, info. Each carries category, location, the problem, the
    existing test that misses it with why it passes anyway, the fix, and the follow-up test.</section>
  <section name="refutation_results">Per critical finding: attempted yes/no, outcome survived | downgraded |
    unavailable, and the refuting evidence, or "no critical findings this run."</section>
  <section name="good_practice">What the work did well, by category.</section>
  <section name="fact_check_results">When fact-check ran: claims confirmed against a named source with the
    confirming passage; claims unconfirmed or only inferred, with the inferential step and correction; and
    claims that couldn't be checked.</section>
  <section name="recommended_actions">Ordered high, medium, low.</section>
</output>
