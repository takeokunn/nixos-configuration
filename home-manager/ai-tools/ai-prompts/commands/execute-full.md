---
argument-hint: [task-description]
description: Full task execution with feedback loop
---

<purpose>
Execute, review across every quality dimension, then fix what the review found — one flow, no user confirmation
  between phases. Eliminating those hand-offs is what this command buys over /execute.
</purpose>

<rules priority="critical">
  <rule>Exactly one fix iteration — a second pass means the implementation was too fragmented, a scope call for
    the user, not something more automation fixes.</rule>
  <rule>Write and run tests for all implemented functionality; failures feed the fix phase, never an excuse to
    complete quietly.</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state to escape a problem.
    SSOT-EXEMPT: restated because the failure is irreversible.</rule>
  <rule>No AI-slop prose anywhere this command writes — the report, every review and fix summary, commit
    messages, PR bodies, documentation, code comments. Delete on sight: announcements and closing restatements
    ("In this section", "Overall", "In summary", "It is worth noting"); empty intensifiers and self-praise
    ("robust", "comprehensive", "seamless", "successfully", "significantly"); informationless hedges
    ("essentially", "basically", "arguably"); formulaic parallelism ("not only X but also Y"); and any sentence
    carrying no fact the reader lacked. This is a correctness rule, not a style preference: padding is what
    makes an unverified claim read as a finished one, and "successfully implemented a robust solution" is the
    exact shape of a completion claim that names no command and no file:line. It applies hardest to the fix
    phase, where a fixed symptom described in praise rather than as a named diff hunk is indistinguishable from
    a symptom that stopped reproducing on its own.</rule>
  <rule>A commit message or PR body holds only what its reader needs to approve and cannot get anywhere else.
    Not the diff — it already shows every changed file, line, and function name. Not the commit history — it
    already shows how the work evolved. Not a CI check that already ran — the checks tab already shows its
    pass/fail and count. Write instead the judgment the diff can't show: why a workaround stands in for a root
    fix, what was deliberately left out of scope, and which verification no CI gate runs and had to be done by
    hand — name what was actually checked, not that it passed, since a selector matching nothing exits zero the
    same as a real one.</rule>
</rules>
<rules priority="important">
  <rule>Skip the fix phase when the review found nothing — say so explicitly instead of running it as a
    no-op.</rule>
  <rule>Fix only what the review identified — a broad rewrite here means a planning failure, and discards the
    review that justified it.</rule>
  <rule>Done is an enumerated set of commands exiting zero — test, plus lint/build/type-check where they exist.
    Report which ran and their exit status; a completion claim naming none isn't one.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load execution-workflow — it governs the delegation contract, definition of done, and review
        criteria all three phases use.</action>
      <tool>Skill (execution-workflow)</tool>
    </step>
    <step order="2">
      <action>Read both memory stores — memory_policy in CLAUDE.md splits them. Auto-memory (MEMORY.md index):
        traps this project already cost someone, and issues a previous run deferred — inherited work, not a
        clean slate. Serena: {feature}-patterns, {language}-conventions, testing-patterns, and any
        completion-checklist or canonical-gate memory — the last says which commands mean done here, and what
        they deliberately don't cover, without re-deriving it from build files. Querying only one store returns
        an empty result indistinguishable from a checked "found nothing".</action>
      <tool>Read (auto-memory MEMORY.md and the entries it names), Serena activate_project, list_memories,
        read_memory</tool>
      <output>Matched memory names per store, the ones loaded, and the deferred issues inherited</output>
    </step>
  </phase>

  <phase name="execute">
    <step order="1">
      <action>Split the work into atomic units with stated boundaries. Adding a member to an existing family
        (module, entity, test, command)? Enumerate its registration surfaces first — grep the nearest sibling's
        identifier repo-wide; every hit outside its own module is a required touch point, and sites naming no
        sibling are convention-discovered. A missed list produces silent failure: it compiles, and the feature
        is unreachable at runtime.</action>
      <tool>Grep</tool>
      <output>Atomic units with boundaries; required touch points and the convention-discovered sites</output>
    </step>
    <step order="2">
      <action>Assign an agent per unit, mark the independent ones, and define what proves it done in two lists —
        command-discharged (name it) and artifact-discharged (name the file:line). Anything else is a discussion
        point, not a checklist entry: a prose checkbox gets ticked by impression, and ticked boxes have approved
        a defective diff before.</action>
      <output>Assignments with parallel groups; the verification checklist split by what discharges each
        item</output>
    </step>
    <step order="3">
      <action>Before any code is written, send the planned placement to design — module/layer for each new
        symbol, and its dependencies. Placement is the costliest finding to repair, since fixing it means moving
        code and dependencies rather than rewriting — and with one fix iteration, a layering violation caught in
        the review wave has no budget left to correct.</action>
      <tool>Agent (design)</tool>
      <output>Placement approved, or the layer violation named before implementation</output>
    </step>
    <step order="4">
      <action>Delegate each unit with its scope, paths, deliverable, and verifying command. For a removed or
        migrated definition, have the assignee grep the identifier itself, not its usual call shape — forward
        declarations, differently-shaped call sites, comments, and test doubles share only the name.</action>
      <tool>Agent</tool>
    </step>
    <step order="5">
      <action>Establish what the verification command covers before running it — name isn't scope: config may
        exclude part of the tree, the editor/language server may read a different config than the command does,
        and a narrowed filter can still pull in shared fixtures. Confirm it covers what changed and that its
        input set saw files created this session — a tool snapshotting from version control or reading an
        explicit entry manifest silently skips an untracked new file.</action>
      <output>Covered scope, the config defining it, and confirmation the new work was included</output>
    </step>
    <step order="6">
      <action>Confirm the tree is quiescent before compiling — every write-capable agent has returned, no fix in
        flight. Compiling over an edit mixes generations: the suite exercises a stale unit while isolated
        source-preferred runs pass, two disagreeing results from the same source. Freeze edits, compile to
        completion, then run the suite fresh.</action>
      <tool>Bash (test runner)</tool>
      <output>Results and failing test names, feeding the review and fix phases</output>
    </step>
  </phase>

  <phase name="collect_feedback">
    <action>Dispatch all six review agents in one message — quality-assurance, security, design, docs,
      performance, test. They cover independent dimensions of the same output, so serializing only costs wall
      time.</action>
    <tool>Agent</tool>
    <output>Six reports</output>
  </phase>
  <reflection_checkpoint id="feedback_quality" after="collect_feedback">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>All six agents and what each returned; name any that timed out or died — a missing report is not an
      absence of issues.</check>
    <check>Per issue, the file:line or command output it cites — findings citing nothing checkable are a retry
      condition, not a clean result.</check>
    <check>The issues classified critical and the runtime impact making each one critical, or that none
      are.</check>
    <on_unmet>Re-run the named agent once with a narrower prompt naming the specific files. If it fails again,
      review that dimension here and report that the delegation failed.</on_unmet>
  </reflection_checkpoint>

  <phase name="fix_issues" condition="issues_found">
    <step order="1">
      <action>Consolidate the findings with execute's test failures, then confirm each still holds before acting
        — open the cited file:line and check the condition is present now. A parallel review reports the tree as
        it stood when read; a write after that makes the report stale without making it wrong — the citation was
        accurate when written, so re-scrutinizing it won't reveal the drift. One Read per finding beats fixing,
        and reporting, what's already fixed.</action>
      <output>Consolidated list, each marked still-present or already-resolved with the line that shows
        it</output>
    </step>
    <step order="2">
      <action>Prioritize still-present issues critical, then warning, then info; delegate each to the agent
        matching its category; verify each fix against its issue and re-run the verification commands.</action>
      <tool>Agent, Bash</tool>
      <output>Fixes with verification results</output>
    </step>
  </phase>
  <iteration_limit>1</iteration_limit>
  <reflection_checkpoint id="fix_complete" after="fix_issues">
    <check>Each critical issue and the file:line of the change addressing it, or the reason it was
      deferred.</check>
    <check>The commands re-run after the fixes and their exit status.</check>
    <check>For any symptom that stopped appearing, name the change that stopped it: "it doesn't happen anymore"
      is equally consistent with the fix working, a rebuild clearing a stale artifact, or an unreliable
      observation — without a named diff hunk, a cache clear gets recorded as an engineering win while the
      defect stays open.</check>
    <check>Any test added to guard a fix ran against the pre-fix state and failed there — a regression test
      never seen red asserts the fix rather than guarding it, and an arrange step that steers the system away
      from the tested condition looks like careful setup on inspection.</check>
    <check>Every issue left unaddressed — warnings judged infeasible included — with location and reason, in a
      form the next review can reconcile: unfixed and uncarried, a finding is rediscovered as new or not at all,
      and one fix iteration leaves no other tracking mechanism.</check>
    <on_unmet>Report the unaddressed issues as deferred, with reasons. Do not open a second fix
      iteration.</on_unmet>
  </reflection_checkpoint>

  <phase name="verify">
    <action>Dispatch verification against the completion claim itself, once the fixes land and before anything
      is reported done. Not a seventh review dimension — collect_feedback judges whether the work is good; this
      attacks whether the "it works" claim survives: boundary values, interrupted operations, idempotency, error
      paths the happy-path suite never entered. Give it the commands said to exit zero and the claim each
      supports; handed a diff, an agent just re-reviews the diff.</action>
    <tool>Agent (verification)</tool>
    <output>The claim attacked and what survived, or the input that broke it</output>
  </phase>

  <phase name="persist">
    <action>Write the fix phase's unaddressed issues to auto-memory as a ledger — one entry per issue:
      identifier, file:line, severity, deferral reason. fix_complete demands they survive "in a form the next
      review can reconcile against," and with one fix iteration that's the only mechanism — unwritten, a
      deferral is rediscovered as new or not at all. Then, against memory_policy in CLAUDE.md, capture what's
      expensive to re-derive and ungreppable: the canonical verification command and its blind spots; the exact
      zero-exit invocation, environment prefix and path flags included; and any abstraction deliberately
      unbuilt, paired with the condition that should re-open it — recorded without a trigger, a deferral gets
      re-argued next session with less information than this one had. memory_policy picks the store: the ledger
      and traps go to auto-memory, the symbol-anchored pattern to Serena. Then verify the memories read in
      prepare: bump, correct, or archive.</action>
    <tool>Read and Write (auto-memory MEMORY.md and its entries), Serena list_memories, write_memory or
      edit_memory, rename_memory</tool>
    <output>The ledger entries written, the memories written, edited, or archived — or "persist: no triggers
      matched — skip", which requires the deferred-issue list to be empty as well</output>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <check>Any required section absent or out of order, or that all are present.</check>
  <check>The branch or worktree the work happens in, confirmed not to be the default branch.</check>
  <on_unmet>Stop and resolve the gap before executing any phase.</on_unmet>
</reflection_checkpoint>

<agents>
  Roles this command dispatches. The subagent_type's own description is injected by the harness; what appears
    here is the contract this command adds.

  <agent name="design" subagent_type="design">Runs twice — on the planned placement before implementation, and
    on what was built in the review wave — every violation documented with its location.</agent>
  <agent name="quality" subagent_type="quality-assurance">Syntax, type safety, format; issues with severity and
    file:line evidence.</agent>
  <agent name="security" subagent_type="security">Vulnerabilities introduced by the change, with CWE and
    file:line.</agent>
  <agent name="test" subagent_type="test">Tests for the acceptance criteria plus the command that runs them.
    <constraint>Never pair an always-passing test with a comment explaining it can't be verified here — read the
    existing test helpers first, since the harness usually already has the capability, and a left-behind
    rationale suppresses the next attempt too. Remove a stub of this shape, don't keep it.</constraint>
  <constraint>A test guarding a specific fix must be run against the pre-fix state and observed to fail there
    before it counts as a regression test.</constraint>
  </agent>
  <agent name="docs" subagent_type="docs">Documentation for changed interfaces and behavior, no stale
    references.</agent>
  <agent name="performance" subagent_type="performance">Cost of the change, quantified only where measured on
    both sides.</agent>
  <agent name="debug" subagent_type="general-purpose">Failures during implementation or test execution.
    <constraint>A symptom that stopped appearing closes only when the change that stopped it is named — report
    the diff hunk, not the absence.</constraint>
  </agent>
  <agent name="refactor" subagent_type="general-purpose">Structure improvements preserving observable behavior.
    <constraint>Removing or migrating a definition? Grep the identifier itself across every file, not its
    typical usage shape — it's the only invariant shared by forward declarations, differently-shaped call sites,
    comments, and test doubles.</constraint>
  </agent>
  <agent name="verification" subagent_type="verification">Attacks the completion claim after the fixes land, not
    a seventh diff review — give it the commands claimed to exit zero and the claim each supports, not the
    diff.</agent>
  <agent name="memory" subagent_type="general-purpose">Decisions and patterns to whichever store memory_policy
    assigns them, and freshness of the memories consulted this task.</agent>
  <agent name="validator" subagent_type="validator" dispatch="on_demand">Re-derive one disputed claim from its
    citation alone, without the originating agent's reasoning — only when two agents disagree and evidence
    doesn't settle it, or a consequential claim carries none.</agent>

  For work outside these roles — dead code, error handling, migrations, schema, infrastructure, CI,
    observability — pick the matching subagent_type from the injected listing.
</agents>
<execution_graph>
  <sequential_phase id="execute" depends_on="none">
    <sequential_step id="placement_review">design, before any code is written</sequential_step>
    <parallel_group id="quality_assurance">quality, security</parallel_group>
    <parallel_group id="implementation">test, docs</parallel_group>
    <sequential_step id="consolidation">Wait for every write-capable agent, then compile and run the suite in a
      fresh process</sequential_step>
  </sequential_phase>
  <sequential_phase id="feedback" depends_on="execute">
    <parallel_group id="feedback_agents">quality, security, design, docs, performance, test</parallel_group>
  </sequential_phase>
  <conditional_phase id="fix" depends_on="feedback">
    <condition>Any agent reported an issue at critical or warning severity</condition>
    <skip_when>Every agent reported zero issues</skip_when>
    <pass_forward>The consolidated issues with the file:line each cites, the agent reports, and the test
      failures</pass_forward>
  </conditional_phase>
  <sequential_step id="verify" depends_on="fix">verification, against the settled post-fix
    artifact</sequential_step>
  <sequential_step id="persist_phase" depends_on="verify">memory</sequential_step>
</execution_graph>

<decision_criteria>
  <factor name="task_clarity" precedence="1">
    <unmet>The request admits two readings that produce different implementations. Ask with AskUserQuestion
      before delegating; do not implement the cheaper reading.</unmet>
  </factor>
  <factor name="implementation_quality" precedence="2">
    <unmet>No test command ran, or one failed. Run it, or record the failing test names as issues feeding the
      fix phase — an unverified implementation is not a completion candidate however clean the reviews
      look.</unmet>
  </factor>
  <factor name="feedback_severity" precedence="3">
    <unmet>Any agent reported a critical or warning issue. Enter the fix phase; skipping is permitted only when
      every agent reported zero.</unmet>
  </factor>
  <factor name="fix_completeness" precedence="4">
    <unmet>A critical issue is neither fixed-and-re-verified nor recorded as deferred with a reason. Report it
      as an open blocker rather than closing the cycle.</unmet>
  </factor>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md. verification carries the zero-exit commands enumerated for this change
    and which actually ran, plus the test command's scope and confirmation it saw files created this session.
    Add:

  <section name="files_modified">Path and what changed, per file.</section>
  <section name="review">Per agent: completed, timed out, or nothing checkable, with evidence tier. Then issues
    grouped critical/warning/info — category, location, and, for ones the fix phase saw, whether still present
    at fix time with the line showing it.</section>
  <section name="fixes">Per issue addressed: the finding and the fix, named as a change, not a symptom's
    disappearance. Then deferred issues with location and reason — replaced by an explicit skip confirmation
    when review found nothing.</section>
  <section name="weakest_claim">The claim resting on the thinnest evidence, and what would confirm it.</section>
  <section name="next_steps">Recommended follow-up, if any.</section>
</output>
