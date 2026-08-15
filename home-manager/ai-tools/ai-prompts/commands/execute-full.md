---
argument-hint: [task-description]
description: Full task execution with feedback loop
---

<purpose>
Execute, review across every quality dimension, then fix what the review found — in one flow, with no user
confirmation between phases. Eliminating those hand-offs is what this command buys over /execute.
</purpose>

<rules priority="critical">
  <rule>Exactly one fix iteration. A second pass means the initial implementation was too fragmented, which is
    a scope decision for the user rather than something more automation fixes.</rule>
  <rule>Write tests for all implemented functionality and run them. Failures are issues that feed the fix
    phase, never a reason to complete quietly.</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state to escape a problem.
    SSOT-EXEMPT: restated because the failure is irreversible.</rule>
</rules>
<rules priority="important">
  <rule>Skip the fix phase when the review found nothing, and say so explicitly rather than running it as a
    no-op.</rule>
  <rule>Fix only what the review identified. A broad rewrite during the fix phase indicates a planning failure
    and discards the review that justified it.</rule>
  <rule>Done is an enumerated set of commands that exit zero — the project's test command, plus its lint,
    build, or type-check command where one exists. Report which ran, with its exit status. A completion claim
    naming no command is not a completion claim.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load execution-workflow. It governs the delegation contract, the definition of done, and the
        review criteria all three phases rest on.</action>
      <tool>Skill (execution-workflow)</tool>
    </step>
    <step order="2">
      <action>Activate the Serena project, call list_memories, and read the entries matching this task —
        {feature}-patterns, {language}-conventions, testing-patterns, and any completion-checklist or
        canonical-gate memory for this project. That last category tells you which commands constitute done
        here, and what they deliberately leave uncovered, without re-deriving it from build files.</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>Matched memory names and the ones loaded</output>
    </step>
  </phase>

  <phase name="execute">
    <step order="1">
      <action>Split the work into atomic units with stated boundaries. If the task adds one more member of a
        family that already exists — a module, entity, test, command — enumerate its registration surfaces
        first: take the nearest sibling, grep its identifier repository-wide, and treat every hit outside its
        own module as a required touch point. Sites naming no sibling are convention-discovered. A missed
        explicit list produces the failure that looks like success: everything compiles and the feature is
        unreachable at runtime.</action>
      <tool>Grep</tool>
      <output>Atomic units with boundaries; required touch points and the convention-discovered sites</output>
    </step>
    <step order="2">
      <action>Assign an agent per unit, mark the independent ones, and define what will prove the work done in
        two lists: items a command discharges (name the command) and items an artifact discharges (name the
        file:line). An item carrying neither is a discussion point — a prose checkbox gets ticked by
        impression, and a wall of ticked boxes has approved a defective diff before.</action>
      <output>Assignments with parallel groups; the verification checklist split by what discharges each item</output>
    </step>
    <step order="3">
      <action>Before any code is written, send the planned placement to design: which module or layer each new
        symbol lands in and what it will depend on. Placement is the one finding whose repair costs as much as
        the implementation, because the fix is moving code and its dependencies rather than rewriting it — and
        with one fix iteration available, discovering a layering violation in the review wave leaves no budget
        to correct it.</action>
      <tool>Task (design)</tool>
      <output>Placement approved, or the layer violation named before implementation</output>
    </step>
    <step order="4">
      <action>Delegate each unit with its scope, paths, deliverable, and verifying command. Where a unit
        removes or migrates a definition, instruct the assignee to grep the identifier itself rather than the
        shape it is usually called in: forward declarations, differently-shaped call sites, comments, and test
        doubles share only the name.</action>
      <tool>Task</tool>
    </step>
    <step order="5">
      <action>Establish what the verification command covers before running it. Its name is not its scope: the
        configuration may exclude part of the tree, the editor or language server may read a different
        configuration than the command does, and a narrowed filter may still pull in shared fixtures so a
        failure need not belong to this change. Confirm it includes what changed, and that its input set saw
        files created this session — a tool that snapshots from version control or reads an explicit entry
        manifest silently skips an untracked new file.</action>
      <output>Covered scope, the config defining it, and confirmation the new work was included</output>
    </step>
    <step order="6">
      <action>Confirm the tree is quiescent before compiling or running the suite: every write-capable agent
        has returned and no fix is in flight. A compile overlapping an edit produces a mixed-generation
        artifact set, and the suite then exercises a stale unit while isolated source-preferred runs pass — two
        results from the same source that disagree. Freeze edits, compile to completion, then run the suite in
        a fresh process.</action>
      <tool>Bash (test runner)</tool>
      <output>Results and failing test names, feeding the review and fix phases</output>
    </step>
  </phase>

  <phase name="collect_feedback">
    <step order="1">
      <action>Dispatch all six review agents in one message — quality-assurance, security, design, docs,
        performance, test. They evaluate independent dimensions of the same output, so serializing them only
        costs wall time.</action>
      <tool>Task</tool>
      <output>Six reports</output>
    </step>
  </phase>
  <reflection_checkpoint id="feedback_quality" after="collect_feedback">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>All six agents and what each returned. Name any that timed out or died — a missing report is not an
      absence of issues.</check>
    <check>Per issue: the file:line or command output it cites. An agent whose findings cite nothing checkable
      is a retry condition, not a clean result.</check>
    <check>The issues classified critical and the runtime impact making each one critical, or that none
      are.</check>
    <on_unmet>Re-run the named agent once with a narrower prompt naming the specific files. If it fails again,
      review that dimension here and report that the delegation failed.</on_unmet>
  </reflection_checkpoint>

  <phase name="fix_issues" condition="issues_found">
    <step order="1">
      <action>Consolidate the findings with the test failures from execute, then confirm each still holds
        before acting: open the file:line it cites and check the condition is present now. A parallel review
        reports the tree as it stood when that agent read it, and any write between the read and the fix makes
        the report stale without making it wrong — the citation was accurate when written, so scrutinizing the
        evidence will not reveal the drift. The check costs one Read per finding; fixing what is already fixed
        and reporting it costs more.</action>
      <output>Consolidated list, each marked still-present or already-resolved with the line that shows it</output>
    </step>
    <step order="2">
      <action>Prioritize the still-present issues critical before warning before info, delegate each to the
        agent matching its category, then verify each fix against the issue it addressed and re-run the
        verification commands.</action>
      <tool>Task, Bash</tool>
      <output>Fixes with verification results</output>
    </step>
  </phase>
  <iteration_limit>1</iteration_limit>
  <reflection_checkpoint id="fix_complete" after="fix_issues">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>Each critical issue and the file:line of the change addressing it, or the reason it was deferred.</check>
    <check>The commands re-run after the fixes and their exit status.</check>
    <check>For any symptom that stopped appearing, the change that stopped it. "It does not happen any more" is
      equally consistent with the fix working, with a rebuild clearing a stale artifact, and with the
      observation being unreliable; without a named diff hunk, a cache clear gets recorded as an engineering
      result while the defect stays open.</check>
    <check>For any test added to guard a fix, that it was run against the pre-fix state and failed there. A
      regression test that has never been red is an assertion about the fix rather than a guard on it, and an
      arrange step steering the system away from the condition under test looks like careful setup on
      inspection.</check>
    <check>Every issue left unaddressed — including warnings judged infeasible — with its location and reason,
      in a form the next review can reconcile against. A finding neither fixed nor carried forward is
      rediscovered as new or not at all, and one fix iteration leaves no other mechanism for tracking it.</check>
    <on_unmet>Report the unaddressed issues as deferred, with reasons. Do not open a second fix iteration.</on_unmet>
  </reflection_checkpoint>

  <phase name="persist">
    <step order="1">
      <action>Against the memory_policy triggers in CLAUDE.md, capture what this cycle produced that is
        expensive to re-derive and undiscoverable by grep: the project's canonical verification command with
        what it deliberately does not cover; the exact invocation that exited zero, including environment
        prefix and path flags; and any abstraction deliberately not built, paired with the condition that
        should re-open it — a deferral recorded without its trigger is re-argued next session with less
        information than this one had. Then verify the memories read in prepare: bump, correct, or archive.</action>
      <tool>Serena list_memories, write_memory or edit_memory, rename_memory</tool>
      <output>Memories written, edited, or archived — or "persist: no triggers matched — skip"</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Per gate_discipline in CLAUDE.md.</gate>
  <check>Any required section absent or out of order, or that all are present.</check>
  <check>The branch or worktree the work happens in, confirmed not to be the default branch.</check>
  <on_unmet>Stop and resolve the gap before executing any phase.</on_unmet>
</reflection_checkpoint>

<agents>
  Roles this command dispatches. The subagent_type's own description is injected by the harness; what appears
  here is the contract this command adds.

  <agent name="design" subagent_type="design">Runs twice: on the planned placement before implementation, and
    on what was built in the review wave. Every violation documented with its location.</agent>
  <agent name="quality" subagent_type="quality-assurance">Syntax, type safety, format; issues with severity
    and file:line evidence.</agent>
  <agent name="security" subagent_type="security">Vulnerabilities introduced by the change, with CWE and
    file:line.</agent>
  <agent name="test" subagent_type="test">Tests for the acceptance criteria plus the command that runs them.
    <constraint>Never write an always-passing test alongside a comment explaining that the behavior cannot be
      verified here. Read the existing test helpers first: the harness usually already has the capability, and
      the rationale comment left behind suppresses the next attempt too. A stub of this shape is removed, not
      kept.</constraint>
    <constraint>A test guarding a specific fix must be run against the pre-fix state and observed to fail there
      before it counts as a regression test.</constraint></agent>
  <agent name="docs" subagent_type="docs">Documentation for changed interfaces and behavior, no stale
    references.</agent>
  <agent name="performance" subagent_type="performance">Cost of the change, quantified only where measured on
    both sides.</agent>
  <agent name="debug" subagent_type="general-purpose">Failures during implementation or test execution.
    <constraint>A symptom that stopped appearing closes only when the change that stopped it is named. Report
      the diff hunk, not the absence.</constraint></agent>
  <agent name="refactor" subagent_type="general-purpose">Structure improvements preserving observable
    behavior.
    <constraint>When removing or migrating a definition, grep the identifier itself across every file rather
      than the usage shape it typically appears in — the identifier is the only invariant shared by forward
      declarations, differently-shaped call sites, comments, and test doubles.</constraint></agent>
  <agent name="memory" subagent_type="general-purpose">Decisions and patterns to Serena, and freshness of the
    memories consulted this task.</agent>
  <agent name="validator" subagent_type="validator" dispatch="on_demand">Re-derive one disputed claim from its
    citation alone, without the originating agent's reasoning. Only when two agents disagree and their evidence
    does not settle it, or a consequential claim carries no citation.</agent>

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
  <sequential_step id="persist_phase" depends_on="fix">memory</sequential_step>
</execution_graph>

<decision_criteria>
  <factor name="task_clarity" precedence="1">
    <unmet>The request admits two readings that produce different implementations. Ask with AskUserQuestion
      before delegating; do not implement the cheaper reading.</unmet>
  </factor>
  <factor name="implementation_quality" precedence="2">
    <unmet>No test command was run, or a test failed. Run it, or record the failing test names as issues
      feeding the fix phase. An unverified implementation is not a completion candidate however clean the
      review reports look.</unmet>
  </factor>
  <factor name="feedback_severity" precedence="3">
    <unmet>Any agent reported a critical or warning issue. Enter the fix phase; skipping is permitted only when
      every agent reported zero.</unmet>
  </factor>
  <factor name="fix_completeness" precedence="4">
    <unmet>A critical issue is neither fixed-and-re-verified nor recorded as deferred with a reason. Report it
      as an open blocker rather than closing the cycle.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md. verification carries the enumerated commands that must exit zero for
  this change and which of them actually ran, with the test command's scope and confirmation it saw the files
  created this session. Add:

  <section name="files_modified">Path and what changed, per file.</section>
  <section name="review">Per agent: completed, timed out, or returned nothing checkable, with its evidence
    tier. Then the issues grouped critical, warning, info, each with category, location, and — for the ones
    the fix phase saw — whether it was still present at fix time, with the line that shows it.</section>
  <section name="fixes">Per issue addressed: the original finding and the change that fixed it, named as a
    change rather than as the disappearance of the symptom. Then the deferred issues with location and
    reason. Replaced by an explicit skip confirmation when the review found nothing.</section>
  <section name="weakest_claim">The claim resting on the thinnest evidence, and what would confirm it.</section>
  <section name="next_steps">Recommended follow-up, if any.</section>
</output>
