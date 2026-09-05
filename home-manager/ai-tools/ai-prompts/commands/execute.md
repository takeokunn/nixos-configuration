---
argument-hint: [task-description]
description: Task execution command
---

<purpose>
Executes a task by delegating detail to sub-agents while holding policy and orchestration, with test
  self-healing — one targeted fix attempt on failure. For full multi-agent review across every dimension, use
  /execute-full.
</purpose>

<rules priority="critical">
  <rule>Write and run tests for all implemented functionality — untested code isn't complete, however clean it
    reads.</rule>
  <rule>One fix iteration only for failing tests — report remaining failures as a blocker; a second pass hides a
    scope problem that's the user's call.</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state to escape a problem.
    SSOT-EXEMPT: restated because the failure is irreversible.</rule>
  <rule>No AI-slop prose anywhere this command writes — the report, commit messages, PR bodies, documentation,
    code comments. Delete on sight: announcements and closing restatements ("In this section", "Overall", "In
    summary", "It is worth noting"); empty intensifiers and self-praise ("robust", "comprehensive", "seamless",
    "successfully", "significantly"); informationless hedges ("essentially", "basically", "arguably"); formulaic
    parallelism ("not only X but also Y"); and any sentence carrying no fact the reader lacked. This is a
    correctness rule, not a style preference: padding is what makes an unverified claim read as a finished one,
    and "successfully implemented a robust solution" is the exact shape of a completion claim that names no
    command and no file:line.</rule>
  <rule>A commit message or PR body holds only what its reader needs to approve and cannot get anywhere else.
    Not the diff — it already shows every changed file, line, and function name. Not the commit history — it
    already shows how the work evolved. Not a CI check that already ran — the checks tab already shows its
    pass/fail and count. Write instead the judgment the diff can't show: why a workaround stands in for a root
    fix, what was deliberately left out of scope, and which verification no CI gate runs and had to be done by
    hand — name what was actually checked, not that it passed, since a selector matching nothing exits zero the
    same as a real one.</rule>
</rules>
<rules priority="important">
  <rule>Delegate detail: run independent units in parallel, dependent ones in order, and verify output before
    integrating it — a report citing nothing checkable is not a result.</rule>
  <rule>Done means an enumerated set of commands exiting zero — test, plus lint, build, or type-check where one
    exists — report which actually ran.</rule>
  <rule>When a mechanical gate rejects an edit — additive-only check, formatter, lint rule — add a sibling
    element instead of rewording the original, keeping it byte-identical and decoupling new behavior from old
    wording; reword only if a sibling would be redundant.</rule>
  <rule>Aligning with a reference implementation is one-directional for anything fail-closed — security gates,
    verification strictness, fail-closed defaults — move looser to stricter, never the reverse; a strictness the
    reference lacks is an asset, not a divergence.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <objective>Load what governs this run</objective>
    <step order="1">
      <action>Load execution-workflow — it carries the delegation contract, definition of done, and review
        criteria this command needs.</action>
      <tool>Skill (execution-workflow)</tool>
    </step>
    <step order="2">
      <action>Activate the Serena project, call list_memories, and read only entries matching this task —
        {feature}-patterns, {language}-conventions, testing-patterns, or a
        completion-checklist/verification-command memory giving done-commands without re-deriving them from
        build files. Nothing matching means the index alone is the answer.</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>Matched memory names and the ones loaded, or "index matched nothing"</output>
    </step>
  </phase>

  <phase name="analyze">
    <objective>Establish the inventory, its touch points, and what will prove it done</objective>
    <step order="1">
      <action>Split the work into atomic units and state the boundary of each.</action>
      <output>Task inventory with boundaries</output>
    </step>
    <step order="2">
      <action>Adding one more of an existing kind — module, entity, test, command — means enumerating its
        registration surfaces first: take the nearest sibling, grep its identifier repo-wide, and treat every
        hit outside its own module as a required touch point (unmatched sites are convention-discovered, no edit
        needed). Skipped, this produces a false success: it compiles, but the feature is unreachable because one
        explicit list went unupdated.</action>
      <tool>Grep</tool>
      <output>Required touch points, and the sites confirmed convention-discovered</output>
    </step>
    <step order="3">
      <action>Select the best-fit agent per unit, mark which are independent, and order the rest by the specific
        output each waits on.</action>
      <output>Delegation map with parallel groups and the dependency behind each sequential step</output>
    </step>
    <step order="4">
      <action>Define what proves the work done as two lists — items a command discharges (name it) and items an
        artifact discharges (name the file:line) — an item carrying neither is a discussion point, not a
        checklist entry: a prose checkbox gets ticked by impression.</action>
      <output>Verification checklist split by what discharges each item</output>
    </step>
  </phase>
  <reflection_checkpoint id="analysis_quality" after="analyze">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>Each task in the inventory and the agent it is going to.</check>
    <check>The registration surfaces found in step 2, or that this task adds no new member of an existing
      family.</check>
    <check>Which tasks run in parallel, and the dependency forcing the rest to be sequential.</check>
    <check>The branch or worktree the work will happen in, confirmed not to be the default branch.</check>
    <on_unmet>Obtain the missing item before delegating.</on_unmet>
  </reflection_checkpoint>

  <phase name="assign">
    <objective>Delegate with enough context that the assignee does not guess</objective>
    <step order="1">
      <action>For a task modifying an existing symbol, call find_referencing_symbols and embed the reference
        count and affected files in the delegation prompt — a missed caller becomes a failure the single fix
        iteration can't afford. When removing or migrating a definition, grep the identifier itself, not its
        usual call shape: forward declarations, differently-shaped call sites, comments, and test doubles share
        only the name.</action>
      <tool>Serena find_referencing_symbols, Grep</tool>
      <output>Blast radius: N references in M files, included in the delegation context</output>
    </step>
    <step order="2">
      <action>Dispatch each task with its scope, target paths, expected deliverable, the command that verifies
        it, and any reference implementation to follow.</action>
      <tool>Agent</tool>
    </step>
  </phase>
  <reflection_checkpoint id="assignment_complete" after="assign">
    <check>Every task in the inventory and the agent it went to, or that it is being done here and why — a task
      on neither list was dropped.</check>
    <check>The file paths and expected deliverable given to each agent. A prompt naming no path is not a
      delegation.</check>
    <check>The tasks that must wait, and the specific output each waits on.</check>
    <on_unmet>Do not dispatch. Supply the missing item, or ask with AskUserQuestion if only the user can resolve
      it.</on_unmet>
  </reflection_checkpoint>

  <phase name="consolidate">
    <objective>Integrate the results and establish that the verification verified this change</objective>
    <step order="1">
      <action>Check each agent's output for completeness, then call get_diagnostics_for_file (min_severity=2) on
        every modified file to catch language-server errors before running tests.</action>
      <tool>Serena get_diagnostics_for_file</tool>
      <output>Verified results; any diagnostics reported as blockers</output>
    </step>
    <step order="2">
      <action>Before running the verification command, establish its actual coverage — name isn't scope, and it
        diverges three ways: config excludes part of the tree; editor or language-server config disagrees with
        the command's; a narrowed filter still pulls in shared fixtures, so a failure need not belong here. One
        question answers it: does it include what changed? Route whatever the gate writes into the tree — build
        output, coverage data, generated loaders — to a temp directory, since an ignored artifact won't appear
        in a diff. Confirm the run starts its own service or fixture, not one an earlier session left
        running.</action>
      <output>Covered scope with the config file defining it; what the gate writes; whether it
        self-starts</output>
    </step>
    <step order="3">
      <action>Run the test commands — infer from the project's language and framework, then its package, build,
        or manifest, then report a blocker if neither yields one. Confirm the run's input set included the new
        work: a tool that snapshots from version control, honors an ignore file, or reads an entry manifest
        silently skips an untracked new file — check it appears in the tool's file list, or the new test in the
        run count.</action>
      <tool>Bash (test runner)</tool>
      <output>Results with the command run, and confirmation the run saw the new files</output>
    </step>
    <step order="4">
      <action>If tests fail, delegate one targeted fix for the failing tests and re-run once. If failures
        remain, report them as blockers and set the status to error.</action>
      <tool>Agent (test agent, or general-purpose)</tool>
    </step>
    <step order="5">
      <action>Before reporting something unverifiable here, grep the app's environment variables and scripts
        directory for a substitute backend, in-memory adapter, or recorded-fixture mode — a codebase mature
        enough for a test suite usually has a runnable driver behind that seam, and an unverifiable claim
        reported as a gap is rarely revisited.</action>
      <output>The substitute mode found and exercised, or confirmation none exists</output>
    </step>
    <step order="6">
      <action>Once green, dispatch verification against the claim the change works — a green suite is the
        evidence most offered and least attacked, since it only shows the paths someone thought to write still
        behave, a different claim from the one being made. Hand over the zero-exit commands and the claim each
        supports, not the diff — an agent handed a diff reviews the diff, which review agents already
        did.</action>
      <tool>Agent (verification)</tool>
      <output>What survived the attack and what broke</output>
    </step>
  </phase>

  <phase name="persist">
    <objective>Capture orchestrator-level synthesis that no sub-agent could see</objective>
    <step order="1">
      <action>Per memory_policy in CLAUDE.md, three things here are expensive to re-derive: the verification
        command in its exact zero-exit form — environment prefix and path flags included, since a bare tool name
        costs the next session the same trial and error; the canonical gate and what it deliberately skips; and
        an abstraction deliberately not built, paired with the condition that should re-open it, since an
        untriggered deferral gets re-argued from scratch with less information. Check list_memories for the
        topic first, then write or edit — output "persist: no triggers matched — skip" when none apply.</action>
      <tool>Serena list_memories, write_memory or edit_memory</tool>
      <output>Memory names written or edited, or the explicit skip</output>
    </step>
    <step order="2">
      <action>For memories read in prepare: bump last-verified if still accurate, correct if partly outdated, or
        rename with an -archived suffix if fully superseded — don't read further memories only to check
        freshness.</action>
      <tool>Serena edit_memory, rename_memory</tool>
      <output>Verified, updated, or archived — or "none read this task required verification"</output>
    </step>
  </phase>
</workflow>

<agents>
  Roles this command dispatches — the subagent_type's own description is injected by the harness, not restated;
    what follows is this command's added contract.

  <agent name="quality" subagent_type="quality-assurance">Syntax, type safety, format compliance on modified
    files; issues carry severity and file:line evidence.</agent>
  <agent name="security" subagent_type="security">Vulnerabilities introduced by this change, with CWE and
    file:line.</agent>
  <agent name="test" subagent_type="test">Tests covering the acceptance criteria, plus the confirmed-executable
    command that runs them. <constraint>Never pair an always-passing test with a comment explaining why the
    behavior can't be verified — read the existing test helpers first, since the harness usually already has the
    capability, and a stale rationale left behind suppresses the next attempt too.</constraint></agent>
  <agent name="docs" subagent_type="docs">Documentation for changed public interfaces and behavior, with no
    stale references left.</agent>
  <agent name="review" subagent_type="quality-assurance">Holistic post-implementation review across the agent
    reports and test results; go/no-go with rationale.</agent>
  <agent name="verification" subagent_type="verification">Attacks the works-claim once the suite is green:
    boundary values, interrupted operations, idempotency, error paths a passing suite never entered. Give it the
    zero-exit commands and the claim each supports, not the diff — an agent handed a diff reviews the diff,
    which review agents already did.</agent>
  <agent name="memory" subagent_type="general-purpose">Patterns and decisions surfaced by the implementation
    agents, written to whichever store memory_policy assigns them.</agent>
  <agent name="validator" subagent_type="validator" dispatch="on_demand">Re-derive one disputed claim from its
    citation alone, without the originating agent's reasoning — only when two agents disagree and evidence
    doesn't settle it, or a consequential claim rests on no citation.</agent>

  For work outside these roles — refactoring, debugging, performance, dead-code removal, error handling,
    migrations, schema, infrastructure, CI, observability — pick the matching subagent_type from the injected
    listing and give it the same four things every delegation carries.
</agents>
<execution_graph>
  <parallel_group id="quality_assurance" depends_on="none">quality, security</parallel_group>
  <parallel_group id="implementation" depends_on="none">test, docs</parallel_group>
  <sequential_step id="review_phase" depends_on="quality_assurance,implementation">review</sequential_step>
  <sequential_step id="claim_attack" depends_on="review_phase">verification, against the settled
    artifact</sequential_step>
  <sequential_step id="persist_phase" depends_on="claim_attack">memory</sequential_step>
</execution_graph>

<decision_criteria>
  <factor name="task_clarity" precedence="1">
    <unmet>The request admits two readings that produce different implementations — ask with AskUserQuestion
      before delegating; do not implement the cheaper reading.</unmet>
  </factor>
  <factor name="verification_completeness" precedence="2">
    <unmet>No test command was run against the change — run it before claiming completion, or report a blocker
      if none can be inferred from the manifests.</unmet>
  </factor>
  <factor name="implementation_quality" precedence="3">
    <unmet>A test failed, or get_diagnostics_for_file reports an error on a modified file: delegate one targeted
      fix, re-run once, and report a blocker if it still fails.</unmet>
  </factor>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md; verification carries the test command, exit status, what it covered, and
    confirmation the run saw this session's new files — or "none run" with the reason. Add: changes as a
    path-per-line list of what was edited and why, and the weakest claim with what would confirm it.
</output>
