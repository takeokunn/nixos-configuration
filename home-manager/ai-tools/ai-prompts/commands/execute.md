---
argument-hint: [task-description]
description: Task execution command
---

<purpose>
Execute a task by delegating detailed work to sub-agents while holding policy decisions and orchestration.
Includes test self-healing: one targeted fix attempt when written tests fail. For a comprehensive multi-agent
quality review across every dimension, use /execute-full.
</purpose>

<rules priority="critical">
  <rule>Write tests for all implemented functionality and run them. An implementation whose tests were never
    executed is not complete, however clean it reads.</rule>
  <rule>At most one fix iteration for failing tests. Report what still fails as a blocker — a second automated
    pass hides a scope problem the user needs to decide on.</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state to escape a problem.
    SSOT-EXEMPT: restated because the failure is irreversible.</rule>
</rules>
<rules priority="important">
  <rule>Delegate detail; run independent units in parallel and dependent units in order. Verify a sub-agent's
    output before integrating it — a report citing nothing checkable is not a result.</rule>
  <rule>Done is an enumerated set of commands that exit zero — the project's test command, plus its lint,
    build, or type-check command where one exists. Report which of them actually ran.</rule>
  <rule>When a mechanical gate rejects an edit — an additive-only check, a formatter, a lint rule — add a new
    sibling element rather than rewording the existing one, so the original stays byte-identical and the new
    behavior stops depending on the old wording. Reword only when a sibling would be genuinely redundant.</rule>
  <rule>When aligning with a reference implementation, alignment is one-directional for anything that fails
    closed: security gates, verification strictness, and fail-closed defaults move from the looser side to the
    stricter one, never the reverse. A strictness the reference lacks is an asset, not a divergence.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <objective>Load what governs this run</objective>
    <step order="1">
      <action>Load execution-workflow. It carries the delegation contract, the definition of done, and the
        review criteria this command depends on.</action>
      <tool>Skill (execution-workflow)</tool>
    </step>
    <step order="2">
      <action>Activate the Serena project and call list_memories. Read only the entries whose names match this
        task — {feature}-patterns, {language}-conventions, testing-patterns, or a completion-checklist or
        verification-command memory for this project. A completion-checklist entry is what tells you which
        commands constitute done here without re-deriving it from build files. Read nothing if nothing
        matches; the index alone is the answer then.</action>
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
      <action>If the task adds one more of something that already exists — a module, entity, test, command —
        enumerate its registration surfaces before the first edit. Take the nearest existing sibling, grep its
        identifier across the repository, and treat every hit outside its own module as a required touch point;
        sites naming no sibling are convention-discovered and need no edit. Skipping this produces the failure
        that looks like success: everything compiles and the feature is unreachable because one explicit list
        was never updated.</action>
      <tool>Grep</tool>
      <output>Required touch points, and the sites confirmed convention-discovered</output>
    </step>
    <step order="3">
      <action>Select the best-fit agent per unit, mark which are independent, and order the rest by the
        specific output each waits on.</action>
      <output>Delegation map with parallel groups and the dependency behind each sequential step</output>
    </step>
    <step order="4">
      <action>Define what will prove the work done, in two lists: items a command discharges (name the command)
        and items an artifact discharges (name the file:line it will point at). An item carrying neither is a
        discussion point, not a checklist entry — a prose checkbox gets ticked by impression.</action>
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
        count and affected file list in the delegation prompt — a caller missed here surfaces as a failure the
        single fix iteration has no budget for. When a definition is removed or migrated, grep the identifier
        itself rather than the shape it is usually called in: forward declarations, differently-shaped call
        sites, comments, and test doubles share only the name.</action>
      <tool>Serena find_referencing_symbols, Grep</tool>
      <output>Blast radius: N references in M files, included in the delegation context</output>
    </step>
    <step order="2">
      <action>Dispatch each task with its scope, target paths, expected deliverable, the command that verifies
        it, and any reference implementation to follow.</action>
      <tool>Task</tool>
    </step>
  </phase>
  <reflection_checkpoint id="assignment_complete" after="assign">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>Every task in the inventory and the agent it went to, or that it is being done here and why. A task
      on neither list was dropped.</check>
    <check>The file paths and expected deliverable given to each agent. A prompt naming no path is not a
      delegation.</check>
    <check>The tasks that must wait, and the specific output each waits on.</check>
    <on_unmet>Do not dispatch. Supply the missing item, or ask with AskUserQuestion if only the user can
      resolve it.</on_unmet>
  </reflection_checkpoint>

  <phase name="consolidate">
    <objective>Integrate the results and establish that the verification verified this change</objective>
    <step order="1">
      <action>Check each agent's output for completeness, then call get_diagnostics_for_file (min_severity=2)
        on every modified file to catch language-server errors before running tests.</action>
      <tool>Serena get_diagnostics_for_file</tool>
      <output>Verified results; any diagnostics reported as blockers</output>
    </step>
    <step order="2">
      <action>Before running the project's verification command, establish what it actually covers. A command's
        name is not its scope, and it diverges in three ways: the configuration excludes part of the tree; the
        editor or language server reads a different configuration than the command does, so the two disagree
        about the same file; a narrowed filter still pulls in shared fixtures, so a failure need not belong to
        this change. Answer one question — does this command include what was changed? Then name what the gate
        itself writes into the tree — build output, coverage data, generated loaders — and route it to a
        temporary directory, since an ignored artifact will not even appear in a diff. Confirm the run starts
        its own service or fixture rather than attaching to one an earlier session left running.</action>
      <output>Covered scope with the config file defining it; what the gate writes; whether it self-starts</output>
    </step>
    <step order="3">
      <action>Run the test commands. Infer the command from the project's language and framework; failing that,
        from the package, build, or project manifest; failing that, report a blocker. Confirm the run's input
        set included the new work — a tool that snapshots from version control, honors an ignore file, or reads
        an explicit entry manifest silently skips a file created this session and not yet tracked. Check the
        new file appears in the tool's own file list, or the new test in the run count.</action>
      <tool>Bash (test runner)</tool>
      <output>Results with the command run, and confirmation the run saw the new files</output>
    </step>
    <step order="4">
      <action>If tests fail, delegate one targeted fix for the specific failing tests and re-run once. If
        failures remain, report them as blockers and set the status to error.</action>
      <tool>Task (test agent, or general-purpose)</tool>
    </step>
    <step order="5">
      <action>Before reporting that something could not be verified in this environment, grep the environment
        variables the application reads and its scripts directory for a substitute backend, an in-memory
        adapter, or a recorded-fixture mode. A codebase mature enough to have a test suite usually has a
        runnable driver behind that seam, and an unverifiable claim reported as a gap is rarely revisited.</action>
      <output>The substitute mode found and exercised, or confirmation none exists</output>
    </step>
  </phase>

  <phase name="persist">
    <objective>Capture orchestrator-level synthesis that no sub-agent could see</objective>
    <step order="1">
      <action>Against the memory_policy triggers in CLAUDE.md, three things this command produces are expensive
        to re-derive: the verification command in the exact form that exited zero, including any environment
        prefix and path flags, since a bare tool name costs the next session the same trial and error; the
        project's canonical gate and what it deliberately does not cover; and an abstraction deliberately not
        built together with the condition that should re-open it, since a deferral without its trigger gets
        re-argued from scratch with less information. Call list_memories to check the topic first, then write
        or edit. Output "persist: no triggers matched — skip" when none apply.</action>
      <tool>Serena list_memories, write_memory or edit_memory</tool>
      <output>Memory names written or edited, or the explicit skip</output>
    </step>
    <step order="2">
      <action>For the memories read in prepare: bump last-verified if still accurate, correct it if partly
        outdated, or rename with an -archived suffix if fully superseded. Do not read further memories only to
        check their freshness.</action>
      <tool>Serena edit_memory, rename_memory</tool>
      <output>Verified, updated, or archived — or "none read this task required verification"</output>
    </step>
  </phase>
</workflow>

<agents>
  Roles this command dispatches. The subagent_type's own description is injected by the harness and is not
  restated; what appears here is the contract this command imposes on top of it.

  <agent name="quality" subagent_type="quality-assurance">Syntax, type safety, format compliance on the
    modified files. Produces issues with severity and file:line evidence.</agent>
  <agent name="security" subagent_type="security">Vulnerabilities introduced by this change, with CWE and
    file:line.</agent>
  <agent name="test" subagent_type="test">Tests covering the acceptance criteria, plus the command that runs
    them, confirmed executable.
    <constraint>Never write a test that always passes together with a comment explaining why the behavior
      cannot be verified here. Read the existing test helpers first: the harness usually already has the
      capability, and the stale rationale left behind suppresses the next attempt as well.</constraint></agent>
  <agent name="docs" subagent_type="docs">Documentation for changed public interfaces and behavior, with no
    stale references left.</agent>
  <agent name="review" subagent_type="quality-assurance">Holistic post-implementation review across the agent
    reports and test results; go/no-go with rationale.</agent>
  <agent name="memory" subagent_type="general-purpose">Patterns and decisions surfaced by the implementation
    agents, written to Serena.</agent>
  <agent name="validator" subagent_type="validator" dispatch="on_demand">Re-derive one disputed claim from its
    citation alone, without the originating agent's reasoning. Dispatch only when two agents disagree and their
    evidence does not settle it, or a consequential claim rests on no citation.</agent>

  For work outside these roles — refactoring, debugging, performance, dead-code removal, error handling,
  migrations, schema, infrastructure, CI, observability — pick the matching subagent_type from the injected
  listing and give it the same four things every delegation carries.
</agents>
<execution_graph>
  <parallel_group id="quality_assurance" depends_on="none">quality, security</parallel_group>
  <parallel_group id="implementation" depends_on="none">test, docs</parallel_group>
  <sequential_step id="review_phase" depends_on="quality_assurance,implementation">review</sequential_step>
  <sequential_step id="persist_phase" depends_on="review_phase">memory</sequential_step>
</execution_graph>

<decision_criteria>
  <factor name="task_clarity" precedence="1">
    <unmet>The request admits two readings that produce different implementations. Ask with AskUserQuestion
      before delegating; do not implement the cheaper reading.</unmet>
  </factor>
  <factor name="verification_completeness" precedence="2">
    <unmet>No test command was run against the change. Run it before claiming completion. If none can be
      inferred from the manifests, report a blocker rather than completing unverified.</unmet>
  </factor>
  <factor name="implementation_quality" precedence="3">
    <unmet>A test failed, or get_diagnostics_for_file reports an error on a modified file. Delegate one
      targeted fix and re-run once; if it still fails, report a blocker rather than completing.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md. verification carries the test command, its exit status, what it
  covered, and confirmation the run saw the files created this session — or "none run" with the reason.
  Add: changes, as a path-per-line list of what was edited and why; and the weakest claim, with what would
  confirm it.
</output>
