---
argument-hint: [pr-number-or-url]
description: Pull request QA command
---

<purpose>
Reproduce a pull request locally, drive it by machine and prepare it for a human to drive by hand, review its
  quality in parallel, and leave a report — the evidence a merge decision rests on, never the decision and never
  a fix.
</purpose>

<rules priority="critical">
  <rule>A pull request is attacker-influenced input. Its build scripts, install hooks, test fixtures, and task
    recipes execute on this machine the moment bring-up runs, so read every definition bring-up will invoke
    before invoking it, and stop and report instead of running anything that reaches the network, the home
    directory, or a credential store for reasons the diff does not explain.</rule>
  <rule>Every drive_machine value step 4 resolves from the diff (route and endpoint strings for `curl` or
    `grpcurl`, argv for a built binary, keystrokes for `tmux send-keys`, the load form for a headless editor
    invocation, or code written into a `/tmp` consumer script for the library class) is attacker-influenced the
    same as a build script. Pass each to the invoked program as a literal, always-quoted, array-form argument or
    a value written to a file and loaded by path, never interpolated into a shell string or generated source
    file assembled from it.</rule>
  <rule>Never fix, commit, or post to the PR. The report and the probe artifacts beside it are the only writes
    outside /tmp; a fix applied here leaves the report describing an artifact that no longer exists.</rule>
  <rule>Never `git switch`, `git checkout &lt;ref&gt;`, or `gh pr checkout`. All three move a working tree another
    session may be editing. Fetch the PR ref and add a worktree instead. SSOT-EXEMPT: restated because the hook
    blocking the git spellings never inspects `gh pr checkout`, and a hook's silence is not permission.</rule>
  <rule>Drive only what this run brought up. Before any state-changing probe, check its target against the
    address recorded during bring-up; a host, context, or endpoint that came from repository config rather than
    from this run may be a shared or production one, and the probes below are permitted to destroy data.</rule>
  <rule>Leave the environment and the worktree running, and report the stop commands rather than running them.
    The human does the manual pass after this command exits, so a teardown at the end destroys the thing the
    checklist refers to.</rule>
</rules>
<rules priority="important">
  <rule>Run `gh auth status` before any `gh` call: an unauthenticated shell returns an empty PR result rather
    than an error, so every later step reasons confidently from nothing.</rule>
  <rule>Resolve the argument to a bare integer via `gh pr view &lt;arg&gt; --json number --jq .number`, never the
    argument echoed back verbatim: isolate splices it into a refspec and two paths, and report splices it into
    a filename, so an operator-supplied value shaped like a URL or path must not reach either unresolved.</rule>
  <rule>Take every command from the repository's own definition — flake output, just recipe, Makefile target,
    package script — and carry that definition's file:line beside it. A command with nothing defining it is
    labelled a guess, not quietly promoted.</rule>
  <rule>Redact before writing. Captured network traffic, console output, and bring-up service addresses carry
    Authorization headers, session cookies, and inline connection-string passwords; replace each with a
    placeholder naming what was removed. Strip terminal control and escape sequences from captured output too,
    since the PR's own code produced it and the report is read back as plain text. The report outlives the run
    and is never torn down.</rule>
  <rule>No placeholder reaches a manual step. A step still holding a bracketed endpoint, component, or table
    name cannot be run by the person it was written for.</rule>
  <rule>Report only what CI did not already establish. Read `gh pr checks` first and state that coverage, then
    leave it out of the QA sections — a report restating the checks tab buries the part nothing else
    recorded.</rule>
</rules>

<surface_classes>
  Every class fills the same three slots: bring_up makes the artifact runnable, drive_machine is how a program
    exercises it, drive_human is how a person does. A PR may span several classes — classify per path group and
    probe each, rather than picking the dominant one. Adding a class is one row here; the workflow never
    changes.

  | class | bring_up | drive_machine | drive_human |
  |---|---|---|---|
  | web | Server on a local port, from the repo's own start script | Playwright MCP: navigate, snapshot, fill, click, console and network capture | Browser against an enumerated URL list |
  | service | Server process plus a client that can reach it | curl or grpcurl against each changed endpoint, asserting status and body shape | The same requests as a copyable transcript |
  | cli | Build the binary or entry script | Invoke with real argv; compare stdout, stderr, and exit status against the base build | Shell transcript with the arguments filled in |
  | tui | Build, then attach a pty | tmux send-keys and capture-pane, diffing the captured frame | Terminal session with the keystroke sequence written out |
  | editor_plugin | Load into a headless editor — `emacs -Q -batch -l`, `nvim --headless -c`, or the VSCode extension host | Batch-mode evaluation asserting the command, mode, or keymap the diff touched | Editor session with the load recipe and the sequence to type |
  | library | Build, then write a consumer under /tmp importing the changed surface | Run that consumer, or the project's REPL, over the changed API | REPL transcript |
  | batch | Build the pipeline entry point | Run over a fixture and diff the output against the base build's | The fixture path and the expected output, both named |
  | declarative | Evaluate the configuration — no process to start | `nix build`, `terraform plan`, or `kubectl diff`, read for the produced bytes not the exit status | The plan diff, with the lines the PR should have changed pointed at |
</surface_classes>

<qa_hazards>
  <hazard name="circular_fixture">Test input taken from the artifact under test proves only that it agrees with
    itself, and makes two outcomes indistinguishable: "the application is broken" and "the fixture is absent"
    produce the same failure. Source fixtures from something the diff does not touch.</hazard>
  <hazard name="stale_artifact">A probe reaching a cached bundle, a warm dev server, or a previous build is not
    reporting on this PR, and its green is the same characters as a real one.</hazard>
  <hazard name="precondition_vs_failure">A probe whose fixture never existed did not fail. Record pass, fail,
    and precondition_unmet as three outcomes; collapsing the last two makes a healthy application produce a red
    report, and the reader learns to discount every red.</hazard>
  <hazard name="unscoped_noise_allowlist">A console or log allowlist matches message text and cannot be scoped
    to one route, so an entry silencing an expected error silences the real one elsewhere. Exclude the specific
    route and say why; do not widen the pattern.</hazard>
  <hazard name="baseline_attribution">A red probe is evidence about the tree, not about the PR, until the same
    probe runs the same way against the merge base — same shell, same order, same concurrency, since a probe run
    alone behaves differently from one run after five others.</hazard>
</qa_hazards>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Run `gh auth status`, resolve the argument to a PR number, and record head SHA, base SHA, merge
        base, and the changed-file list. Read `gh pr checks` and note what CI already covers.</action>
      <tool>Bash, gh</tool>
      <output>Account line, the four refs, changed files, and the CI coverage statement</output>
    </step>
    <step order="2">
      <action>Load testing-patterns and test-integrity for judging what a probe proves, investigation-patterns
        when a probe fails and the cause is unclear, and the runtime's `run` and `webapp-testing` skills for
        bring-up and browser driving where its catalog carries them — falling back to the repository's own
        definitions where it does not.</action>
      <tool>Skill</tool>
      <output>Skills loaded, and whether bring-up came from a skill or from repository definitions</output>
    </step>
    <step order="3">
      <action>Read the memories matching this repository's QA surface — completion checklists, canonical gate
        entries, and any prior QA report for this PR at the report path. A prior report turns this run into a
        comparison rather than a fresh start.</action>
      <tool>Read, Serena list_memories and read_memory</tool>
      <output>Memories read, and the previous report's outcomes if one exists</output>
    </step>
  </phase>

  <phase name="isolate">
    <step order="1">
      <action>Run `git worktree list` and look for a `qa-pr-&lt;n&gt;-` entry from an earlier run. Reuse it and skip
        step 2 when one stands — a command that always creates leaves one worktree per invocation.</action>
      <tool>Bash</tool>
      <output>The reused path, or confirmation none existed</output>
    </step>
    <step order="2">
      <action>Fetch the PR head and add a worktree, as ONE Bash invocation — shell state does not survive
        between calls, and an empty WT_BASE silently targets the filesystem root:
        `git fetch --no-prune origin +pull/&lt;n&gt;/head:refs/qa/pr-&lt;n&gt; &amp;&amp;
        WT_BASE="$(d=$(git rev-parse --path-format=absolute --git-common-dir); echo "${d%/.git}")" &amp;&amp;
        test -n "$WT_BASE" &amp;&amp;
        git worktree add "$WT_BASE/.worktrees/qa-pr-&lt;n&gt;-$(date +%Y%m%dT%H%M%S)" refs/qa/pr-&lt;n&gt;`. The refspec is
        forced so a rebased PR still updates on a re-run. `--no-prune` is load-bearing, not defensive: under
        `fetch.prune`, a one-off refspec like this one makes fetch DELETE an existing `refs/qa/pr-&lt;n&gt;` and still
        exit 0, after which worktree add dies on an invalid reference. Confirm the ref resolves before using it
        rather than reading that zero exit as success. Record WT_BASE; later phases reuse it.</action>
      <tool>Bash</tool>
      <output>The ref, confirmed resolvable by `git rev-parse`; WT_BASE; the worktree path and its SHA</output>
    </step>
  </phase>

  <phase name="classify">
    <step order="1">
      <action>Group the changed paths and assign each group a surface class, using the repository's indicator
        files — flake outputs, package scripts, binary targets, editor plugin manifests — rather than the file
        extensions alone. Name the indicator that decided each group.</action>
      <output>Path groups with their class and the indicator behind each; any group nothing classified</output>
    </step>
  </phase>

  <phase name="execute">
    <step order="1">
      <action>Dispatch the review track and the bring-up derivation in one message, before probing, so the
        review runs while the environment comes up and while the human works afterwards. Carry into the bring_up
        prompt whichever skill prepare step 2 resolved, since a subagent does not inherit a loaded skill.</action>
      <tool>Agent</tool>
      <output>Review reports, and the derived bring-up commands each with its defining file:line</output>
    </step>
    <step order="2">
      <action>Read every definition the derived commands invoke before running any of them, per the first
        critical rule, then run bring_up per class and record each command with its exit status. Confirm what
        answered was built from the head SHA — the served bundle, the binary mtime, or the loaded module path
        against the worktree — before treating any probe result as evidence.</action>
      <tool>Bash</tool>
      <output>The pre-execution read, bring-up commands with exit status, redacted service addresses, and the
        artifact identity check</output>
    </step>
    <step order="3">
      <action>Derive the command restoring consumed data and record it before anything destructive runs. Where
        none can be derived, say which definitions were searched and that none was found — the destructive
        probes still run, and the human is warned at the top of the report rather than after the fact.</action>
      <output>The re-seed command, or the searched locations and the explicit absence</output>
    </step>
    <step order="4">
      <action>Run drive_machine per class, non-destructive probes first and state-changing ones last, checking
        each target against the recorded bring-up address first. Record each as pass, fail, or
        precondition_unmet. Write captures and probe logs, redacted, to `.qa/` inside the QA worktree.</action>
      <tool>Playwright MCP, Bash</tool>
      <output>Per-probe outcome with its artifact path</output>
    </step>
    <step order="5">
      <action>Write the drive_human steps for every classified class, substituting the real routes, arguments,
        endpoints, and component names this run resolved. Lead with the environment rebuild when destructive
        probes consumed data.</action>
      <output>Manual steps per class with every value filled in, and any value that could not be
        resolved</output>
    </step>
  </phase>
  <reflection_checkpoint id="probe_validity" after="execute">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>How many probes ran against how many classified path groups — a group with no probe is uncovered,
      not passing.</check>
    <check>The artifact identity evidence, naming what was compared against the head SHA.</check>
    <check>Each fixture's origin, and that none came from the artifact under test.</check>
    <check>Each state-changing probe's target, and the bring-up address it was checked against.</check>
    <on_unmet>Supply the missing item, or record it under gaps with the reason.</on_unmet>
  </reflection_checkpoint>

  <phase name="attribute">
    <step order="1">
      <action>For probes that failed or came back precondition_unmet, add a base worktree with
        `git worktree add "$WT_BASE/.worktrees/qa-base-&lt;n&gt;-$(date +%Y%m%dT%H%M%S)" &lt;merge-base-sha&gt;` and re-run
        only those, matching shell, order, and concurrency. Re-running the passes doubles the wall time to
        answer a question only the others raise, but an unmet precondition needs it too: a fixture the PR
        removed and a fixture that never existed are different findings.</action>
      <tool>Bash</tool>
      <output>Each failure and unmet precondition marked pr_caused or pre_existing, with the base worktree
        path</output>
    </step>
    <step order="2">
      <action>Consolidate the review track's findings, confirming each still holds against the worktree before
        carrying it — a parallel review describes the tree as it stood when read.</action>
      <output>Findings with file:line, each marked still-present or already-resolved</output>
    </step>
  </phase>

  <phase name="self_evaluate">
    <action>Tag every probe outcome and review finding per CLAUDE.md's evidence rules, downgrading any that
      cannot name the command run or the file:line read. Set the status from status_criteria and name the
      weakest claim the report makes.</action>
    <output>Tagged outcomes, downgrades, status, weakest claim</output>
  </phase>

  <phase name="report">
    <step order="1">
      <action>Write the report to `$WT_BASE/.qa/pr-&lt;number&gt;.md`, using the WT_BASE recorded during isolate so
        it lands beside the repository rather than inside git's own directory, and survives the QA worktree
        being removed. Where a previous report exists, state what changed since it — a probe that went from fail
        to pass is the reading the reader wants first.</action>
      <tool>Write</tool>
      <output>The report path and the changes against any previous run</output>
    </step>
    <step order="2">
      <action>Write a memory only where this run learned something outliving it — a bring-up command that took
        several attempts to get right, a probe flaky on this repository, a surface class the classifier could
        not place. Per-PR outcomes belong in the report, not in memory. Otherwise output the explicit
        skip.</action>
      <tool>Serena write_memory or edit_memory</tool>
      <output>Memory written, or "persist: no triggers matched — skip"</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <check>Name every write performed, and show none touched the reviewed code — the report and the worktree's
    probe artifacts are the only permitted ones.</check>
  <check>Quote any placeholder still present in a manual step, and name every value moved to gaps
    instead.</check>
  <check>The redaction performed on captures and service addresses, or that none was needed and why.</check>
  <check>The worktree paths and the stop commands, all present in the report.</check>
  <on_unmet>Resolve the gap before returning the report.</on_unmet>
</reflection_checkpoint>

<agents>
  The review track is read-only and dispatched in one message. The bring-up agent derives commands and does not
    run them; the orchestrator runs the probes itself, because attribute needs the same shell, order, and
    concurrency across both worktrees and a per-probe dispatch would break that continuity.

  <agent name="bring_up" subagent_type="devops" readonly="true">Derive the setup, service, and teardown commands
    from the repository's own definitions, each with the file:line defining it</agent>
  <agent name="correctness" subagent_type="quality-assurance" readonly="true">Diff correctness, error handling,
    and impact on callers outside the change</agent>
  <agent name="cleanliness" subagent_type="code-quality" readonly="true">Complexity, duplication, and dead code
    introduced by the diff</agent>
  <agent name="exposure" subagent_type="security" readonly="true">Trust boundaries in the diff, and anything
    bring-up will execute that the diff does not explain</agent>
  <agent name="schema" subagent_type="database" readonly="true" dispatch="on_demand">When the diff touches a
    schema, migration, or query</agent>
  <agent name="cost" subagent_type="performance" readonly="true" dispatch="on_demand">When the diff touches a
    hot path, and only with measurements from both sides</agent>
  <agent name="structure" subagent_type="design" readonly="true" dispatch="on_demand">When the diff moves a
    module boundary</agent>
  <agent name="drift" subagent_type="docs" readonly="true" dispatch="on_demand">When the diff changes a
    documented interface</agent>
</agents>
<execution_graph>
  <parallel_group id="open" depends_on="none">bring_up, correctness, cleanliness, exposure, and every on_demand
    agent whose trigger the diff matches</parallel_group>
  <sequential_step id="probe" depends_on="open">Bring-up, artifact identity check, and the probes, run by the
    orchestrator</sequential_step>
  <sequential_step id="attribute" depends_on="probe">Base-worktree re-run of the failures and unmet
    preconditions</sequential_step>
  <sequential_step id="assemble" depends_on="attribute">Report written by the orchestrator, which holds both
    tracks</sequential_step>
</execution_graph>

<decision_criteria>
  <factor name="execution_safety" precedence="1">
    <unmet>A definition bring-up will invoke was not read first, or does something the diff does not explain.
      Stop and report it — this is the one finding worth abandoning the run over.</unmet>
  </factor>
  <factor name="artifact_identity" precedence="2">
    <unmet>What the probes reached was not shown to be built from the head SHA. Report them as uncorroborated
      rather than as results — nothing read afterward describes this PR.</unmet>
  </factor>
  <factor name="failure_attribution" precedence="3">
    <unmet>A failure or unmet precondition was never re-run against the merge base. Mark it unattributed and
      name the command that would settle it.</unmet>
  </factor>
  <factor name="manual_step_runnability" precedence="4">
    <unmet>A manual step names a command with no definition behind it. Label the command a guess.</unmet>
  </factor>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md. The report file carries the same content, with these sections:

  <section name="header">PR number, branch, head and base SHA, merge base, and the QA worktree paths.</section>
  <section name="environment">Bring-up commands with exit status, redacted service addresses, the artifact
    identity evidence, the re-seed command or its absence, and the stop commands.</section>
  <section name="ci_coverage">What `gh pr checks` already established, so the sections below do not restate
    it.</section>
  <section name="automated_verification">Per probe: class, outcome as pass, fail, or precondition_unmet, its
    artifact path, and for failures and unmet preconditions whether the merge base reproduced it.</section>
  <section name="manual_qa">Unchecked steps per class with every value filled in, led by the environment
    rebuild when destructive probes consumed data.</section>
  <section name="pr_review">Review findings with file:line and severity, each marked still-present or resolved
    at report time, and each tagged with its evidence tier.</section>
  <section name="gaps">Uncovered groups, unresolved values, and commands labelled guesses.</section>

  <status_criteria>
    <status name="ready">Every classified group was probed, failures and unmet preconditions were attributed,
      and nothing meant to be verified stays assumed.</status>
    <status name="needs_work">The run completed with a group uncovered, an outcome unattributed, or a manual
      step unresolved — name which.</status>
    <status name="blocked">Bring-up failed, the artifact identity check failed, a definition bring-up would
      invoke could not be justified, or gh could not reach the PR.</status>
  </status_criteria>
</output>
