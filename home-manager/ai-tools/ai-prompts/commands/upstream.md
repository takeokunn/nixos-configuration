---
argument-hint: [upstream-url]
description: Upstream PR preparation and review command
---

<purpose>
Review changes before an upstream OSS PR: fetch contribution guidelines, learn actual conventions from merged
  PRs, assess the diff, and emit PR metadata plus a task breakdown for /execute — a handoff the user decides to
  act on.
</purpose>

<rules priority="critical">
  <rule>Read-only: never modify the repository under review, except writes under /tmp for the verification
    environment.</rule>
  <rule>NEVER create a pull request, by `gh pr create` or any other means, even when the user asks for one in
    this command. Refuse, emit the task breakdown, and say that /execute runs the tasks and the user opens the
    PR. SSOT-EXEMPT: restated as a hard block because the action is externally visible and irreversible.</rule>
</rules>
<rules priority="important">
  <rule>Verify `gh auth status` first: an unauthenticated shell returns an empty PR sample, not an error, so the
    review invents conventions from nothing.</rule>
  <rule>Dispatch the gather-phase agents in one message; they are independent.</rule>
  <rule>Every QA-step command must carry real values from the diff — a step still holding `[endpoint-path]`,
    `[component-name]`, or `[table-name]` cannot be run. List unresolved values under gaps instead.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <objective>Establish the upstream, the diff, and that gh can talk to it</objective>
    <step order="1">
      <action>Run `gh auth status`, resolve owner/repo from `git remote -v` (prefer `upstream`), and diff
        against the upstream default branch — ask, don't pick, if multiple remotes could be upstream or the URL
        contradicts the argument.</action>
      <tool>Bash, AskUserQuestion</tool>
      <output>Account line, resolved owner/repo and its remote, diff --stat</output>
    </step>
    <step order="2">
      <action>Read Serena memories for this upstream's patterns only if the index names one — nothing to load on
        a first review.</action>
      <tool>Serena list_memories, read_memory</tool>
      <output>Matched memories, or "no entry for this upstream"</output>
    </step>
  </phase>
  <reflection_checkpoint id="preflight_complete" after="prepare">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The `gh auth status` account line, resolved owner/repo with its remote, and the diff's file and line
      counts — zero files means nothing to review.</check>
    <check>Judge diff scope both ways — over-inclusion (incidental tooling, unrelated docs, CI edits) and
      under-inclusion (a dependency manifest or lockfile left behind when only the source moved) — both from one
      missing step: enumerate every surface the change must touch and compare against the diff.</check>
    <on_unmet>Stop and report — never proceed on an assumed remote or empty diff. If scope is mixed, report the
      split and let the user decide.</on_unmet>
  </reflection_checkpoint>

  <phase name="gather">
    <objective>Collect the evidence, in parallel</objective>
    <action>Dispatch guidelines, pr_template, changes, tests, and pr_samples in one message.</action>
    <tool>Agent</tool>
    <output>Five reports</output>
  </phase>
  <reflection_checkpoint id="gather_complete" after="gather">
    <check>The URL CONTRIBUTING.md came from, or all three locations tried with the status each
      returned.</check>
    <check>Whether .github/PULL_REQUEST_TEMPLATE.md was fetched or confirmed absent, and by which URL.</check>
    <check>How many merged PRs came back, with numbers — fewer than ten is a gap, not a rounding detail.</check>
    <check>The files the changes agent reviewed and the files the tests agent reviewed.</check>
    <on_unmet>Record unmet items under gaps and proceed on what was retrieved — never present a convention
      inferred from no sample as learned.</on_unmet>
  </reflection_checkpoint>

  <phase name="synthesize">
    <objective>Turn the evidence into a handoff</objective>
    <step order="1">
      <action>Generate the PR title/description from the upstream template where one exists, else from the
        sampled PRs' shared structure — record which, and name the template URL or PR numbers used.</action>
      <output>PR metadata with its basis named</output>
    </step>
    <step order="2">
      <action>Derive local reproduction from the repo's own definitions — flake output, Makefile target, or
        package script, Nix first; no definition is a labelled guess. Name the indicator file, and take service
        deps from compose files, .env.example, or the CI service block, never habit.</action>
      <output>Setup, services, and verification commands, each with the file that defines it</output>
    </step>
    <step order="3">
      <action>Classify what the diff touches — UI, API, database, config, security, integration — from paths and
        contents, and write QA steps with real paths, endpoints, and component names. Where useful, build a
        verification environment under /tmp/&lt;repo&gt;/&lt;branch-or-issue&gt;/ with devenv.nix, a devenv
        .envrc, fixtures per change type, and a README stating expected results concretely. Name any tool a step
        invokes that devenv.nix lacks — it fails there, not at build.</action>
      <output>QA steps with injected values; the verification environment path, or why none was needed</output>
    </step>
    <step order="4">
      <action>Break work into phased /execute tasks — code fixes (CF-nnn), test updates (TU-nnn), docs
        (DOC-nnn), commit prep (GIT-nnn), final verification (VER-nnn) — each with files, deliverable,
        verification criterion, dependencies, and parallel-safety marked. Commit-prep tasks encode git_mechanics
        below; this command only plans them.</action>
      <output>Phased tasks with dependencies, and the decisions and references /execute needs</output>
    </step>
  </phase>

  <phase name="self_evaluate">
    <objective>Find what the review claims but did not establish</objective>
    <step order="1">
      <action>Cross-check guideline-compliance items against code-review findings yourself — both read the same
        diff, and a pass on a file the changes agent flagged is the contradiction worth catching. Dispatch
        validator only for disagreement neither side's evidence settles.</action>
      <output>Contradictions and how each was settled, or "none"</output>
    </step>
    <step order="2">
      <action>Tag every checklist item per CLAUDE.md's evidence rules — verified names the guideline line,
        file:line, or command behind it; pass-because-it-looked-fine is assumed. Set status from
        status_criteria, and name the weakest claim.</action>
      <output>Tagged findings, status, weakest claim</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <check>Name every tool call, and show none wrote to the reviewed repository or created a PR — /tmp writes are
    the only exception.</check>
  <check>Name the output sections produced, and any that is missing.</check>
  <check>Quote any placeholder still present in a QA step command.</check>
  <on_unmet>Resolve the gap before returning the report.</on_unmet>
</reflection_checkpoint>

<agents>
  <agent name="guidelines" subagent_type="docs" readonly="true">Fetch CONTRIBUTING.md — root, .github/, docs/ —
    and extract stated requirements</agent>
  <agent name="pr_template" subagent_type="docs" readonly="true">Fetch .github/PULL_REQUEST_TEMPLATE.md at that
    exact path only, no fallback; return its required sections, or absent</agent>
  <agent name="changes" subagent_type="quality-assurance" readonly="true">Review the diff for quality and
    departure from upstream's prevailing patterns</agent>
  <agent name="tests" subagent_type="test" readonly="true">Evaluate test coverage and appropriateness for the
    change</agent>
  <agent name="pr_samples" subagent_type="general-purpose" readonly="true">`gh pr list --repo {owner}/{repo}
    --state merged --limit 10 --json title,body,number,author`; extract title patterns, description structure,
    and commit splits</agent>
  <agent name="metadata" subagent_type="docs" readonly="true">Compose the PR title/description from the template
    where present, else sampled patterns — record which</agent>
  <agent name="verify" subagent_type="devops" readonly="true">Derive reproduction steps and verification
    environment; inject diff values into the QA steps</agent>
  <agent name="validator" subagent_type="validator" readonly="true" dispatch="on_demand">Re-derive one disputed
    claim, only when the cross-check can't settle it</agent>
</agents>
<execution_graph>
  <parallel_group id="gather" depends_on="none">guidelines, pr_template, changes, tests,
    pr_samples</parallel_group>
  <parallel_group id="post_gather" depends_on="gather">metadata, verify</parallel_group>
  <sequential_phase id="self_evaluation" depends_on="post_gather">
    <action>Cross-check compliance against review findings, tag evidence tiers, list gaps</action>
    <conditional_agent>validator</conditional_agent>
    <reason>An independent pass costs more than the reports it checks, so it runs only for unsettled
      disagreement, never routinely</reason>
  </sequential_phase>
</execution_graph>

<decision_criteria>
  <factor name="guideline_compliance" precedence="1">
    <unmet>A stated CONTRIBUTING.md requirement is unmet, or the file couldn't be fetched. Report the
      requirement and violating file; if guidelines are missing, say compliance rests on sampled PRs, not stated
      rules.</unmet>
  </factor>
  <factor name="test_coverage" precedence="2">
    <unmet>Behavior changed with no test on the new path, or the test command never ran. Name the untested
      behavior and the command to run.</unmet>
  </factor>
  <factor name="code_quality" precedence="3">
    <unmet>The change departs from an upstream pattern citable at both locations — report both file:line
      references.</unmet>
  </factor>
</decision_criteria>

<git_mechanics>
  Principles commit_prep tasks encode; planned here, never run.

  <principle name="branch_naming">Name the branch after the issue, cut from the upstream default branch —
    fix/&lt;issue-number&gt;-&lt;slug&gt; for a bug, feat/&lt;slug&gt; for a feature.</principle>
  <principle name="rebase_onto_upstream">Rebase onto the freshly fetched upstream default branch so the PR
    applies cleanly with only intended changes, no merge-commit noise.</principle>
  <principle name="commit_split_from_precedent">Derive commit count from how the closest analogous change
    landed, using the ten sampled merged PRs as evidence, not habit. Where a repo consistently lands this shape
    as an ordered series — interface first, implementation/migration, then the public surface with guarding
    rules — one squashed commit is harder to review: every commit stands alone, and a security-relevant surface
    never precedes its guarding rule.</principle>
  <principle name="single_reviewable_commit">Absent precedent, use one reviewable, logically complete commit and
    squash incidental fixups — reviewers read a coherent diff, not the authoring history.</principle>
  <principle name="scope_is_exact">Plan the commit to hold every surface the change needs and nothing else:
    unrelated tooling or docs force a reviewer to untangle the diff first, and a source change missing its
    dependency manifest or lockfile doesn't build for anyone but its author.</principle>
  <principle name="issue_reference">Reference the issue with a closing keyword — Fixes #N / Closes #N — so the
    merge auto-closes it.</principle>
  <principle name="force_with_lease">Re-push a rebased branch with --force-with-lease, never --force: it updates
    the remote only if its tip matches your remote-tracking ref, refusing to clobber commits pushed since your
    last fetch — plain --force overwrites them silently. A background `git fetch` can invalidate the lease;
    --force-if-includes closes that gap by requiring fetched updates be integrated locally first.</principle>
  <principle name="compat_and_tests_as_a_set">Pair a backward-compatibility fallback with its test coverage:
    gate new behavior behind an opt-in — a new enum variant, mode flag, or config key — preserving the old
    default, and test both paths. Compatibility without a test pinning the old behavior is
    unverified.</principle>
</git_mechanics>

<output>
  Follows output_contract in CLAUDE.md, with these sections:

  <section name="summary">Upstream owner/repo, branch, what the change does, status.</section>
  <section name="checklist">Findings grouped as guidelines, quality, coverage, and recurring patterns from past
    reviews — each with pass/fail/warn and location.</section>
  <section name="pr_metadata">Title, markdown description matching upstream conventions, and its basis —
    template URL, sampled PR numbers, or neither, noting the structure is a general convention if so.</section>
  <section name="local_reproduction">Ecosystem and its indicator file, setup, service deps with source, and
    verification commands each with the defining file — undefined commands are labelled guesses.</section>
  <section name="manual_verification">The QA steps with injected values, the verification-environment path if
    one was built, and every unresolved placeholder or missing tool.</section>
  <section name="task_breakdown">The phased tasks with dependencies, and the decisions, references, and
    constraints /execute needs to run them without another planning pass.</section>

  <status_criteria>
    <status name="ready">Every check the review set out to make ran and passed; nothing meant to verify stays
      assumed.</status>
    <status name="needs_work">The review completed, but a check couldn't run, an item rests on assumed evidence,
      or a warning stands — name the gap.</status>
    <status name="blocked">A critical finding stands, or a blocker stopped the central question from being
      answered — gh auth failure, no upstream detected, or an empty diff.</status>
  </status_criteria>
</output>
