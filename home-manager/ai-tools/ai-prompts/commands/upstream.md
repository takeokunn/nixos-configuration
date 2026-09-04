---
argument-hint: [upstream-url]
description: Upstream PR preparation and review command
---

<purpose>
Review changes before they are submitted to an upstream OSS repository: fetch the contribution guidelines,
learn the repository's actual conventions from merged PRs, assess the diff, and emit PR metadata plus a task
breakdown that /execute can consume. Everything produced is a handoff the user decides to act on.
</purpose>

<rules priority="critical">
  <rule>Read-only. Never modify the repository under review. The verification-environment files under /tmp are
    the only writes permitted.</rule>
  <rule>NEVER create a pull request, by `gh pr create` or any other means, even when the user asks for one in
    this command. Refuse, emit the task breakdown, and say that /execute runs the tasks and the user opens the
    PR. SSOT-EXEMPT: restated as a hard block because the action is externally visible and irreversible.</rule>
</rules>
<rules priority="important">
  <rule>Verify `gh auth status` before any PR-history operation. An unauthenticated shell returns an empty PR
    sample rather than an error, and the review then invents conventions from nothing.</rule>
  <rule>Dispatch the gather-phase agents in one message; they are independent.</rule>
  <rule>Every command in an emitted QA step carries real values from the diff. A step still holding
    `[endpoint-path]`, `[component-name]`, or `[table-name]` cannot be run by the reviewer it was written for;
    list what could not be resolved under gaps instead of leaving it in the command.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <objective>Establish the upstream, the diff, and that gh can talk to it</objective>
    <step order="1">
      <action>Load git-ecosystem only if commit-history work will be planned — a rebase onto the upstream
        default branch, a squash, or a re-push. Skip it for a review that produces PR metadata alone.</action>
      <tool>Skill (git-ecosystem)</tool>
    </step>
    <step order="2">
      <action>Run `gh auth status`, resolve owner/repo from `git remote -v` (prefer an `upstream` remote), and
        take the diff against the upstream default branch. If more than one remote could be the upstream, or
        the detected URL contradicts the command argument, ask rather than picking.</action>
      <tool>Bash, AskUserQuestion</tool>
      <output>Account line, resolved owner/repo and its remote, diff --stat</output>
    </step>
    <step order="3">
      <action>Read Serena memories for this upstream's contribution patterns only if the index names
        one — a repository reviewed for the first time has nothing to load.</action>
      <tool>Serena list_memories, read_memory</tool>
      <output>Matched memories, or "no entry for this upstream"</output>
    </step>
  </phase>
  <reflection_checkpoint id="preflight_complete" after="prepare">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The account line from `gh auth status`, the resolved owner/repo with its remote, and the file and
      line counts from the diff. Zero files means there is nothing to review.</check>
    <check>Judge the diff's scope in both directions. Over-inclusion: files the change does not need —
      incidental tooling, unrelated docs, CI edits picked up along the way — which a maintainer must untangle
      before reviewing anything. Under-inclusion: surfaces the change requires but does not touch, most often a
      dependency manifest or lockfile left behind when only the source moved. Both come from the same missing
      step, so enumerate every surface this change must touch to hold together and compare it against the
      diff.</check>
    <on_unmet>Stop and report. Never proceed on an assumed remote or an empty diff. When the scope is mixed,
      report the split and let the user decide what belongs in this PR.</on_unmet>
  </reflection_checkpoint>

  <phase name="gather">
    <objective>Collect the evidence, in parallel</objective>
    <step order="1">
      <action>Dispatch guidelines, pr_template, changes, tests, and pr_samples in one message.</action>
      <tool>Agent</tool>
      <output>Five reports</output>
    </step>
  </phase>
  <reflection_checkpoint id="gather_complete" after="gather">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The URL CONTRIBUTING.md came from, or all three locations tried with the status each returned.</check>
    <check>Whether .github/PULL_REQUEST_TEMPLATE.md was fetched or confirmed absent, and by which URL.</check>
    <check>How many merged PRs came back, with their numbers. Fewer than ten is a gap to record, not a detail
      to round up.</check>
    <check>The files the changes agent reviewed and the files the tests agent reviewed.</check>
    <on_unmet>Record each unmet item under gaps and proceed on what was actually retrieved. Never present a
      convention inferred from no sample as a learned upstream pattern.</on_unmet>
  </reflection_checkpoint>

  <phase name="synthesize">
    <objective>Turn the evidence into a handoff</objective>
    <step order="1">
      <action>Generate the PR title and description from the upstream template where one exists, otherwise from
        the structure the sampled PRs share. Record which of the two it was, and name the template URL or the
        PR numbers it was derived from.</action>
      <output>PR metadata with its basis named</output>
    </step>
    <step order="2">
      <action>Derive local reproduction from the repository's own definitions — a flake output, Makefile
        target, or package script — Nix first where a flake exists. A command with no such definition is a
        guess and is labelled one. Name the indicator file the ecosystem was identified from, and take service
        dependencies from compose files, .env.example, or the CI service block rather than from habit.</action>
      <output>Setup, services, and verification commands, each with the file that defines it</output>
    </step>
    <step order="3">
      <action>Classify what the diff touches — UI, API, database, config, security, integration — from the
        paths and contents themselves, and write QA steps whose commands carry the real paths, endpoints, and
        component names. Where a verification environment helps, build it under
        /tmp/&lt;repo&gt;/&lt;branch-or-issue&gt;/ with a devenv.nix, a .envrc using devenv, fixtures for the change
        types found, and a README stating expected results concretely enough to compare against; load
        devenv-ecosystem at that point for the option surface. Name any tool the steps invoke that devenv.nix
        does not provide — the environment fails at that command, not at build time.</action>
      <output>QA steps with injected values; the verification environment path, or why none was needed</output>
    </step>
    <step order="4">
      <action>Break the work into phased tasks for /execute: code fixes (CF-nnn), test updates (TU-nnn),
        documentation (DOC-nnn), commit preparation (GIT-nnn), final verification (VER-nnn). Give each task its
        files, its deliverable, its verification criterion, and its dependencies, and mark which phases are
        parallel-safe. Commit-preparation tasks encode the principles in git_mechanics below; this command
        plans them and never runs them.</action>
      <output>Phased tasks with dependencies, and the decisions and references /execute needs</output>
    </step>
  </phase>

  <phase name="self_evaluate">
    <objective>Find what the review claims but did not establish</objective>
    <step order="1">
      <action>Cross-check the guideline-compliance items against the code-review findings yourself; they are
        two readings of the same diff, and a compliance item marked pass on a file the changes agent flagged is
        the contradiction worth catching. Dispatch validator only when that leaves a consequential disagreement
        you cannot settle from what each side examined.</action>
      <output>Contradictions and how each was settled, or "none"</output>
    </step>
    <step order="2">
      <action>Tag every checklist item per the evidence rules in CLAUDE.md. A verified item names the guideline
        line, the file:line, or the command behind it; an item marked pass because it looked fine is assumed.
        Then set the status from status_criteria and name the weakest claim with what would confirm it.</action>
      <output>Tagged findings, status, weakest claim</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Per gate_discipline in CLAUDE.md.</gate>
  <check>Name every tool call made and show that none wrote to the repository under review and none created a
    PR. Writes under /tmp are the only exception.</check>
  <check>Name the output sections produced, and any that is missing.</check>
  <check>Quote any placeholder still present in a QA step command.</check>
  <on_unmet>Resolve the gap before returning the report.</on_unmet>
</reflection_checkpoint>

<agents>
  <agent name="guidelines" subagent_type="docs" readonly="true">Fetch CONTRIBUTING.md, trying root, then .github/, then docs/; extract the stated requirements</agent>
  <agent name="pr_template" subagent_type="docs" readonly="true">Fetch .github/PULL_REQUEST_TEMPLATE.md at that exact path only, no fallback; return its required sections, or absent</agent>
  <agent name="changes" subagent_type="quality-assurance" readonly="true">Review the diff for quality and for departure from patterns prevailing in the upstream repository</agent>
  <agent name="tests" subagent_type="test" readonly="true">Evaluate test coverage and appropriateness for the change</agent>
  <agent name="pr_samples" subagent_type="general-purpose" readonly="true">`gh pr list --repo {owner}/{repo} --state merged --limit 10 --json title,body,number,author`; extract title patterns, description structure, and how each change was split into commits</agent>
  <agent name="metadata" subagent_type="docs" readonly="true">Compose the PR title and description from the template where present, otherwise from the sampled patterns, recording which</agent>
  <agent name="verify" subagent_type="devops" readonly="true">Derive the reproduction steps and the verification environment, and inject diff values into the QA steps</agent>
  <agent name="validator" subagent_type="validator" readonly="true" dispatch="on_demand">Re-derive one disputed claim, only when the cross-check could not settle it</agent>
</agents>
<execution_graph>
  <parallel_group id="gather" depends_on="none">guidelines, pr_template, changes, tests, pr_samples</parallel_group>
  <parallel_group id="post_gather" depends_on="gather">metadata, verify</parallel_group>
  <sequential_phase id="self_evaluation" depends_on="post_gather">
    <action>Cross-check compliance against review findings, tag evidence tiers, list gaps</action>
    <conditional_agent>validator</conditional_agent>
    <reason>An independent pass costs materially more than the reports it checks, so it is dispatched for an unsettled disagreement rather than as a routine phase</reason>
  </sequential_phase>
</execution_graph>

<decision_criteria>
  <factor name="guideline_compliance" precedence="1">
    <unmet>A stated CONTRIBUTING.md requirement is not met, or CONTRIBUTING.md could not be fetched. Report the
      requirement and the violating file; if the guidelines are missing, say the compliance section rests on
      sampled PRs rather than on stated rules.</unmet>
  </factor>
  <factor name="test_coverage" precedence="2">
    <unmet>Behavior changed and no test exercises the new path, or the test command was never run. Name the
      untested behavior and the command a reviewer should run.</unmet>
  </factor>
  <factor name="code_quality" precedence="3">
    <unmet>The change departs from a pattern used elsewhere upstream and both locations can be cited. Report
      both file:line references.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<git_mechanics>
  The principles the commit_prep tasks encode. This command plans them and never runs them. Referenced by
  git-ecosystem, which deliberately does not restate them.

  <principle name="branch_naming">Name the branch after the issue it addresses, cut from the upstream default
    branch — fix/&lt;issue-number&gt;-&lt;slug&gt; for a bug, feat/&lt;slug&gt; for a feature.</principle>
  <principle name="rebase_onto_upstream">Rebase onto the freshly fetched upstream default branch so the PR
    applies cleanly and carries only the intended changes, not merge-commit noise.</principle>
  <principle name="commit_split_from_precedent">Derive the number of commits from how the closest analogous
    change landed, using the ten sampled merged PRs as the evidence, rather than from habit. Where a repository
    consistently lands a change of this shape as an ordered series — interface first, then implementation and
    migration, then the public surface together with the rules guarding it — one squashed commit is harder to
    review, not easier. Every commit in such a series stands on its own, and a security-relevant surface is
    never introduced before the rule that guards it.</principle>
  <principle name="single_reviewable_commit">Absent such a precedent, organize the change into one reviewable,
    logically complete commit and squash incidental fixups. Reviewers read a coherent diff, not the authoring
    history.</principle>
  <principle name="scope_is_exact">Plan the commit to contain every surface the change needs and nothing else.
    Unrelated tooling or documentation swept in makes a reviewer untangle the diff before reading it; a source
    change whose dependency manifest or lockfile was left behind does not build for anyone but its author.</principle>
  <principle name="issue_reference">Reference the issue with a closing keyword — Fixes #N / Closes #N — so the
    merge auto-closes it.</principle>
  <principle name="force_with_lease">A rebased branch is re-pushed with --force-with-lease, never --force.
    --force-with-lease updates the remote only if its tip still matches your remote-tracking ref, so it refuses
    to clobber commits pushed since your last fetch; plain --force overwrites them silently. A background
    `git fetch` can invalidate the lease, and --force-if-includes closes that gap by requiring the fetched
    updates to be integrated locally first.</principle>
  <principle name="compat_and_tests_as_a_set">Design a backward-compatibility fallback together with its test
    coverage. Gate new behavior behind an opt-in — a new enum variant, mode flag, or config key — that
    preserves the old default, and pair it with tests exercising both paths. Compatibility without a test
    pinning the old behavior is unverified.</principle>
</git_mechanics>

<output>
  Follows output_contract in CLAUDE.md, with these sections:

  <section name="summary">Upstream owner/repo, branch, what the change does, status.</section>
  <section name="checklist">Findings grouped as contribution guidelines, code quality, test coverage, and
    recurring patterns from past reviews. Each item carries pass/fail/warn and its location.</section>
  <section name="pr_metadata">Title, description in markdown matching upstream conventions, and the basis it
    was derived from — the template URL, the sampled PR numbers, or neither, in which case say the structure is
    a general convention and was not learned from this repository.</section>
  <section name="local_reproduction">Ecosystem and the indicator file it was identified from, setup, service
    dependencies with their source, and verification commands each with the file that defines it. A command
    with no definition is labelled a guess.</section>
  <section name="manual_verification">The QA steps with injected values, the verification-environment path if
    one was built, and every unresolved placeholder or missing tool.</section>
  <section name="task_breakdown">The phased tasks with dependencies, and the decisions, references, and
    constraints /execute needs to run them without another planning pass.</section>

  <status_criteria>
    <status name="ready">Every check the review set out to make was made and none failed; nothing it was meant
      to verify is left at assumed.</status>
    <status name="needs_work">The review completed, but a check could not run, an item rests on assumed
      evidence, or a warning-level finding stands. The gap is named.</status>
    <status name="blocked">A critical finding stands, or a blocker stopped the central question from being
      answered — gh auth failure, no upstream detected, or an empty diff.</status>
  </status_criteria>
</output>
