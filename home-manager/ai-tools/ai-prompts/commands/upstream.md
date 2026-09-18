---
argument-hint: [upstream-url]
description: Upstream PR preparation and review command
---

Review changes before an upstream OSS PR: fetch contribution guidelines, learn actual conventions from merged
PRs, assess the diff, and emit PR metadata plus a task breakdown for /execute: a handoff the user decides to act
on.

## Rules

Critical:

- Never modify reviewed source. Optional verification artifacts must stay in a dedicated scratch directory
  inside the active project root; report their paths. Do not write to adjacent checkouts or external temporary paths.
- NEVER create a pull request, by `gh pr create` or any other means, even when the user asks for one in this
  command. Refuse, emit the task breakdown, and say that /execute runs the tasks and the user opens the PR.
  SSOT-EXEMPT: restated as this command's explicit scope boundary.

Important:

- Verify `gh auth status` first and check retrieval exit statuses; an empty or failed PR sample establishes no
  upstream convention.
- Dispatch the gather-phase agents in one message; they are independent.
- Every QA-step command must carry real values from the diff: a step still holding `[endpoint-path]`,
  `[component-name]`, or `[table-name]` cannot be run. List unresolved values under gaps instead.

## Workflow

### Prepare

Establish the upstream, the diff, and that gh can talk to it.

1. Run `gh auth status`, resolve owner/repo from `git remote -v` (prefer `upstream`), and diff against the
   upstream default branch: ask, don't pick, if multiple remotes could be upstream or the URL contradicts the
   argument. Use Bash and AskUserQuestion. Return the account line, resolved owner/repo and its remote, diff
   --stat. Record the local comparison ref and SHA and whether its freshness was verified. Fetching requires
   current-request git-write authority; if absent, use available refs with a freshness gap or ask when blocking.
2. Read Serena memories for this upstream's patterns only if the index names one: nothing to load on a first
   review. Use Serena list_memories and read_memory. Return the matched memories, or "no entry for this
   upstream".

### Checkpoint on preflight complete, after prepare

Per gate_discipline in CLAUDE.md. Name:

- The `gh auth status` account line, resolved owner/repo with its remote, and the diff's file and line counts:
  zero files means nothing to review.
- Judge diff scope both ways: over-inclusion (incidental tooling, unrelated docs, CI edits) and under-inclusion
  (a dependency manifest or lockfile left behind when only the source moved), both from one missing step:
  enumerate every surface the change must touch and compare against the diff.

Unmet: stop and report: never proceed on an assumed remote or empty diff. If scope is mixed, report the split and
let the user decide.

### Gather

Collect the evidence, in parallel. Dispatch guidelines, pr_template, changes, tests, and pr_samples in one
message. Use Agent. Return five reports.

### Checkpoint on gather complete, after gather

- The URL CONTRIBUTING.md came from, or all three locations tried with the status each returned.
- Whether .github/PULL_REQUEST_TEMPLATE.md was fetched or confirmed absent, and by which URL.
- How many merged PRs came back, with numbers: fewer than ten is a gap, not a rounding detail.
- The files the changes agent reviewed and the files the tests agent reviewed.

Unmet: record unmet items under gaps and proceed on what was retrieved: never present a convention inferred from
no sample as learned.

### Synthesize

Turn the evidence into a handoff.

1. Load pull-request, then generate the PR title/description from the upstream template where one exists, else from the sampled PRs'
   shared structure: record which, and name the template URL or PR numbers used. Return PR metadata with its
   basis named.
2. Derive local reproduction from the repo's own definitions: flake output, Makefile target, or package script,
   Nix first; no definition is a labelled guess. Name the indicator file, and take service deps from compose
   files, .env.example, or the CI service block, never habit. Return setup, services, and verification commands,
   each with the file that defines it.
3. Classify what the diff touches (UI, API, database, config, security, integration) from paths and contents, and
   write QA steps with real paths, endpoints, and component names. Where useful, build a verification environment
   in a unique scratch directory inside the active project root. Create only files needed for the identified
   checks, using the repository's existing environment conventions. State expected results and missing tools.
   Return QA steps with injected values; the verification environment path, or why
   none was needed.
4. Break work into phased /execute tasks: code fixes (CF-nnn), test updates (TU-nnn), docs (DOC-nnn), commit prep
   (GIT-nnn), final verification (VER-nnn), each with files, deliverable, verification criterion, dependencies,
   and parallel-safety marked. Commit-prep tasks encode the git mechanics below; this command only plans them.
   The plan grants no authority to perform git writes in a later request. Return phased tasks with dependencies,
   and the decisions and references /execute needs.

### Self evaluate

Find what the review claims but did not establish.

1. Cross-check guideline-compliance items against code-review findings yourself: both read the same diff, and a
   pass on a file the changes agent flagged is the contradiction worth catching. Dispatch validator only for
   disagreement neither side's evidence settles. Return the contradictions and how each was settled, or "none".
2. Tag every checklist item per CLAUDE.md's evidence rules: verified names the guideline line, file:line, or
   command behind it; pass-because-it-looked-fine is assumed. Set status from the status criteria, and name the
   weakest claim. Return tagged findings, status, weakest claim.

### Checkpoint on group consistency

- Name every write and external operation; establish that reviewed source was unchanged, scratch files stayed
  within the active project root, and no PR was created.
- Name the output sections produced, and any that is missing.
- Quote any placeholder still present in a QA step command.

Unmet: resolve the gap where authorized; otherwise report it without claiming the checkpoint passed.

## Agents

| Agent | Subagent type | Dispatch | Scope |
|---|---|---|---|
| guidelines | docs | always, read-only | Fetch CONTRIBUTING.md (root, .github/, docs/) and extract stated requirements |
| pr_template | docs | always, read-only | Fetch .github/PULL_REQUEST_TEMPLATE.md at that exact path only, no fallback; return its required sections, or absent |
| changes | quality-assurance | always, read-only | Review the diff for quality and departure from upstream's prevailing patterns |
| tests | test | always, read-only | Evaluate test coverage and appropriateness for the change |
| pr_samples | general-purpose | always, read-only | `gh pr list --repo {owner}/{repo} --state merged --limit 10 --json title,body,number,author`; extract title/body patterns. For commit-count or split claims, read each cited PR with `gh pr view {number} --repo {owner}/{repo} --json commits`. Inspect each returned commit's message and changed paths with `git show --stat {sha}` when available locally; report missing commit evidence as a gap rather than fetching without authorization |
| metadata | docs | always, read-only | Compose the PR title/description from the template where present, else sampled patterns: record which |
| verify | devops | always, read-only | Derive reproduction steps and verification environment; inject diff values into the QA steps |
| validator | validator | on_demand, read-only | Re-derive one disputed claim, only when the cross-check can't settle it |

## Execution graph

- **gather** (parallel, depends on nothing): guidelines, pr_template, changes, tests, pr_samples.
- **post_gather** (parallel, depends on gather): metadata, verify.
- **self_evaluation** (sequential, depends on post_gather): cross-check compliance against review findings, tag
  evidence tiers, list gaps. Conditional agent: validator. An independent pass costs more than the reports it
  checks, so it runs only for unsettled disagreement, never routinely.

## Decision criteria

1. **Guideline compliance.** A stated CONTRIBUTING.md requirement is unmet, or the file couldn't be fetched.
   Report the requirement and violating file; if guidelines are missing, say compliance rests on sampled PRs, not
   stated rules.
2. **Test coverage.** Behavior changed with no test on the new path, or the test command never ran. Name the
   untested behavior and the command to run.
3. **Code quality.** The change departs from an upstream pattern citable at both locations: report both file:line
   references.

## Git mechanics

Principles commit_prep tasks encode; planned here, never run.

- **branch_naming.** Name the branch after the issue, cut from the upstream default branch:
  fix/&lt;issue-number&gt;-&lt;slug&gt; for a bug, feat/&lt;slug&gt; for a feature.
- **rebase_onto_upstream.** Rebase onto the freshly fetched upstream default branch so the PR applies cleanly
  with only intended changes, no merge-commit noise.
- **commit_split_from_precedent.** Derive commit count from how the closest analogous change landed, using the
  ten sampled merged PRs as evidence, not habit. Where a repo consistently lands this shape as an ordered series
  (interface first, implementation/migration, then the public surface with guarding rules), one squashed commit is
  harder to review: every commit stands alone, and a security-relevant surface never precedes its guarding rule.
- **single_reviewable_commit.** Absent precedent, use one reviewable, logically complete commit and squash
  incidental fixups: reviewers read a coherent diff, not the authoring history.
- **scope_is_exact.** Plan the commit to hold every surface the change needs and nothing else: unrelated tooling
  or docs force a reviewer to untangle the diff first, and a source change missing its dependency manifest or
  lockfile doesn't build for anyone but its author.
- **issue_reference.** Reference the issue with a closing keyword (Fixes #N / Closes #N) so the merge auto-closes
  it.
- **force_with_lease.** If an authorized rebase requires a force-push, plan a lease with an explicitly verified
  expected remote tip. Never use plain --force or treat a background-updated tracking ref as review evidence.
- **compat_and_tests_as_a_set.** Pair a backward-compatibility fallback with its test coverage: gate new behavior
  behind an opt-in (a new enum variant, mode flag, or config key), preserving the old default, and test both
  paths. Compatibility without a test pinning the old behavior is unverified.

## Output

Follows output_contract in CLAUDE.md, with these sections:

- **summary.** Upstream owner/repo, branch, what the change does, status.
- **checklist.** Findings grouped as guidelines, quality, coverage, and recurring patterns from past reviews: each
  with pass/fail/warn and location.
- **pr_metadata.** Title, markdown description matching upstream conventions, and its basis: template URL,
  sampled PR numbers, or neither, noting the structure is a general convention if so.
- **local_reproduction.** Ecosystem and its indicator file, setup, service deps with source, and verification
  commands each with the defining file: undefined commands are labelled guesses.
- **manual_verification.** The QA steps with injected values, the verification-environment path if one was built,
  and every unresolved placeholder or missing tool.
- **task_breakdown.** The phased tasks with dependencies, and the decisions, references, and constraints /execute
  needs to run them without another planning pass.

### Status criteria

- **success.** Every check the review set out to make ran and passed; nothing meant to verify stays assumed.
- **warning.** The review completed, but a check couldn't run, an item rests on assumed evidence, or a warning
  stands: name the gap.
- **error.** A check failed, a critical finding stands, or a blocker stopped the central question from being answered: gh auth
  failure, no upstream detected, or an empty diff.
