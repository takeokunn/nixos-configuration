---
argument-hint: [upstream-url]
description: Upstream PR preparation and review command
---

Review the current change for an upstream contribution. Produce PR metadata and an /execute plan; do not edit source, run Git writes, or create/post a PR. The user opens the PR after implementation. Apply CLAUDE.md's hard_rules, evidence, delegation, consensus, and output_contract.

Optional report/probe scratch files must use unique paths inside the active project root. Report every such write; adjacent checkouts and external temporary directories are read-only.

## Establish scope

1. Run `gh auth status` before other gh calls and confirm the account. Record access failures.
2. Resolve the upstream owner/repository from the supplied upstream-url; otherwise use remotes, preferring the upstream remote. Ask when ownership remains ambiguous or the supplied URL conflicts with the intended contribution target.
3. Record the local comparison ref/SHA and its freshness limitation. Fetching requires current Git-write authorization; do not infer that the local ref is fresh.
4. Inspect a nonempty full diff, file list, and untracked files. Check both extra scope and missing dependencies, including manifests and lockfiles.
5. Read only upstream-relevant memories.

## Gather evidence in parallel

Dispatch independent read-only investigations:

| Track | Evidence |
|---|---|
| Guidelines | CONTRIBUTING at root, .github, then docs; record each searched URL and result |
| Template | Exact .github/PULL_REQUEST_TEMPLATE.md only; do not substitute another template |
| Change quality | quality-assurance review, citing diff locations and repository patterns |
| Tests | Changed behavior, coverage, fixtures, and missing acceptance checks |
| Merged precedents | `gh pr list --repo <owner/repo> --state merged --limit 10 --json title,body,number,author` |

Resolve placeholders before running commands. For commit shape, inspect representative PRs with `gh pr view ... --json commits`, then local commit messages and `git show --stat <sha>`. Missing local objects remain a gap unless fetching is authorized. Fewer than ten samples, no samples, or failed queries must be reported; none establishes an upstream convention.

Before continuing, name the guideline/template results, sampled PR numbers, reviewed files, and gaps. Proceed with labelled gaps rather than invented conventions.

## Derive the handoff

After gathering, prepare metadata/documentation and reproduction/verification instructions in parallel, read-only.

- Load pull-request for the title/body. Follow the exact upstream template when present, otherwise evidenced merged-PR patterns. If neither exists, label the structure as a general convention.
- Derive reproduction commands from actual flake outputs, Make targets, package scripts, service/compose definitions, environment examples, and CI. Prefer the repository's Nix entrypoints where present. Carry defining file:line beside every command; never guess an executable recipe.
- Classify changed UI, API, database, configuration, security, and integration surfaces. Give manual checks with real paths, endpoints, components, inputs, expected results, and tools. Unresolved values belong in gaps, not executable steps.
- Use optional scratch artifacts only when needed by an existing verification convention; keep them isolated and named.
- Cross-check guidelines, diff, metadata, tests, and manual checks. Resolve contradictions from evidence; use a validator only for a consequential unsettled claim.

## Execution plan

Produce phased /execute tasks using CF (code fixes), TU (tests), DOC, GIT, and VER where applicable. Each task names affected files, deliverable, verification command, dependencies, safe parallelism, decisions, references, and constraints. A plan carries no authorization into its later execution.

When proposing opt-in behavior, preserve the old default and include tests for both paths.

Git actions are plans only:

- Branch from the upstream default branch with an issue-linked fix/feature name.
- Rebase on fresh upstream history without unrelated merge commits.
- Derive commit boundaries from the closest sampled precedents. Without useful precedent, propose one coherent commit and squash fixups. Order dependent commits so safeguards land before an exposed public surface.
- Scope all required files explicitly and exclude unrelated work.
- Use Fixes/Closes only for the applicable issue.
- If an authorized rebase later requires force-push, use a lease with an explicit expected remote tip, never plain force or an assumption about background-updated tracking refs.

## Report

Return output_contract with:

- Guideline, quality, coverage, and precedent checklist, tagged by evidence tier.
- Proposed PR title/body and the source of their structure.
- Source-backed local reproduction and runnable manual QA.
- Phased tasks, unresolved decisions, optional scratch paths, and the weakest consequential claim.

Before returning, name every write and external operation. Confirm reviewed source is unchanged, scratch stayed inside the root, and no PR was created.
