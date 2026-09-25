---
argument-hint: [task-description]
description: Task execution command
---

Implement the requested task and verify the result. Use /execute-full when the user wants the full review wave.
Apply CLAUDE.md's hard_rules, work_selection, delegation, evidence, and output_contract throughout.

## Prepare

Load execution-workflow. Confirm the project root, current ref, and existing changes before editing.
Use Serena when available; activate the project and read only task-matching memories, including verification
commands. If unavailable, use repository evidence and report the gap, not a guessed memory result.
Record HEAD and the initial dirty paths/diff so later scans distinguish this task's additions from existing work.

Name the requested outcome, affected paths, and acceptance checks. For a local, understood change, this is
enough planning. For work spanning interfaces or uncertain behavior, identify atomic units, dependencies, and
owners before implementation. Missing authority or materially different interpretations require a question.

## Implement

1. Read the edit targets and nearest relevant pattern.
   Before adding a family member, trace its sibling through registration/discovery and name required touch
   points. Before removing or migrating a definition, search the identifier itself, including declarations,
   comments, and test doubles. Use references to establish caller impact for interface changes.
2. Implement directly or delegate substantial independent units under the delegation contract. Do not require
   an agent for each file. Each writer follows output_discipline and returns evidence, not self-approval.
3. Add or update tests for changed functionality and documentation for changed public behavior. Do not create
   unrelated tests or docs to fill a role. Never substitute an always-passing test for behavior unavailable
   here; inspect existing helpers for a usable fixture or adapter first.

## Verify and review

1. Name the applicable test, lint, build, type, or artifact-validation commands from the project. Establish
   their input scope, configuration, generated outputs, and fixture/service requirements. Ensure new and
   untracked work is included; keep scratch outputs isolated from other sessions.
2. Run the checks and inspect assertions as well as exit status. Use diagnostics when available, otherwise
   repository commands. For unavailable behavior, search relevant scripts/configuration for a fixture,
   in-memory adapter, or substitute backend; report the search and the remaining gap.
3. For failing tests, allow one targeted fix attempt across the task, then rerun. Remaining failures are
   blockers, not permission to weaken the gate. Preserve fail-closed behavior when following a reference
   implementation.
4. Review the settled change for correctness, caller impact, and missing acceptance coverage. Select extra
   review by risk: security for trust boundaries, design for new module dependencies, performance for changed
   hot paths, and a verification agent for consequential claims not established by the suite. Independent
   reviews may run together, read-only. Do not assign multiple reviewers the same check.
   Give claim-verification agents the claim and supporting commands, not merely a diff. If agents are
   unavailable, perform the checks here and disclose that they were not independent.
5. Run the wording/code-artifact pass below, then verify any resulting changes. Completion requires passing
   checks over the requested behavior. Static validation of prose/configuration does not establish runtime
   behavior, and a missing check remains a reported gap.

## Wording and code-artifact pass

Load ai-slop-detector. Scan this task's added lines using its lexical patterns and Japanese token source;
include new files and exclude pre-existing edits using the baseline recorded in Prepare. Review those lines
for its judgment tells as well, directly or using the reviewer already assigned. Writers return zero lexical
hits or each retained hit with its contextual reason.

Apply one removal pass for confirmed findings, directly or through the existing owner, then rescan and rerun
affected verification. This wording-only pass is outside the failing-test fix budget; it does not authorize
behavioral refactoring or a second test-fix cycle. If both lexical and judgment findings are empty, skip removal.
Record the scan command, extracted-line count, hits before/after, and retained judgment findings with reasons.

## Finish

Apply memory_policy only when durable learning or unresolved findings warrant a write. Refresh only memories
actually checked against the result; do not launch a memory agent or a freshness audit by default.

Return output_contract with changed paths, verification scope (including new files), review concerns checked,
the wording pass results, and any uncovered acceptance criteria. State the weakest consequential claim and
what would confirm it when one remains.
