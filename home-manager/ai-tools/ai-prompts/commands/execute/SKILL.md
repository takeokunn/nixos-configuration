---
argument-hint: [task-description]
description: Implement a task and verify it. Use for a bounded change that needs no full review wave.
---

Implement the requested task and verify the result. Use /execute-full when the user wants the full review wave.
Apply CLAUDE.md's hard_rules, work_selection, delegation, evidence, and output_contract throughout.

## Prepare

Load execution-workflow once. Use Serena when available and read memories only when prior knowledge could
change the work (for example a canonical verification command); if unavailable, use repository evidence.
Run `jj status` before editing only if you need to tell your changes from existing ones at completion.

Name the requested outcome, affected paths, and acceptance checks. For a local, understood change, this is
enough planning. For work spanning interfaces or uncertain behavior, identify atomic units, dependencies, and
owners before implementation. State the assumption when interpretations differ; ask only under CLAUDE.md's work_selection rule.

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
5. Completion requires passing
   checks over the requested behavior. Static validation of prose/configuration does not establish runtime
   behavior, and a missing check remains a reported gap.

## Finish

Apply memory_policy only when durable learning or unresolved findings warrant a write. Refresh only memories
actually checked against the result; do not launch a memory agent or a freshness audit by default.

Return output_contract with changed paths, verification scope (including new files), review concerns checked,
and any uncovered acceptance criteria. State the weakest consequential claim and
what would confirm it when one remains.
