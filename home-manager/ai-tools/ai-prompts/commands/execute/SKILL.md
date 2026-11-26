---
argument-hint: [task-description]
description: Implement a task and verify it. Use when the scope is understood.
disable-model-invocation: true
---

Implement the requested task and verify the result.

## Prepare

Load execution-workflow only when isolating the work in a jj workspace or dispatching several agents. Read
memories only when prior knowledge could change the work, for example a canonical verification command.
Run `jj status` before editing only if you need to tell your changes from existing ones at completion.

For work spanning interfaces or uncertain behavior, identify atomic units, dependencies, and owners before
implementation. For planning depth and when to ask, see CLAUDE.md work_selection.

## Implement

1. Read the edit targets and the nearest relevant pattern. Before adding a family member, trace its sibling's
   registration (see the investigation-patterns skill). Before removing or migrating a definition, search the
   identifier itself, including declarations, comments, and test doubles. Use references to establish caller
   impact for interface changes.
2. Implement directly or delegate substantial independent units (see CLAUDE.md delegation). Do not require an
   agent for each file. Each writer returns evidence, not self-approval.
3. Add or update tests only when the task asks for tests or the repository has tests for that kind of change.
   A user instruction not to write tests takes precedence over any other guidance. Document changed public
   behavior. Do not create unrelated tests or docs to fill a role.
4. When the task commits, put structural changes (refactor) and behavior changes in separate commits. Split the
   diff by kind first, then check each structural commit: reverting it must leave behavior unchanged. If it does
   not, the commit still holds a behavior change, so split that out again. Create and split these commits with jj,
   following the jujutsu skill, and pass explicit paths (CLAUDE.md `hard_rules`). The Commits section of the
   pull-request skill governs the split of a whole pull request; this step governs the commits within a task.

## Verify and review

1. Name the applicable test, lint, build, type, or artifact-validation commands from the project, with their
   configuration, generated outputs, and fixture or service requirements. Keep scratch outputs isolated from
   other sessions. Before claiming a check passed, see CLAUDE.md evidence.
2. Use diagnostics when available, otherwise repository commands. For behavior unavailable here, search
   existing helpers, scripts, and configuration for a fixture, in-memory adapter, or substitute backend;
   report the search and the remaining gap.
3. For failing tests, allow one targeted fix attempt across the task, then rerun. Remaining failures are
   blockers (see CLAUDE.md hard_rules).
4. Review the settled change for correctness, caller impact, and missing acceptance coverage. For when to add
   a review or verification agent, see CLAUDE.md work_selection. Use a security agent for security or data
   changes, a verification agent for an untested interface change, and a design or performance agent when the
   user requests that role. Independent reviews may run together, read-only; do not assign multiple reviewers
   the same check. Give claim-verification agents the claim and supporting commands, not merely a diff. If
   agents are unavailable, perform the checks here and disclose that they were not independent.

## Finish

For memory writes, see CLAUDE.md memory_policy. Do not launch a memory agent or a freshness audit by default.

Reply in plain prose in the output style's form: changed paths, verification scope including new files,
review concerns checked, uncovered acceptance criteria, and the weakest consequential claim with what would
confirm it when one remains.
