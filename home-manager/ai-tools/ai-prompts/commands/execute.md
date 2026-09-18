---
argument-hint: [task-description]
description: Task execution command
---

Execute a task through scoped delegation, verification, and at most one targeted fix attempt for failing tests.
For review across all listed quality dimensions, use /execute-full.

## Rules

Critical:

- Write and run tests for all implemented functionality: untested code isn't complete, however clean it reads.
- One fix iteration only for failing tests: report remaining failures as a blocker; a second pass hides a scope
  problem that's the user's call.
- Never commit to the default branch, and never mutate shared working-tree state to escape a problem.
  SSOT-EXEMPT: restated because the failure is irreversible.
- Load pull-request before writing a commit message or a PR body; it owns what belongs in one.

Important:

- Delegate detail: run independent units in parallel, dependent ones in order, and verify output before
  integrating it: a report citing nothing checkable is not a result.
- Completion requires passing assertions over the changed behavior, plus applicable lint, build, and type checks.
  Report commands, exit statuses, selected inputs, and uncovered acceptance criteria; zero exit alone is not proof.
- When a mechanical gate rejects an edit, identify the violated rule and correct the edit within the approved
  scope. Do not bypass the gate or weaken it. Report a suspected gate defect with evidence.
- Aligning with a reference implementation is one-directional for anything fail-closed (security gates,
  verification strictness, fail-closed defaults), move looser to stricter, never the reverse; a strictness the
  reference lacks is an asset, not a divergence.

## Workflow

### Prepare

1. Load execution-workflow with Skill: it carries the delegation contract, definition of done, and review
   criteria this command needs.
2. Activate the Serena project, call list_memories, and read only entries matching this task:
   {feature}-patterns, {language}-conventions, testing-patterns, or a
   completion-checklist/verification-command memory giving done-commands without re-deriving them from build
   files. Nothing matching means the index alone is the answer. Use Serena activate_project, list_memories,
   read_memory. Return matched memory names and the ones loaded, or "index matched nothing".

### Analyze

1. Split the work into atomic units and state the boundary of each. Return the task inventory with boundaries.
2. Before adding a module, entity, test, or command, trace how its family is registered or discovered. Search
   the nearest sibling's identifier repo-wide, then inspect the loaders, manifests, or discovery rules that
   consume it. Return required touch points and file:line evidence for each automatic discovery mechanism.
   A missing search hit does not establish automatic discovery; report unresolved registration paths as gaps.
3. Select the best-fit agent per unit, mark which are independent, and order the rest by the specific output
   each waits on. Return the delegation map with parallel groups and the dependency behind each sequential step.
4. Define what proves the work done as two lists: items a command discharges (name it) and items an artifact
   discharges (name the file:line). An item carrying neither is a discussion point, not a checklist entry: a
   prose checkbox gets ticked by impression. Return the verification checklist split by what discharges each
   item.

### Checkpoint after analyze: analysis quality

Per gate_discipline in CLAUDE.md. Name:

- Each task in the inventory and the agent it is going to.
- The registration surfaces found in step 2, or that this task adds no new member of an existing family.
- Which tasks run in parallel, and the dependency forcing the rest to be sequential.
- The branch or worktree the work will happen in, confirmed not to be the default branch.

Unmet: obtain the missing item before delegating.

### Assign

1. For a task modifying an existing symbol, call find_referencing_symbols and embed the reference count and
   affected files in the delegation prompt: a missed caller becomes a failure the single fix iteration can't
   afford. When removing or migrating a definition, grep the identifier itself, not its usual call shape:
   forward declarations, differently-shaped call sites, comments, and test doubles share only the name. Use
   Serena find_referencing_symbols and Grep. Return the blast radius: N references in M files, included in the
   delegation context.
2. Prepare each delegation with its scope, target paths, expected deliverable, verifying command, and any
   reference implementation to follow.

### Checkpoint after assign: assignment complete

Name:

- Every task in the inventory and its planned agent, or that it will be done here and why: a task on
  neither list was dropped.
- The file paths and expected deliverable in each prepared prompt.
- The tasks that must wait, and the specific output each waits on.

Unmet: do not dispatch. Supply the missing item, or ask with AskUserQuestion if only the user can resolve it.

Once the checkpoint passes, dispatch the prepared tasks with Agent, respecting their dependencies.

### Consolidate

1. Check each agent's output for completeness. Use get_diagnostics_for_file (min_severity=2) on modified files
   when available; otherwise use the repository's diagnostic command and name any uncovered files. Return the
   command or tool used and any error diagnostics as blockers.
2. Before running the verification command, establish its actual coverage: name isn't scope, and it diverges
   three ways: config excludes part of the tree; editor or language-server config disagrees with the command's;
   a narrowed filter still pulls in shared fixtures, so a failure need not belong here. One question answers it:
   does it include what changed? Route whatever the gate writes into the tree (build output, coverage data,
   generated loaders) to a temp directory, since an ignored artifact won't appear in a diff. Confirm the run
   starts its own service or fixture, not one an earlier session left running. Return the covered scope with the
   config file defining it; what the gate writes; whether it self-starts.
3. Run the test commands with Bash (test runner): infer from the project's language and framework, then its
   package, build, or manifest, then report a blocker if neither yields one. Confirm the run's input set
   included the new work: a
   tool that snapshots from version control, honors an ignore file, or reads an entry manifest silently skips an
   untracked new file: check it appears in the tool's file list, or the new test in the run count. Return the
   results with the command run, and confirmation the run saw the new files.
4. If tests fail, delegate one targeted fix for the failing tests to Agent (the test agent, or
   general-purpose) and re-run once. If failures remain, report them as blockers and set the status to error.
5. Before reporting something unverifiable here, search the app's environment variables and scripts directory
   for a substitute backend, in-memory adapter, or recorded-fixture mode. Return the mode found and exercised,
   or the exact queries and locations searched without a match. Keep the unverified behavior as a gap; a bounded
   search does not establish that no substitute exists elsewhere.
6. Once green, dispatch verification with Agent against the claim the change works: a green suite is the
   evidence most offered and least attacked, since it only shows the paths someone thought to write still
   behave, a different claim from the one being made. Hand over the zero-exit commands and the claim each
   supports, not the diff: an agent handed a diff reviews the diff, which review agents already did. Return what
   survived the attack and what broke.

### Persist

1. Per memory_policy in CLAUDE.md, three things here are expensive to re-derive: the verification command in its
   exact zero-exit form (environment prefix and path flags included), since a bare tool name costs the next
   session the same trial and error; the canonical gate and what it deliberately skips; and an abstraction
   deliberately not built, paired with the condition that should re-open it, since an untriggered deferral gets
   re-argued from scratch with less information. Check list_memories for the topic first, then write or edit with
   Serena write_memory or edit_memory: output "persist: no triggers matched, skip" when none apply. Return the
   memory names written or edited, or the explicit skip.
2. For memories read in prepare: bump last-verified if still accurate, correct if partly outdated, or rename
   with an -archived suffix if fully superseded; don't read further memories only to check freshness. Use Serena
   edit_memory and rename_memory. Return verified, updated, or archived, or "none read this task required
   verification".

## Agents

Roles this command dispatches: the subagent_type's own description is injected by the harness, not restated;
what follows is this command's added contract.

- **quality**, subagent_type quality-assurance: syntax, type safety, format compliance on modified files; issues
  carry severity and file:line evidence.
- **security**, subagent_type security: vulnerabilities introduced by this change, with CWE and file:line.
- **test**, subagent_type test: tests covering the acceptance criteria, plus the confirmed-executable command
  that runs them. Constraint: never pair an always-passing test with a comment explaining why the behavior can't
  be verified: read the existing test helpers first, since the harness usually already has the capability, and a
  stale rationale left behind suppresses the next attempt too.
- **docs**, subagent_type docs: documentation for changed public interfaces and behavior, with no stale
  references left.
- **review**, subagent_type quality-assurance: holistic post-implementation review across the agent reports and
  test results; go/no-go with rationale.
- **verification**, subagent_type verification: attacks the works-claim once the suite is green: boundary
  values, interrupted operations, idempotency, error paths a passing suite never entered. Give it the zero-exit
  commands and the claim each supports.
- **memory**, subagent_type general-purpose: patterns and decisions surfaced by the implementation agents,
  written to whichever store memory_policy assigns them.
- **validator**, subagent_type validator, dispatched on demand: re-derive one disputed claim from its citation
  alone, without the originating agent's reasoning: only when two agents disagree and evidence doesn't settle
  it, or a consequential claim rests on no citation.

For work outside these roles (refactoring, debugging, performance, dead-code removal, error handling,
migrations, schema, infrastructure, CI, observability), pick the matching subagent_type from the injected
listing and give it the same four things every delegation carries.

## Execution graph

| Group or step | Depends on | Agents |
|---|---|---|
| implementation (independent units in parallel) | analysis and assignment | task-matched implementation agents, test, docs |
| quality_assurance (parallel) | implementation | quality, security |
| review_phase (sequential) | quality_assurance, implementation | review |
| claim_attack (sequential) | review_phase | verification, against the settled artifact |
| persist_phase (sequential) | claim_attack | memory |

## Decision criteria

1. **Task clarity.** The request admits two readings that produce different implementations: ask with
   AskUserQuestion before delegating; do not implement the cheaper reading.
2. **Verification completeness.** No test command was run against the change: run it before claiming
   completion, or report a blocker if none can be inferred from the manifests.
3. **Implementation quality.** A test failed, or get_diagnostics_for_file reports an error on a modified file:
   delegate one targeted fix, re-run once, and report a blocker if it still fails.

## Output

Follows output_contract in CLAUDE.md; verification carries the test command, exit status, what it covered, and
confirmation the run saw this session's new files, or "none run" with the reason. Add: changes as a
path-per-line list of what was edited and why, and the weakest claim with what would confirm it.
