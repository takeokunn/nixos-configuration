---
argument-hint: [task-description]
description: Full task execution with feedback loop
---

Execute, review across the listed quality dimensions, then address confirmed findings within the approved scope.
Continue between phases without routine confirmation; ask when an action needs new authority or a scope decision.

## Rules

Critical:

- Allow at most one fix iteration. Report remaining failures and the decision needed rather than starting another.
- Write and run tests for all implemented functionality; failures feed the fix phase, never an excuse to
  complete quietly.
- Never commit to the default branch, and never mutate shared working-tree state to escape a problem.
  SSOT-EXEMPT: restated because the failure is irreversible.
- Describe each fix by the change and its verification.
- Load pull-request before writing a commit message or a PR body; it owns what belongs in one.

Important:

- Skip the fix phase when no confirmed critical or warning findings and no test failures remain. Report
  informational findings without treating them as permission to expand the task.
- Fix only what the review identified: a broad rewrite here means a planning failure, and discards the review
  that justified it.
- Completion requires passing assertions over the changed behavior, plus applicable lint, build, and type checks.
  Report commands, exit statuses, selected inputs, and uncovered acceptance criteria; zero exit alone is not proof.

## Workflow

### Prepare

1. Load execution-workflow with Skill: it governs the delegation contract, definition of done, and review
   criteria all three phases use.
2. Read both memory stores: memory_policy in CLAUDE.md splits them. Auto-memory (MEMORY.md index): traps this
   project already cost someone, and issues a previous run deferred: inherited work, not a clean slate. Serena:
   {feature}-patterns, {language}-conventions, testing-patterns, and any completion-checklist or canonical-gate
   memory: the last says which commands mean done here, and what they deliberately don't cover, without
   re-deriving it from build files. Querying only one store returns an empty result indistinguishable from a
   checked "found nothing". Use Read (auto-memory MEMORY.md and the entries it names) and Serena
   activate_project, list_memories, read_memory. Return matched memory names per store, the ones loaded, and the
   deferred issues inherited.

### Execute

1. Split the work into atomic units with stated boundaries. Adding a member to an existing family (module,
   entity, test, command)? Search the nearest sibling's identifier repo-wide, then inspect the loaders,
   manifests, or discovery rules that register that family. Return the atomic units, required touch points,
   and file:line evidence for automatic discovery. A missing search hit proves neither automatic discovery nor
   complete registration coverage; report unresolved paths as gaps.
2. Assign an agent per unit, mark the independent ones, and define what proves it done in two lists:
   command-discharged (name it) and artifact-discharged (name the file:line). Anything else is a discussion
   point, not a checklist entry. Return the assignments with parallel groups; the verification checklist split by what
   discharges each item.
3. Before any code is written, send the planned placement to Agent (design): module/layer for each new symbol,
   and its dependencies. Placement is the costliest finding to repair, since fixing it means moving code and
   dependencies rather than rewriting, and with one fix iteration, a layering violation caught in the review wave
   has no budget left to correct. Return placement approved, or the layer violation named before implementation.
4. Delegate each unit with Agent, with its scope, paths, deliverable, and verifying command. For a removed or
   migrated definition, have the assignee grep the identifier itself, not its usual call shape: forward
   declarations, differently-shaped call sites, comments, and test doubles share only the name.
5. Establish what the verification command covers before running it; name isn't scope: config may exclude part
   of the tree, the editor/language server may read a different config than the command does, and a narrowed
   filter can still pull in shared fixtures. Confirm it covers what changed and that its input set saw files
   created this session: a tool snapshotting from version control or reading an explicit entry manifest silently
   skips an untracked new file. Return the covered scope, the config defining it, and confirmation the new work
   was included.
6. Confirm the tree is quiescent before compiling: every write-capable agent has returned, no fix in flight.
   Compiling over an edit mixes generations: the suite exercises a stale unit while isolated source-preferred
   runs pass, two disagreeing results from the same source. Freeze edits, compile to completion, then run the
   suite fresh with Bash (test runner). Return the results and failing test names, feeding the review and fix
   phases.

### Collect feedback

Dispatch all six review agents in one message with Agent: quality-assurance, security, design, docs,
performance, test. This phase is read-only for all six roles, including test and docs: inspect existing
artifacts and report findings without creating or editing tests, documentation, or other files. Return six reports.

### Checkpoint after collect_feedback: feedback quality

Per gate_discipline in CLAUDE.md. Name:

- All six agents and what each returned; name any that timed out or died: a missing report is not an absence of
  issues.
- Per issue, the file:line or command output it cites: findings citing nothing checkable are a retry condition,
  not a clean result.
- The issues classified critical and the runtime impact making each one critical, or that none are.

Unmet: re-run the named agent once with a narrower prompt naming the specific files. If it fails again, review
that dimension here and report that the delegation failed.

### Fix issues, when confirmed critical/warning findings or test failures remain

1. Consolidate the findings with execute's test failures, then confirm each still holds before acting: open the
   cited file:line and check the condition is present now. Return the consolidated list, each marked
   still-present or already-resolved with the line that shows it.
2. Prioritize confirmed critical issues, then warnings and test failures; report informational findings without
   expanding this fix pass. Delegate each fix to the agent matching its
   category with Agent; verify each fix against its issue and re-run the verification commands with Bash. Return
   the fixes with verification results.

### Checkpoint after fix_issues: fix complete

Name:

- Each critical issue and the file:line of the change addressing it, or the reason it was deferred.
- The commands re-run after the fixes and their exit status.
- For any symptom that stopped appearing, name the change that stopped it: "it doesn't happen anymore" is
  equally consistent with the fix working, a rebuild clearing a stale artifact, or an unreliable observation:
  without a named diff hunk, a cache clear gets recorded as an engineering win while the defect stays open.
- Any test added to guard a fix ran against the pre-fix state and failed there: a regression test never seen red
  asserts the fix rather than guarding it, and an arrange step that steers the system away from the tested
  condition looks like careful setup on inspection.
- Every issue left unaddressed (warnings judged infeasible included) with location and reason, in a form the
  next review can reconcile: unfixed and uncarried, a finding is rediscovered as new or not at all, and one fix
  iteration leaves no other tracking mechanism.

Unmet: report the unaddressed issues as deferred, with reasons. Do not open a second fix iteration.

### Verify

Dispatch verification with Agent against the completion claim itself, once the fixes land and before anything is
reported done. Not a seventh review dimension: collect_feedback judges whether the work is good; this attacks
whether the "it works" claim survives: boundary values, interrupted operations, idempotency, error paths the
happy-path suite never entered. Give it the commands said to exit zero and the claim each supports; handed a
diff, an agent just re-reviews the diff. Return the claim attacked and what survived, or the input that broke it.

### Persist

Write the fix phase's unaddressed issues to auto-memory as a ledger; one entry per issue: identifier,
file:line, severity, deferral reason. Then, against memory_policy in CLAUDE.md, capture what's expensive to
re-derive and ungreppable: the
canonical verification command and its blind spots; the exact zero-exit invocation, environment prefix and path
flags included; and any abstraction deliberately unbuilt, paired with the condition that should re-open it:
recorded without a trigger, a deferral gets re-argued next session with less information than this one had.
memory_policy picks the store: the ledger and traps go to auto-memory, the symbol-anchored pattern to Serena.
Then verify the memories read in prepare: bump, correct, or archive. Use Read and Write (auto-memory MEMORY.md
and its entries), Serena list_memories, write_memory or edit_memory, rename_memory. Return the ledger entries
written, the memories written, edited, or archived, or "persist: no triggers matched, skip", which requires the
deferred-issue list to be empty as well.

## Checkpoint: group consistency

Name:

- Any required section absent or out of order, or that all are present.
- The branch or worktree the work happens in, confirmed not to be the default branch.

Check the branch or worktree before implementation. Resolve missing report sections before returning the result.

## Agents

Roles this command dispatches. The subagent_type's own description is injected by the harness; what appears here
is the contract this command adds.

- **design**, subagent_type design: runs twice: on the planned placement before implementation, and on what was
  built in the review wave; every violation documented with its location.
- **quality**, subagent_type quality-assurance: syntax, type safety, format; issues with severity and file:line
  evidence.
- **security**, subagent_type security: vulnerabilities introduced by the change, with CWE and file:line.
- **test**, subagent_type test: during implementation and fixes, tests for acceptance criteria plus their run
  command; during feedback, read-only assessment of coverage and test validity. When writing tests:
  never pair an always-passing test with a comment explaining it can't be verified here: read the existing test
  helpers first, since the harness usually already has the capability, and a left-behind rationale suppresses
  the next attempt too. Remove a stub of this shape, don't keep it. Constraint: a test guarding a specific fix
  must be run against the pre-fix state and observed to fail there before it counts as a regression test.
- **docs**, subagent_type docs: during implementation and fixes, document changed interfaces and behavior;
  during feedback, report missing or stale documentation without editing it.
- **performance**, subagent_type performance: cost of the change, quantified only where measured on both sides.
- **debug**, subagent_type general-purpose: failures during implementation or test execution. Constraint: a
  symptom that stopped appearing closes only when the change that stopped it is named: report the diff hunk, not
  the absence.
- **refactor**, subagent_type general-purpose: structure improvements preserving observable behavior.
  Constraint: removing or migrating a definition? Grep the identifier itself across every file, not its typical
  usage shape: it's the only invariant shared by forward declarations, differently-shaped call sites, comments,
  and test doubles.
- **verification**, subagent_type verification: attacks the completion claim after the fixes land, not a seventh
  diff review: give it the commands claimed to exit zero and the claim each supports, not the diff.
- **memory**, subagent_type general-purpose: decisions and patterns to whichever store memory_policy assigns
  them, and freshness of the memories consulted this task.
- **validator**, subagent_type validator, dispatched on demand: re-derive one disputed claim from its citation
  alone, without the originating agent's reasoning: only when two agents disagree and evidence doesn't settle
  it, or a consequential claim carries none.

For work outside these roles (dead code, error handling, migrations, schema, infrastructure, CI, observability),
pick the matching subagent_type from the injected listing.

## Execution graph

- **execute** (sequential phase), depends on none:
  - placement_review (sequential step): design, before any code is written
  - implementation (independent units in parallel): task-matched implementation agents, test, docs
  - consolidation (sequential step): wait for every write-capable agent, then compile and run the suite in a
    fresh process
- **feedback** (sequential phase), depends on execute:
  - feedback_agents (parallel, read-only group): quality, security, design, docs, performance, test
- **fix** (conditional phase), depends on feedback:
  - Condition: a confirmed critical or warning finding, or a test failure remains
  - Skip when: none remain; report informational findings without a fix iteration
  - Pass forward: the consolidated issues with the file:line each cites, the agent reports, and the test failures
- **verify** (sequential step), depends on fix: verification, against the settled post-fix artifact
- **persist_phase** (sequential step), depends on verify: memory

## Decision criteria

1. **Task clarity.** The request admits two readings that produce different implementations. Ask with
   AskUserQuestion before delegating; do not implement the cheaper reading.
2. **Implementation quality.** No test command ran, or one failed. Run it, or record the failing test names as
   issues feeding the fix phase: an unverified implementation is not a completion candidate however clean the
   reviews look.
3. **Feedback severity.** A confirmed critical or warning finding, or a test failure remains. Enter the fix
   phase within the approved scope; ask before a fix requiring new authority.
4. **Fix completeness.** A critical issue is neither fixed-and-re-verified nor recorded as deferred with a
   reason. Report it as an open blocker rather than closing the cycle.

## Output

Follows output_contract in CLAUDE.md. verification carries the zero-exit commands enumerated for this change and
which actually ran, plus the test command's scope and confirmation it saw files created this session. Add:

- files_modified: path and what changed, per file.
- review: per agent: completed, timed out, or nothing checkable, with evidence tier. Then issues grouped
  critical/warning/info: category, location, and, for ones the fix phase saw, whether still present at fix time
  with the line showing it.
- fixes: per issue addressed: the finding and the fix, named as a change, not a symptom's disappearance. Then
  deferred issues with location and reason: replaced by an explicit skip confirmation when review found nothing.
- weakest_claim: the claim resting on the thinnest evidence, and what would confirm it.
- next_steps: recommended follow-up, if any.
