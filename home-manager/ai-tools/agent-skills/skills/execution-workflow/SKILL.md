---
name: execution-workflow
description: Load when isolating work in a jj workspace, dispatching several agents, reconciling conflicting agent reports, triaging a CI failure, or judging whether non-trivial work is done. Covers orchestration phases, delegation partitioning and retries, verification gates, choosing the proof for a change, and code review standards. Not for authoring agents or commands, see workflow-patterns for that.
metadata:
  version: "4.5.0"
---

How work gets placed, dispatched, verified, and judged done. CLAUDE.md's `delegation` and `evidence` sections are
the contract, assumed here; this file carries the procedure, the gates, and the delegation detail that does not
need to be resident. Where the two appear to disagree, CLAUDE.md wins: it is resident in every request and this
file is not.

## Orchestration

### Analyze before dispatching

State what is being asked in one sentence. Use the runtime's user-question mechanism only when the answer
materially changes the work; otherwise state a reasonable assumption and proceed.

**Audit a broad directive against the current tree before treating any item as unmet.** A multi-item instruction
from a plan, a prior review, or a hook's rubric often contains items already satisfied, and re-doing them is the
most common source of wasted parallel waves. Where the directive names a tool-defined property (dead code,
duplication, cyclomatic complexity), run that tool's own detector rather than judging by reading.

When Serena is available and relevant prior knowledge could change the work (project conventions, a completion
checklist), call `list_memories` once and read only matching entries.
Treat memories as historical evidence; verify claims that affect this task against the current tree.

Identify which subtasks are independent. **Two subtasks writing to the same file are not independent however
unrelated they look, and a change that must land atomically across several files is one subtask however many
files it spans.**

A call edge between subtasks does not by itself make them dependent. When the callee's contract (types,
signatures, API shape) is first written down as an artifact both sides read, caller and callee can proceed in
parallel while their write sets stay disjoint. A written contract does not split an atomic multi-file change.

### Dispatch

Record file ownership in the dispatch prompts so it can be checked; use a separate partition artifact only when
the coordination needs it. Keep shared-file and atomic multi-file edits together; delegate independent units only
when CLAUDE.md's `delegation` criteria justify the cost.

Prefer a purpose-built agent, then a general-purpose one. When repurposing an agent outside its specialty, say in
the prompt what it is standing in for: **the first entry in an agent's own decision criteria can fail closed on a
task it was not designed for, and a dispatch-prompt override is not a guarantee the gate will yield.** Check the
returned report for evidence the agent did the work rather than refused it politely.

Partition ownership before dispatch. Run independent tasks together; keep atomic cross-file changes together
and resolve shared-file edits before parallel writers. State in each prompt whether external reads are needed.
Use runtime restrictions when available; a prompt alone is not an access control. Agents preserve others'
edits, use unique scratch paths and leave them in place (the auto-mode classifier denies `rm -rf` on them), and
return evidence. Record an isolated agent's base ref and recheck its findings against your tree when the refs
differ. Give a resumed agent explicit ownership and end state.

Dispatch independent tasks together using the runtime's agent tools. Size the fan-out by the independent subtasks
found above: comparing alternatives usually needs two to four agents; a wider fan-out suits only a broad survey
of independent parts. Give each concurrent writer a unique scratch path inside its assigned project or worktree;
create a worktree only when concurrent writers need one. The Agent tool's `isolation: "worktree"` creates one and pins
the agent to it, so Edit refuses the existing worktree you meant it to change; omit `isolation` for work on an
existing checkout. When Edit refuses, an agent that writes the file through a shell command instead has evaded
a boundary, not fixed it.

### Consolidate

Check each report against the questions it was given: did it answer all of them, and does each finding cite a
file:line or command output? A report citing nothing checkable is a retry condition, not a result.

Synthesize the accepted findings yourself. Verify any fix an agent prescribed before adopting it: **a correct
diagnosis routinely arrives with a fix that breaks the build.** Report a rejected fix and its evidence; persist it
only when it meets CLAUDE.md's `memory_policy`.

Treat results from parallel worktrees as competing alternatives, not composable increments: two working versions
of the same area are a choice to make, not two halves to merge.

Apply CLAUDE.md's `memory_policy` and serena-usage's store selection before persisting a durable discovery.
Read-only work returns a candidate without writing. Check staleness only for memories this task read; never read
an entry solely to refresh it, which turns every task into an index sweep.

### Cross-validate what would be expensive to get wrong

For a finding that is expensive to get wrong, obtain a second analysis from a *different evidence base*: a
different tool, entry point, or artifact. Naming that base is the work; CLAUDE.md's `evidence` section says why
repeating one base proves nothing. A second persona prompt with the same model, input, and tools is not a
different base: its errors correlate with the first.

### Resolving disagreement

Resolve disagreement by evidence, not votes or agent titles. Prefer inspected source, version, and lifecycle
over convention. Re-read the disputed location when both sides cite evidence; report unresolved alternatives
rather than averaging them. An author's self-approval is not independent review. Repeated gate complaints with
no new evidence call for a user decision, not another identical cycle.

### When something fails

When a sub-agent failed or returned nothing checkable, retry at most twice, and only for a timeout, an
incomplete answer, or missing citations. Inspect status and logs before retrying a silent agent, and partial
edits before redispatching a writer. Narrow the retry prompt to the specific files and the single unanswered
question, since re-sending the same prompt tests nothing new. If the agent is unavailable or still failing,
perform the checks yourself and disclose the limitation. A missing report never means no findings.

Name the cause before choosing the response:

- **Missing context**: the agent lacked a file, a fact, or a precise question. Retry in the narrowed form above.
- **Missing permission**: a hook, sandbox, or authorization boundary stopped it. Do not retry or rephrase around
  the boundary; report the blocker and the authority it needs.
- **Insufficient capability**: the task exceeds what the agent or tool does in one pass. Split the task or change
  the approach, within the same retry budget.

When missing permission is among the causes, it decides the response. A timeout or crash with no visible cause is
retried under the budget as it stands.

No relevant memory exists: continue from current evidence. Investigate only a gap that blocks this task; absence
of a memory is not itself a requirement to research or write one.

### A failed CI job

Tie the diagnosis to the exact commit and job; a pull request's summary status can describe an older run.
Before treating a cancelled run as a failure, check whether a newer run on the same branch superseded it. Fetch
the failed job's log once and reuse it.

Classify the failure as product, harness, infrastructure, or credential before responding. Only a product
failure is fixed in product code; a credential failure is a blocker to report with the authority it needs, not
to retry. Baseline and harness checks are in CLAUDE.md's `evidence`; a rerun that turns green is not a fix.
Trace a cause with [investigation-patterns](../investigation-patterns/SKILL.md).

## Gates

Clear each gate with a concrete path, command, agent result, or file:line, never a bare "yes", and obtain missing
evidence before proceeding. Each gate's checklist follows.

### After analysis, before delegating

- Each sub-agent selected and the one question it will answer.
- Relevant memories read, or the reason lookup was unnecessary or unavailable.
- Which items of the incoming directive were already satisfied and are excluded.
- Which subtasks run in parallel, and the dependency forcing the rest to be sequential.

Unmet: do not delegate. Obtain the missing item, then re-run the gate.

### After writing the prompts, before dispatching

- Every subtask maps to a dispatched agent, or to an explicit decision to do it here with the reason.
- No two agents in the same message write to the same file, and no atomic multi-file change is split across
  agents. If either could happen, the tasks are not independent: serialize them or give each its own worktree.
- Each prompt names the files, the specific change wanted, and the command that verifies it.
- Each prompt tells the agent to keep scratch artifacts inside its own worktree.
- For a worktree-isolated agent, that you will re-check its findings against your branch when its base ref
  differs from yours.
- No timing measurement is requested from an agent running concurrently with others; parallel load invalidates it.

Unmet: revise before dispatching. If only the user can resolve the ambiguity and the readings differ in a destructive or external effect, ask; otherwise state the assumption.

### Before editing code

- The target file was read *in this turn*, not an earlier one.
- The work is on a feature branch or in a worktree, never the default branch.
- The change follows a pattern already present in the file; any deviation is stated, not introduced silently.
- If verification is currently blocked, the edit does not proceed on static analysis alone. **Static analysis
  supporting a change is evidence, not authorization**: either restore the ability to verify, or state that the
  change ships unverified and why that was accepted.
- If a mechanical gate rejected the edit, identify the violated rule. Do not evade it by creating a sibling entry
  or rephrasing the same prohibited change.

### Before compiling or testing against a shared artifact

- No agent is still editing sources that feed the artifact. Compiling while another agent writes produces a
  mixed-generation artifact set, and the suite may exercise an older wrapper while the source-level check passes.
- The runner loaded the source you changed, not a stale build product. Stale artifacts generate false reds as
  readily as false greens, and **false red is the more expensive one**: it sends you to fix code that is already
  correct.

Unmet: freeze edits, rebuild to completion, then run the suite in a fresh process. Do not delete or rewrite shared
build artifacts as a workaround during concurrent work; that breaks other sessions' verification.

### Before reporting complete

Establish the applicable evidence below, then report the result and verification gaps through CLAUDE.md's
`output_contract`. Do not paste the whole checklist when the concise report covers it.

- The exact verification command and its exit status, or that none ran. "Should work" is not a verification.
- What that command covers: which files, selectors, platforms. A file created this session may be invisible to
  the project's canonical command if never added to the manifest that command reads.
- The count of tests or items the gate selected, nonzero and matching expectation. A selector matching nothing
  exits zero.
- That the gate's input was non-empty, naming the assertion used. An empty tree passing most of a check suite is a
  vacuous pass.
- For a generated artifact, the observed bytes or size of the output, not just that generation succeeded.
- For a bulk replace or regex edit, a grep for the glued or concatenated forms it could produce and a run of the
  edited artifact. A syntax or balance check cannot see a wrong symbol or two lines merged into one.
- For a failed check, the baseline before calling it a regression. Do not attribute a pre-existing failure to this
  change.
- For a new probe, that it fails against a known-good control, and that state it created is kept apart from the
  original state. Unit tests do not establish integration behavior.
- Where several agents verified, whether they used the same command. The same command run N times is one tier of
  evidence, not N.
- Anything asked for that was not done, and why.
- If a durable discovery earned a memory, its store and write outcome, or the unpersisted candidate.

Unmet: **missing evidence is not a pass.** Run the missing verification now rather than reporting around it. Where
no real gate exists in this repository, enumerate the manual checks performed and label them manual. Before
declaring something unverifiable, check whether the tool offers a fake, offline, or dry-run mode.

## Workspace isolation

Use jj, following [jujutsu](../jujutsu/SKILL.md). First inspect initialization, the existing workspace, and
current on-disk changes. A suitable existing workspace needs no new version-control writes. If the directory has
no `.jj/` of its own, do the file work in place and report the missing isolation; do not fall back to git.
Fetching, creating a workspace, and creating a feature bookmark are local writes that need no separate
authorization when the task calls for isolation.

1. Inspect `jj workspace list --ignore-working-copy --no-pager` and the intended workspace's ownership and
   changes, including untracked files. Do not move or discard another session's working copy.
2. Obtain the default branch name with `gh repo view --json defaultBranchRef --jq .defaultBranchRef.name`.
3. Fetch the named base with `jj git fetch --remote origin --branch <default>`. Compare
   `jj log -r '<default>@origin' --no-graph -T 'commit_id'` with
   `gh api repos/<owner>/<repo>/git/ref/heads/<default> --jq .object.sha` before using it as the base; a local
   remote ref alone is not freshness evidence. If they differ, fetch again with explicit `--remote` and
   `--branch` and recompare; if they still differ, stop and report.
4. When a separate workspace is needed, choose a nonexisting destination within the project's `.worktrees/`
   directory or the scope the user named, and use `jj workspace add --no-colocate --sparse-patterns full --name
   <name> -r <verified-base> <destination>`. This creates an empty working-copy commit whose parent is the
   verified base, not a working copy with the base's own commit ID.
   Activate that workspace as the project root before editing.
5. Create a feature bookmark when needed for delivery. Do not commit to the default bookmark unless the user
   asked; push and PR creation need confirmation.
6. Report the workspace path. Do not forget or remove it without the user's say; cleanup deletes work.

A workspace under the repository root can inherit parent configuration through directory-upward search: tool
configs, environment files, and ignore rules. Inspect that inheritance when verifying in isolation. Do not append
ignore rules or silently write to adjacent checkouts. Activate a separate location as
the project root before edits.

### Asking whether a branch's work already landed

Ancestry and content are different questions. For ancestry, inspect
`jj log --ignore-working-copy --no-pager --no-graph -r '<branch> & ancestors(<main>)'`: a nonempty result means
the branch tip is an ancestor of main. A squash merge creates a new commit and need not preserve that ancestry.
For current content, use `jj diff --ignore-working-copy --no-pager --git --from <main> --to <branch> -- <paths>`.
Later unrelated changes can also appear, so inspect the scoped change and relevant history before concluding the
work landed. Neither an ancestry result nor an empty scoped diff alone establishes all delivery criteria.

### After merging, and before tagging

Green on each branch is no evidence about their union: a signature change on one side breaks stubs on the other,
and a type-aware lint can fail only on the combined tree. Rerun the typecheck, lint, and tests on the merged tip,
one merge at a time. A commit message claiming another branch fixed something needs ancestry evidence against the
tip being released, or scoped content and history evidence for a squash merge.

Run the release gate on the exact tree being tagged, after the version bump, and grep the tests for the old
version string, since some suites mirror it. Tag the SHA confirmed with
`gh api repos/<owner>/<repo>/git/ref/heads/<branch> --jq .object.sha`, not a local `<branch>@origin` that a
fetch without explicit `--remote` and `--branch` left stale. A burned version number cannot be reused.

## Prohibited

CLAUDE.md's `hard_rules` states the version-control and shared-working-copy prohibitions; this file adds the
orchestration-specific list:

- Delegating synthesis. Synthesize first, then write prompts that prove you understood: paths, line numbers, the
  specific change, the verification command. **The orchestrator owns synthesis; sub-agents own execution.**
- Overlapping concurrent writers or verifying an artifact while its sources are changing.
- Creating a workspace when a suitable one exists. Bounded local work does not require delegation or a new
  workspace.

## Definition of done

Done requires the requested outcome and meaningful verification, not merely zero exit statuses.

Identify the project's canonical gate and the narrow checks meaningful for the affected paths. Inspect the
selected inputs and assertions; run the applicable checks and name missing coverage. Never report a subset as the
full gate. A bounded local change does not require unrelated suites.

A failing pre-push or pre-commit hook is evidence about the work, not an obstacle in front of it: fix the work,
never bypass with a skip-verification flag, and read a red CI job the same way.

A gate that selects and runs zero tests is a false green: assert a nonzero selected-test count before reading a
pass as a pass. See [test-integrity](../test-integrity/SKILL.md) for the full treatment of selector, double, and
teardown traps.

### Unattended loops

CLAUDE.md's `work_selection` already holds that reaching an iteration limit is not completion. Before
compaction or handoff, retain the objective, changed paths, exact checks and results, the decision criteria in
force with the options they rejected, and the unresolved work; recheck live state when resuming. A loop that runs
while the user is not watching (`/loop`, a scheduled wakeup, `/goal`, a repeated headless run) needs three things
fixed before it starts:

- A stop condition decided by a command and a threshold: an exit status, a test count, or a measured value
  together with a no-regression clause. A condition needing judgment the loop cannot supply, such as taste or a
  product decision, goes to the user.
- An iteration limit.
- One item per iteration, verified before the next begins, so a failure points at one change.

When the stop condition passes while the no-regression clause fails, the loop stops and reports.

A `/goal` condition is judged by a separate model that reads only the conversation; it runs no commands and reads
no files. Write the condition so the transcript proves it, such as "the test command exits 0 and its output is
shown", and bound it with a turn limit such as "or stop after 20 turns".

### Choose the smallest proof that covers the change

Start with the check that exercises the changed contract; widen it after a failure, after a further change, or
for a risk it cannot reach. The project's canonical gate keeps its own coverage; the next section's tier table
states what each check establishes.

| Change | First proof |
|---|---|
| Runtime defect | A narrow reproduction; after the fix, it plus the sibling tests |
| Ordinary source change | Changed-file checks and the tests that own the changed code |
| Public interface | Changed-file checks plus representative consumer tests |
| Build output, lazy loading, or a package boundary | A real build of the artifact, plus the tests |
| CI workflow definition | The workflow linter and a whitespace check of the diff |
| Documentation only | Format and link checks; no runtime suite |

### Report the verification tier you actually reached

| Tier | What happened |
|---|---|
| 1 | Static read or parse check: no target behavior was exercised |
| 2 | Interpreted or partial load: the code loaded but was not compiled or exercised |
| 3 | Real compile, load, and run of the relevant tests locally |
| 4 | The project's canonical gate green in CI, on a clean environment |

Name the tier achieved, and say plainly that a lower tier is not equivalent to a higher one *even when it found
real bugs*: hand-tracing catches genuine defects and is worth doing, but is not a compile-and-run confirmation.
Record the exact command the next session should run first to close the gap, so resuming is a lookup rather than
a reconstruction. Report which checks ran and which could not, with the reason: **a silently omitted check reads
as a passed check.**

The gap between tiers is real risk, not bookkeeping: bugs surviving extensive local smoke testing are routinely
caught only by a full clean-environment run.

## Code review

Categorize findings by what the reader must do: **critical**: security, data corruption, breaking changes, must
fix before merge; **important**: logic errors, missing error handling, performance, should fix; **suggestion**:
style, refactoring, documentation. Report findings and open questions; include positive observations only when
they change a reader decision. Every finding carries a file:line and a concrete change, never a direction to
improve.

Check a reviewer's finding, from a person or an agent, against the current tree before applying it; for one
that is expensive to get wrong, see [Cross-validate](#cross-validate-what-would-be-expensive-to-get-wrong). A
review that timed out, stopped early, or covered part of the change is incomplete, not clean: report what it did
not cover.

### Choose the lens deliberately

Convention-conformance review and behavior review are different reviews, **and the first will approve what the
second rejects.** A reviewer working from a conformance checklist systematically cannot catch a correctness
defect spelled like the convention: every box ticks, and the change ships with the bug the convention was meant
to prevent.

If the change's stated purpose is behavioral (performance, correctness, concurrency), a conformance pass is not
sufficient evidence. Never report a high conformance score as approval; state which lens produced it, so a later
reader does not treat a style pass as behavioral clearance. When two reviews of the same change disagree sharply,
the cause is usually a lens difference, not a judgement difference: identify each lens before reconciling the
verdicts.

### Committing in a shared checkout

Commit when the task calls for it (CLAUDE.md `hard_rules`). jj has no staging area: `jj commit` takes the whole
working copy unless you pass paths.

1. Inspect `jj status` and the full `jj diff --git` before committing anything, without
   `--ignore-working-copy` (see CLAUDE.md). jj ignores git's external differ, so `--git` emits a plain unified
   diff that is parseable.
2. If every hunk is cleanly attributable to your own work, commit only those paths
   (`jj commit -m "<msg>" <paths>`).
3. If a shared file (an export list, a build manifest, a lockfile) carries changes interleaved with someone
   else's, commit only your own paths, leave that file out, and report it. Do not bundle the interleaved
   changes or split them speculatively; whose-work-is-it is not inferable from the diff.

## Concurrent sessions in one checkout

CLAUDE.md's `hard_rules` lists the destructive shared-tree operations. Use an assigned isolated worktree when
available. A jj workspace or a WIP change is a local write that needs no separate authorization; neither is a way to
get around a blocked command.

Do not mirror a worktree into a shared checkout: overwrites and sync deletion can destroy concurrent work without
changing Git metadata. Hand off the changed paths and diff, and leave integration to an
operation that preserves the destination's unrelated changes. When integrating, check the agent's
scoped diff against the destination's current content, apply it with the available patch tool, and check for
conflict markers afterwards. Copying its whole files onto a base that moved since reverts the work merged in between, and the tree and gates stay green.

Remove a linked worktree only when removal is requested and its tracked, untracked, and ignored work has been
checked for preservation elsewhere. A clean tracked diff alone does not establish that removal is safe.

## Related

Naming a skill here does not load it. Use the runtime's skill loader, or read its SKILL.md, when triggered.

- [serena-usage](../serena-usage/SKILL.md): before any memory check or symbol operation
- [investigation-patterns](../investigation-patterns/SKILL.md): when review reveals behavior that is unclear
- [testing-patterns](../testing-patterns/SKILL.md): when verifying coverage or designing a suite
- [test-integrity](../test-integrity/SKILL.md): when a gate reports green
- [workflow-patterns](../workflow-patterns/SKILL.md): for the refutation pass
