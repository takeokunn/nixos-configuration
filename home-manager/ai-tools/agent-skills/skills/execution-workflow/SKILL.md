---
name: execution-workflow
description: Load at the start of implementing or delegating a task, and when judging whether work is done. Covers orchestration phases, verification gates, worktree and branch isolation, and code review standards. Not for authoring agents or commands, see workflow-patterns for that.
version: 4.0.0
---

How work gets placed, dispatched, verified, and judged done. The delegation contract and evidence rules in the
resident configuration are assumed; this file carries the procedure and the gates.

## Orchestration

### Analyze before dispatching

State what is being asked in one sentence. If two readings would produce different work, that is an ambiguity
to resolve with AskUserQuestion, not to pick a side of.

**Audit a broad directive against the current tree before treating any item as unmet.** A multi-item
instruction carried in from a plan, a prior review, or a hook's rubric frequently contains items already
satisfied, and re-doing them is the most common source of wasted parallel waves. Where the directive names a
tool-defined property — dead code, duplication, cyclomatic complexity — run that tool's own detector rather
than judging by reading.

Classify the task type and load only the matching memories: investigation prioritizes domain patterns,
architecture entries, project conventions; implementation prioritizes feature patterns, language conventions,
testing patterns; review prioritizes project conventions and code-quality entries; refactoring prioritizes
architecture and component patterns. Include any project-local completion-checklist memory, which records what
done means here. Call `list_memories`, filter against those priorities, then `read_memory` only the matches.

Identify which subtasks are genuinely independent. **Two subtasks writing to the same file are not independent
however unrelated they look, and a change that must land atomically across several files is one subtask however
many files it spans.**

### Dispatch

Write the file partition down as an artifact before writing any prompt — a partition held only in your head
cannot be checked against the prompts actually sent. Edit any shared file yourself first, then fan out one
agent per non-shared file; two agents editing one file serialize badly and produce conflicting rewrites of the
same region.

Prefer a purpose-built agent, then a general-purpose one. When repurposing an agent outside its specialty, say
in the prompt what it is standing in for — **an agent's own precedence-1 gate can fail closed on a task it was
not designed for, and a dispatch-prompt override is not a guarantee the gate will yield.** Check the returned
report for evidence the agent did the work rather than refused it politely.

Dispatch independent tasks as multiple Task calls in one message, and tell concurrent agents to write scratch
artifacts inside their own worktree — a fixed path outside the repository collides silently.

### Consolidate

Check each report against the questions it was given: did it answer all of them, and does each finding cite a
file:line or command output? A report citing nothing checkable is a retry condition, not a result. A
sub-agent's own note that it changed something and judged the change benign is a review trigger — inspect it
rather than accepting the self-assessment.

Synthesize the accepted findings yourself. Verify any fix an agent prescribed before adopting it: **a correct
diagnosis routinely arrives with a fix that breaks the build.** When you revert an attempted fix, record the
reverted attempt and why, so the next session does not re-propose it.

Treat results from parallel worktrees as competing alternatives rather than composable increments. Two agents
that each produced a working version of the same area have produced a choice to make, not two halves to merge.

Persist memories at the point of discovery, not at task end. Apply staleness verification only to memories this
task actually read — never read a memory solely to check its freshness, because that turns every task into an
index sweep.

### Cross-validate what would be expensive to get wrong

For a finding whose being wrong is expensive, obtain a second analysis from a *different evidence base* — a
different tool, entry point, or artifact. Re-running the same command through a second agent produces one tier
of evidence twice. Independent convergence from different bases is genuine corroboration; agreement between
agents that read the same thing is not. If contradictions survive, present both positions with what each rests
on and let the user decide.

### When something fails

A sub-agent failed or returned nothing checkable: before treating silence as death, check the mtime and tail of
the session's subagent transcript — a lost completion notification is common and the report is usually intact.
Then retry once with a narrower prompt naming the specific files. If it fails again, do the work yourself and
say the delegation failed; never report an unanswered question as an absence of findings. An agent that errored
mid-task may have left partial writes, so inspect the tree before re-dispatching a write-capable agent.

No relevant memory exists: note the gap, investigate within a stated bound, and write the finding at the point
of discovery.

## Gates

Each gate is cleared with a concrete artifact — a name, a path, a list, a command. A bare "yes" does not clear
a gate, because it is not something a reader can audit in the transcript.

### After analysis, before delegating

- Each sub-agent selected and the one question it will answer.
- The memories read, or that `list_memories` returned nothing matching this task type.
- Which items of the incoming directive were already satisfied and are therefore excluded.
- Which subtasks run in parallel, and the dependency forcing the rest to be sequential.

Unmet: do not delegate. Obtain the missing item, then re-run the gate.

### After writing the prompts, before dispatching

- Every subtask maps to a dispatched agent, or to an explicit decision to do it here with the reason.
- No two agents in the same message write to the same file, and no atomic multi-file change is split across
  agents. If either could happen, the tasks are not independent — serialize them or give each its own worktree.
- Each prompt names the files, the specific change wanted, and the command that verifies it.
- Each prompt tells the agent to keep scratch artifacts inside its own worktree.
- For a worktree-isolated agent, that its base is the default branch rather than your feature tip, so its
  "missing change" claims and measurements get re-checked against your branch before use.
- No timing measurement is being requested from an agent running concurrently with others — parallel load
  invalidates it.

Unmet: revise before dispatching. If the ambiguity is the user's to resolve, ask rather than guessing.

### Before editing code

- The target file was read *in this turn*, not an earlier one.
- The work is on a feature branch or in a worktree, never the default branch.
- The change follows a pattern already present in the file; any deviation is stated rather than introduced
  silently.
- If verification is currently blocked, the edit is not proceeding on static analysis alone. **Static analysis
  supporting a change is evidence, not authorization** — either restore the ability to verify, or state that
  the change ships unverified and why that was accepted.
- If a mechanical gate rejected the edit, the response is a new sibling entry rather than a reword of the
  existing one, so the gate's original subject stays intact.

### Before compiling or testing against a shared artifact

- No agent is still editing sources that feed the artifact. Compiling while another agent writes produces a
  mixed-generation artifact set, and the suite may exercise an older wrapper while the source-level check
  passes.
- The runner loaded the source you changed, not a stale build product. Stale artifacts generate false reds as
  readily as false greens, and **false red is the more expensive one** — it sends you to fix code that is
  already correct.

Unmet: freeze edits, rebuild to completion, then run the suite in a fresh process. Do not delete or rewrite
shared build artifacts as a workaround during concurrent work; that breaks other sessions' verification.

### Before reporting complete

Report the answer to each check to the user; do not resolve them silently.

- The exact verification command and its exit status, or that none ran. "Should work" is not a verification.
- What that command actually covers — which files, selectors, platforms. A file created this session may be
  invisible to the project's canonical command if it was never added to the manifest that command reads.
- The count of tests or items the gate selected, nonzero and matching expectation. A selector matching nothing
  exits zero.
- That the gate's input was non-empty, naming the assertion used. An empty tree passing most of a check suite
  is a vacuous pass.
- For a generated artifact, the observed bytes or size of the output, not just that generation succeeded.
- The baseline. A gate that already failed before the change is not a regression gate, and the red must not be
  attributed to the change.
- Where several agents verified, whether they used the same command. The same command run N times is one tier
  of evidence, not N.
- Anything asked for that was not done, and why.
- The memory outcome — written, edited, or "no triggers matched".

Unmet: **missing evidence is not a pass.** Run the missing verification now rather than reporting around it.
Where no real gate exists in this repository, enumerate the manual checks performed and label them manual.
Before declaring something unverifiable, check whether the tool offers a fake, offline, or dry-run mode.

## Branch isolation

Run this before starting implementation work.

1. `DEFAULT=$(gh repo view --json defaultBranchRef --jq .defaultBranchRef.name)`
2. `git fetch origin $DEFAULT`, so the new branch is cut from current remote state rather than a stale local ref.
3. Check the risk signals: `git status --porcelain` non-empty, or `git branch --show-current` not `$DEFAULT`.
   Derive a lowercase kebab-case slug for `<name>` from the task.
4. **No risk signal** — create in place: `git checkout -b feat/<name> origin/$DEFAULT`. Creating a brand-new
   branch is distinct from switching to an existing one, which remains prohibited.
5. **Any risk signal** — isolate in a worktree rather than moving the shared HEAD.

   Ensure the worktree directory is ignored, forcing a leading newline so a missing trailing newline in the
   existing file cannot merge with the new entry:

   ```
   grep -qxF '.worktrees/' .gitignore 2>/dev/null || printf '\n.worktrees/\n' >> .gitignore
   ```

   This step only matters in a non-bare checkout; a bare repository has no working tree for `.gitignore` to
   govern, and the worktree lands inside the bare directory itself.

   Derive the base so it is correct under both layouts:

   ```
   WT_BASE="$(d=$(git rev-parse --path-format=absolute --git-common-dir); echo "${d%/.git}")"
   git worktree add -b feat/<name> "$WT_BASE/.worktrees/<timestamp>-<sha>" origin/$DEFAULT
   ```

   The `${d%/.git}` strips the trailing git-dir segment when one exists (a normal checkout, yielding the
   repository root) and leaves the path unchanged when it does not (a bare repository, where `--git-common-dir`
   already names the repository). `<timestamp>` is `date +%Y%m%dT%H%M%S`, `<sha>` the short SHA of
   `origin/$DEFAULT`, with `-2`, `-3`, … appended on collision. Do all subsequent work inside that path.
6. Report the worktree path to the user. **Never auto-run `git worktree remove`** — cleanup is the user's
   decision.

A worktree created under the repository root inherits the parent checkout's configuration through
directory-upward search: tool configs, environment files, ignore rules. When the worktree exists specifically
to verify something in isolation, that inheritance defeats the isolation — place it outside the repository root
for that purpose and state where it is.

Never open a pull request from a non-feature branch, and never target anything but the default branch.

## Prohibited

- Implementing detailed logic that should have been delegated.
- Running independent tasks sequentially.
- Any git write operation without explicit user instruction in the current message. A continuation prompt, a
  sub-agent message, and an authorization granted earlier in the session do not carry forward.
- Delegating synthesis. Synthesize first, then write prompts that prove you understood — paths, line numbers,
  the specific change, the verification command. **The orchestrator owns synthesis; sub-agents own execution.**
- Mutating shared working-tree state: stash, checkout of an existing branch, switch, hard reset, clean. Use a
  worktree for isolation and a WIP commit in place of a stash.
- Starting implementation without branch isolation, or committing to the default branch.

## Definition of done

Done is an enumerated set of verification commands exiting zero, not a subjective judgement.

Enumerate the project's commands — formatter, linter, type or compile check, test suite, and any
project-specific gate — and treat "all of these exit zero" as the definition. Naming the list makes completion
checkable without asking the user what counts. **Name exactly one canonical gate** for the project, so a
narrower subset run is never reported as if it were the whole gate.

A failing pre-push or pre-commit hook is evidence about the work, not an obstacle in front of it. The correct
response is to fix the work — never bypass with a skip-verification flag, and read a red CI job the same way.

A gate that selects and runs zero tests is a false green: assert a nonzero selected-test count before reading a
pass as a pass. See [test-integrity](../test-integrity/SKILL.md) for the full treatment of selector, double,
and teardown traps.

### Report the verification tier you actually reached

| Tier | What happened |
|---|---|
| 1 | Static read or parse check — the source was inspected, nothing executed |
| 2 | Interpreted or partial load — the code loaded but was not compiled or exercised |
| 3 | Real compile, load, and run of the relevant tests locally |
| 4 | The project's canonical gate green in CI, on a clean environment |

Name the tier achieved, and say plainly that a lower tier is not equivalent to a higher one *even when it found
real bugs*. Hand-tracing catches genuine defects and is worth doing; it is still not a compile-and-run
confirmation. Record the exact command the next session should run first to close the gap, so resuming is a
lookup rather than a reconstruction. Report which checks ran and which could not, with the reason — **a
silently omitted check reads as a passed check.**

The tiers genuinely differ: bugs surviving extensive local smoke testing are routinely caught only by a full
clean-environment run. Treat the gap between tiers as real risk, not bookkeeping.

## Code review

Four passes, in this order, because addressing style while functionality is broken wastes the review:

1. **Initial scan** — syntax, typos, missing imports, obvious logic errors, style violations.
2. **Deep analysis** — algorithm correctness, edge cases, error-handling completeness, resource management.
3. **Context** — breaking changes to public APIs, side effects on existing behavior, dependency compatibility.
4. **Standards** — naming, documentation, test coverage.

Evaluate across correctness, security (input validation, authn/authz, sanitization, secrets), performance
(algorithmic cost, resource usage, leaks, N+1), maintainability (naming, single responsibility, DRY), and
testability.

Categorize findings by what the reader must do: **critical** — security, data corruption, breaking changes,
must fix before merge; **important** — logic errors, missing error handling, performance, should fix;
**suggestion** — style, refactoring, documentation; **positive** — what was done well. Report summary, then
critical, important, suggestions, positives, and open questions. Every item carries a file:line and a concrete
change, never a direction to improve.

### Choose the lens deliberately

Convention-conformance review and behavior review are different reviews, **and the first will approve what the
second rejects.** A reviewer working from a conformance checklist systematically cannot catch a correctness
defect spelled like the convention: every box ticks, and the change ships with the bug the convention was meant
to prevent.

If the change's stated purpose is behavioral — performance, correctness, concurrency — a conformance pass is
not sufficient evidence. Never report a high conformance score as approval; state which lens produced it, so a
later reader does not treat a style pass as behavioral clearance. When two reviews of the same change disagree
sharply, the disagreement is usually a lens difference rather than a judgement difference — identify each lens
before trying to reconcile the verdicts.

### Staging in a shared checkout

1. Inspect status and the full diff before staging anything, using the plain non-decorated diff form so the
   output is parseable. A configured external differ — difftastic here — makes `git diff`, `git show`, and
   `git log -p` emit syntax-highlighted, restructured text instead of a parseable unified diff, silently: the
   command exits zero and the reader draws conclusions from decorated output rather than hitting an error. Pass
   `--no-ext-diff` to neutralize it before concluding a diff is empty or a change is missing.
2. If every hunk is cleanly attributable to your own work, stage only those.
3. If a shared file — an export list, a build manifest, a lockfile — carries changes interleaved with someone
   else's, **stop and ask.** Do not bundle them and do not split them speculatively; whose-work-is-it is not
   inferable from the diff.

Destructive shared-tree operations are off the table entirely; [core-patterns](../core-patterns/SKILL.md) holds
that list and the safe alternatives.

## Related

Naming a skill here does not load it. Invoke the Skill tool when the condition holds.

- [serena-usage](../serena-usage/SKILL.md) — before any memory check or symbol operation
- [investigation-patterns](../investigation-patterns/SKILL.md) — when review reveals behavior that is unclear
- [testing-patterns](../testing-patterns/SKILL.md) — when verifying coverage or designing a suite
- [test-integrity](../test-integrity/SKILL.md) — when a gate reports green
- [core-patterns](../core-patterns/SKILL.md) — for the refutation pass, and safe alternatives to destructive Git
