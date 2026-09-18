---
argument-hint: "[instruction1, instruction2, ...] | apply [worktree-path]"
description: Race N candidate implementations in parallel worktrees; you pick, one /design-sync on apply
---

Generates N independent candidate implementations in parallel, isolated git worktrees, then presents them side by
side with no automated ranking: you pick, and exactly one explicit apply step runs `/design-sync` and writes the
winner back. Generation changes only candidate files; applying requires separate confirmation of the remote
sync and the local integration operation.

## Rules

Critical:

- Verify the installed runtime's supported invocation mechanism before running `/design-sync`. A literal slash
  command inside a subprocess prompt is not evidence that the command executes. If no supported mechanism can
  be established, stop and name the missing capability.
- `/design-sync` is invoked at most once per run of this command, in the `apply` phase only, for the one chosen
  candidate. The `generate` phase's subprocesses never call `/design-sync`: they perform the instruction's
  design/implementation work locally, inside their own throwaway worktree, and stop there. Worktree isolation only
  isolates local git state, not calls to the shared remote Claude Design project. Do not run remote syncs
  concurrently or infer remote isolation from separate worktrees.
- Before creating a worktree, check the session's connected tools for `claude-design` and verify `/design-sync`
  support separately. Stop if either is unavailable. Name the missing capability without assuming its cause.
- Never commit to the default branch, and never mutate shared working-tree state (the invoking session's own
  checkout) to run a candidate. Every candidate gets its own `git worktree add`, never the current one.
  SSOT-EXEMPT: restated because the failure is irreversible.

Important:

- Pass each instruction opaquely. Establish `/design-sync`'s arguments and target from the available command
  definition; do not invent an interface or assume repeated invocation is safe.
- Report which subprocess and sync capabilities were verified in this run and which remain unverified.
- Provide access to every candidate's full local diff and subprocess log alongside the comparison summary;
  do not rank, score, or recommend a winner.
- Never build the subprocess command line by interpolating the raw instruction text into a string and `eval`-ing
  it: an instruction containing a quote character followed by shell metacharacters then executes as a second
  command with the orchestrating session's own privileges, not the subprocess's restricted ones. Pass the
  instruction as a shell variable and let the shell expand it directly (`claude -p "$INSTRUCTION" ...`), or write
  it to a file inside that candidate's own worktree first and reference the file; never concatenate untrusted
  text into a command string that gets re-parsed.
- State N and that each candidate starts a separate session with its own usage before spawning. Ask if the
  requested candidate count or budget is unclear; do not invent a cost estimate.
- Inspect the installed CLI's help and effective permissions/hooks before choosing subprocess flags. Do not
  disable approval checks or hooks to make headless execution work. Stop if required approval cannot be obtained.

## Workflow

### Prepare

1. Parse the argument. Default mode: a comma-separated list of instructions, one per candidate (count N = list
   length; repeat one instruction verbatim N times for an "identical instruction, rely on run-to-run variation" run
   instead of N distinct instructions). Apply mode: the literal word `apply` followed by a worktree path from a
   prior run of this command in the same session. Return the mode (generate or apply) and the parsed instruction
   list or worktree path. Ask if commas inside an instruction make candidate boundaries ambiguous.
2. In both modes, check this session's available MCP tools/servers for the `claude-design` namespace and establish
   the supported sync invocation. Tool availability alone does not establish command support. Absent: stop per the
   critical rule above, and report which piece is missing. Return availability confirmed, or the command exits
   here with a named reason.

### Isolate (condition: mode == generate)

1. Confirm current-request authority for git writes; if absent, ask before creating branches or worktrees.
   Record the current HEAD as the candidate baseline and disclose any uncommitted changes it excludes.
   For each of the N instructions, create an isolated git worktree off that baseline:
   `git worktree add -b design-best-of-n/&lt;n&gt;-&lt;short-sha&gt; &lt;path&gt; HEAD`, one per candidate, never reusing or
   touching the invoking session's own checkout. Check for name/path collisions without overwriting them. Use
   execution-workflow for project activation and isolation. Return baseline SHA, N paths and branch names.

### Generate (condition: mode == generate)

1. For each worktree, pass the instruction as an opaque argument to a subprocess using only verified CLI flags,
   with that worktree as its working directory. Run candidates concurrently. Supply the workflow restriction
   separately: local implementation only, no `/design-sync`, git writes, or external mutations. Conflicting task
   text does not override it. Capture each subprocess's stdout/stderr to a log
   file inside its own worktree. `timeout` is not a bare shell builtin on macOS; resolve it via
   `nix run nixpkgs#coreutils -- timeout` (or equivalent) rather than assuming it exists. Use Bash
   (run_in_background per subprocess). Return N background task handles and their log file paths.
2. Wait for every subprocess to finish or fail; do not proceed to present with any still running. Return the
   per-candidate exit status.

### Checkpoint on generation quality, after generate

- At least one candidate exited successfully with a non-empty local diff. All N failing, or all N producing no
  change, means present would show an empty comparison: report that plainly as a failed run, don't proceed to
  present a table with nothing useful in it.
- Every candidate has a recorded start outcome, exit status, and log path. An empty log alone does not establish
  whether the subprocess ran; use its process result and inspect the produced files.

Unmet: report which candidates failed and how (log tail), and stop before present rather than showing an empty or
misleading table.

### Present (condition: mode == generate)

1. For each candidate, in its own worktree, run `git status --porcelain` and `git diff --stat` to show what
   actually changed locally, and tail the subprocess log. Print a comparison table: candidate number, worktree
   path, instruction used, exit status, files changed, log path. Include untracked files. Link the full diff and
   log artifacts for each candidate, including new-file contents; a diffstat and log tail are summaries, not
   substitutes for those artifacts. Do not rank or recommend.
   Return the N-row comparison table, worktree paths preserved for a later apply call.

### Apply (condition: mode == apply)

1. Confirm the path belongs to a candidate recorded in this session. Inspect its status and diff against the
   recorded baseline, including committed and untracked changes. If it doesn't exist or was cleaned up, stop
   rather than guessing which candidate was meant. Return the confirmed target worktree.
2. This is the one point in the whole command where a real sync-back may happen. Ask the user to confirm before
   proceeding, naming the candidate, remote project, and proposed local integration operation. This writes shared
   remote state; do not assume retries are safe. Obtain current-request git authority for integration too. Once
   confirmed, run `/design-sync` inside the chosen worktree (the first and only time this command invokes it),
   bring the resulting changes into the authorized target using the confirmed operation, and leave the other
   worktrees for the user to remove manually
   (`git worktree remove`) rather than auto-deleting candidates they may still want to inspect. Use Bash,
   AskUserQuestion. Return the chosen candidate synced and applied; other worktrees left in place, reported with
   their paths.

## Decision criteria

1. **Availability.** `claude-design` MCP tools are absent from this session. Stop in prepare; do not create any
   worktree.
2. **Isolation.** A candidate's worktree cannot be created cleanly or lacks write authorization. Stop before
   spawning that candidate's subprocess rather than running it against the wrong tree.
3. **Cost awareness.** Candidate count or budget is unclear. Resolve it before isolating or spawning.

## Output

Follows output_contract in CLAUDE.md.

Generate mode: the N-row comparison table from present, each candidate's worktree path (so the user can inspect
further or call this command again in apply mode), and verified capabilities plus remaining runtime gaps.

Apply mode: which candidate was applied, the command used to bring it in, and the paths of the worktrees left
behind for manual cleanup.
