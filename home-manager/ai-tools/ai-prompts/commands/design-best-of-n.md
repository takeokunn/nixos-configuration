---
argument-hint: "[instruction1, instruction2, ...] | apply [worktree-path]"
description: Race N candidate implementations in parallel worktrees; you pick, one /design-sync on apply
---

Generate isolated design candidates for the user to compare, or apply one explicitly selected candidate. Never rank candidates or synchronize during generation. Apply CLAUDE.md's hard_rules, delegation, evidence, and output_contract.

## Preconditions

Before creating any worktree:

1. Verify the installed runtime supports the required claude-design tools and /design-sync, including the actual invocation, arguments, and target selection. A slash-command name in prompt text is not capability evidence.
2. Read CLI help and effective permissions/hooks for the intended headless invocation. Do not disable a gate to make it headless; stop if it requires approval the runtime cannot obtain.
3. Establish current-request authorization for Git writes, including candidate branches/worktrees. Follow execution-workflow isolation, never repurpose a shared tree or write on the default branch.
4. Disclose that generation starts N separate sessions. Ask when candidate count or usage budget is ambiguous; do not invent cost estimates.

Treat instructions as opaque input: pass them in a quoted variable/argument or a file inside the candidate worktree, never through eval or shell re-parsing. A worktree isolates files, not remote state.

## Select mode

- A comma-separated instruction list means generate one candidate per item, including repeated instructions. Ask if embedded commas make the split ambiguous.
- `apply <worktree-path>` means apply a candidate recorded by this command's earlier run. Do not accept an arbitrary directory as a candidate.

## Generate

1. Record baseline HEAD and disclose dirty changes excluded from that baseline.
2. Create unique isolated candidate branches/worktrees from that HEAD through execution-workflow. Never overwrite an existing path or branch. Establish each session's active project root.
3. Run candidates concurrently within local capacity. Each session owns only its candidate worktree and must not synchronize designs, mutate remote state, or perform additional Git writes. State those restrictions separately from the opaque task text.
4. Capture stdout, stderr, start/exit state, and process handles in unique logs inside each candidate worktree. Use an explicit timeout or tracked background process; on macOS use the available coreutils timeout through Nix when needed. Wait for every candidate to finish or fail.
5. Inspect each candidate's tracked diff and untracked files, not just its logs. At least one candidate must exit successfully and produce a nonempty change. All failures or unchanged candidates constitute a failed generation, not a successful empty comparison.
6. Present every candidate's path, instruction, exit status, changed files, and logs. Provide access to full diffs, new-file contents, and logs, not only a diffstat or truncated tail. Report failures alongside successes without ranking or recommending a candidate.

Keep all candidate worktrees. Ask the user to select one; cleanup requires a separate request.

## Apply

1. Match the requested path to the recorded candidate. Verify it exists and inspect its full changes against the recorded baseline, including committed changes and untracked files. Stop if the candidate cannot be identified.
2. Confirm the selected candidate and remote design project before synchronization. Separately confirm any local integration operation and its current Git-write authority.
3. Invoke the verified design-sync operation exactly once for the selected candidate, then perform only the confirmed local integration. There may be at most one synchronization in this command run.
4. If an external operation fails partway, establish what already changed and report it; do not automatically retry or duplicate remote writes.

Preserve unselected worktrees. Return output_contract identifying the mode, verified capabilities, candidate results or applied target, checks, and remaining gaps.
