## Additive Guidance

- This appendix is additive only. Preserve the built-in role, permissions, workflow, and any machine-consumed plan, todo, or control syntax.
- Human-facing documentation should default to Japanese, but preserve the established document language when editing existing files.

## Compact Execution Board

For any non-trivial task, begin with a compact execution board before acting. Keep it short and operational.

- Task DAG or dependency summary
- Current parallel batches
- Agent roster
- Merge/conflict plan

## True Parallel Delegation

Treat parallel delegation strictly.

- Each dependency-free sibling task must map to a DISTINCT subagent instance.
- When multiple dependency-free sibling tasks are ready at once, prefer launching them in the same wave so long as their write paths are isolated or their writes can be sequenced safely. This remains true parallel execution even when `run_in_background=false`.
- Reusing one subagent instance or one session across multiple sibling tasks does NOT count as parallel delegation.
- Parallel tool calls inside a single agent do NOT count as multi-agent delegation.
- Do not serialize independent sibling tasks through one general worker and call that parallel execution. If true multi-agent concurrency is unavailable at runtime, state that limitation explicitly and do not claim that delegation was parallelized.

## run_in_background Discipline

Always specify `run_in_background` explicitly on every task-dispatch call.

- Use `run_in_background=true` only for read-only information-gathering fan-out.
- Use `run_in_background=false` for implementation, review, verification, or consultation work.

## Direct-Orchestration Packet Discipline (Additive)

- When working directly with the user, do not forward full chat history by default. Keep continuation payloads as compact handoff records with must-carry facts and exact refs; when the same state can be preserved in that form, prefer it over forwarding raw chat history.
- Before delegating or spawning support agents, reduce the work to:
  - objective
  - exact refs
  - non-negotiable constraints
  - explicit non-goals
  - the smallest background needed to preserve intent
- Treat the current repo state plus curated refs as the authority. If the packet still depends on unwritten chat context, tighten it before delegating.
- Preserve one primary intent per delegated task context. A single goal may require multiple files or multiple steps.
- If context starts to accumulate across waves, preserve only:
  - essential purpose
  - must-carry background and decision history
  - completed work
  - changed files and symbols
  - unresolved questions or risks
  - exact artifacts to read next
- If required verification or closeout details do not fit cleanly, split into another wave instead of dropping them.
- This rule is advisory and must not alter built-in role boundaries, machine-consumed syntax, or existing delegation/run_in_background semantics.

## Write Coordination

- If overlapping writes may occur, do parallel read/analyze first and apply edits sequentially unless isolation exists.
- Isolation means a worktree, branch, sandbox, or another mechanism that prevents write conflicts.

## Existing Diff Safety

- Treat pre-existing diffs in the working tree as protected context, not as cleanup targets.
- Do not use `git restore`, `git checkout`, `git reset`, `git clean`, undo, or revert just to create a clean starting point.
- When unrelated diffs are present, ignore them and continue implementing as long as your work can proceed without modifying them and without relying on them to infer the current spec.
- Only ask the user when existing diffs materially affect the required implementation, such as changing the effective spec, API contract, data shape, or behavior you must integrate with.
- Do not treat all remaining changes in the tree as belonging to the current task by default.
