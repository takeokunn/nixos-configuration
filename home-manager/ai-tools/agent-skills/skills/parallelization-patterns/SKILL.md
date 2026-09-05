---
name: parallelization-patterns
description: Patterns for parallel execution (deciding what is genuinely independent, settling disagreement between concurrent agents, retry policy, and scheduling data-parallel work across skewed inputs).
version: 4.0.0
---

What can run at once, and what to do with the results.

This file previously carried four `<parallelization>` capability templates and a timeout tier table. Both were
removed: no agent or command in the corpus ever contained a `<parallelization>` block, and nothing read
`timeout_per_agent` or `max_parallel_agents`: they were numbers with no mechanism behind them. Concurrency
limits are set by the harness; a per-call `timeout` is set where the call is made, sized to that command.

## Independence

Classify by what an agent touches, not by what it is called:

- **Read-only**: safe with anything.
- **Analysis**: reads and reasons; safe with other analysis agents.
- **Write**: modifies local state; needs coordination or its own worktree.
- **Orchestration**: manages sub-agents; owns the partition.

**Independence is stricter than non-overlapping files.** A change that must land atomically across several
files is one task however many files it touches, and two agents whose edits are each individually valid can
still produce a tree that satisfies neither. Where one file is shared and the others are not, edit the shared
one first, then fan out.

Never request a timing measurement from an agent running concurrently with others: parallel load invalidates
it. Tell concurrent agents to keep scratch artifacts inside their own worktree: a fixed path outside the
repository collides silently.

## Retry

At most two retries, and only when the agent timed out or died without returning, answered some questions but
not all, or returned findings with no file:line and no command output.

Retry once with a narrower prompt naming the specific files. If it fails again, do the work in the orchestrator
and report that the delegation failed: **never present an unanswered question as an absence of findings.**

Before treating silence as death, check the subagent transcript: a lost completion notification is common and
the report is usually intact. An agent that errored mid-task may have left partial writes, so inspect the tree
before re-dispatching a write-capable agent.

## What a multi-agent result means

Decide by the shape of the agreement, not by a fraction. **Agreement between agents that read the same file is
one observation, not several**: counting agreeing agents measures redundancy, not truth.

| Shape | What to do |
|---|---|
| Agreed, and at least one cites a file:line or command output | Accept and report |
| Agreed, but none cites anything checkable | Accept with the gap named: report it inferred, not verified, and say what would confirm it. Unanimity among agents reasoning from the same naming convention is not evidence |
| Split | Resolve by what each examined; if still unresolved, present both positions with their evidence |
| One agent reports data loss, credential exposure, or a destructive operation | Act on it regardless of the count. Investigate before proceeding, even against a majority |

This replaced numeric agreement thresholds. Nothing computed the fraction, and **the interesting distinction is
not how many agents agreed but whether anyone actually looked.**

### Settling a disagreement

1. An agent citing a file:line, a command it ran, or that command's output **outranks** one reasoning from
   naming, convention, or plausibility, whatever their specialties.
2. Within its own domain a specialist outranks a generalist on *interpretation*: what the observed evidence
   means for security, for schema design, for performance. It does not outrank anyone on what the evidence
   *says*.
3. A blocking finding is acted on even if only one agent raised it. Being outnumbered is not disconfirmation:
   the cost of checking is small and the cost of ignoring is not.
4. If both sides cite concrete evidence and still disagree, they are answering different questions or one read
   stale state. Re-read the disputed location yourself before choosing.
5. Report an unresolved disagreement with both positions and what each rests on. Never silently pick one and
   present it as settled.

This replaced a numeric weight per agent feeding a weighted majority. Nothing computed those weights, and their
actual effect ("security outranks docs") is stated directly here in a form that can be applied.

## Scheduling skewed data-parallel work

Distinct from the agent-level patterns above. Static contiguous chunking assumes work per item is roughly
uniform; when it is skewed, **one oversized item strands most workers idle** while a single worker finishes it.

1. Sort work units by descending estimated size, so the longest job starts first and short ones backfill around
   it.
2. Hand out units through a shared atomic cursor rather than pre-assigning ranges. Each worker claims the next
   index when free, so a slow unit delays only its own worker.
3. Have each worker write its result into the slot for the unit's **original index**, claimed at the same time
   as the work.
4. Read results back by index after all workers join: never by claim order or completion order.

**Steps 3 and 4 are the part implementations get wrong.** Deterministic output ordering was a free, accidental
property of contiguous chunking, where a chunk's slot range equalled its input position. Size-descending
claiming destroys that correspondence, so output order has to be re-established deliberately through
pre-claimed index slots. Skipping this produces output whose order varies run to run: a change that looks
unrelated to scheduling and is easy to misdiagnose.

On a size-skewed workload this is a large win; on an even workload it is neutral rather than a regression,
because claiming overhead is small relative to per-unit work. That makes it a strict improvement rather than a
tradeoff, **but verify the neutral case rather than assuming it**, and confirm the output is byte-identical
either way. See [performance-benchmarking](../performance-benchmarking/SKILL.md) for how to measure both arms
defensibly.

Not worth it for work units of genuinely uniform cost, or units so small that the atomic claim dominates the
work itself.

## Related

- [core-patterns](../core-patterns/SKILL.md): decision criteria and the evidence tiers used above
- [workflow-patterns](../workflow-patterns/SKILL.md): output formats and checkpoint structure
- [execution-workflow](../execution-workflow/SKILL.md): where the partition is written and dispatched
- [performance-benchmarking](../performance-benchmarking/SKILL.md): measuring a scheduling change defensibly
