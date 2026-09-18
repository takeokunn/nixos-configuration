---
name: parallelization-patterns
description: Patterns for parallel execution (deciding what is genuinely independent, settling disagreement between concurrent agents, retry policy, and scheduling data-parallel work across skewed inputs).
metadata:
  version: "4.0.0"
---

What can run at once, and what to do with the results.

Concurrency limits are set by the harness. Where the tool supports a per-call timeout, size it to the command;
numbers written in a prompt do not configure the runtime.

## Independence

Classify by what an agent touches, not by what it is called:

- **Read-only**: parallelize when inputs are stable; check shared project pointers and diagnostic caches.
- **Analysis**: reads and reasons; record the revision or snapshot when concurrent edits can change inputs.
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

Narrow each retry to the unanswered question and specific files. If both retries fail, do the work in the orchestrator
and report that the delegation failed: **never present an unanswered question as an absence of findings.**

Before treating silence as failure, check runtime status and messages, then transcripts if the runtime exposes
them. Follow serena-usage's recovery procedure; do not assume Claude transcript paths exist in another runtime.
An agent that errored mid-task may have left partial writes, so inspect the tree before re-dispatching a
write-capable agent.

## What a multi-agent result means

Decide by the shape of the agreement, not by a fraction. **Agreement between agents that read the same file is
one observation, not several**: counting agreeing agents measures redundancy, not truth.

| Shape | What to do |
|---|---|
| Agreed, and at least one cites a file:line or command output | Accept and report |
| Agreed, but none cites anything checkable | Mark it assumed and name the missing check; use inferred only when the conclusion follows from cited verified premises. Agreement alone is not evidence |
| Split | Resolve by what each examined; if still unresolved, present both positions with their evidence |
| One agent reports data loss, credential exposure, or a destructive operation | Act on it regardless of the count. Investigate before proceeding, even against a majority |

### Settling a disagreement

1. An agent citing a file:line, a command it ran, or that command's output **outranks** one reasoning from
   naming, convention, or plausibility, whatever their specialties.
2. Compare interpretations by their premises, version, and lifecycle context. A specialist's title does not
   override another agent's stronger evidence.
3. A blocking finding is acted on even if only one agent raised it. Being outnumbered is not disconfirmation:
   the cost of checking is small and the cost of ignoring is not.
4. If both sides cite concrete evidence and still disagree, they are answering different questions or one read
   stale state. Re-read the disputed location yourself before choosing.
5. Report an unresolved disagreement with both positions and what each rests on. Never silently pick one and
   present it as settled.

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

Measure skewed and uniform workloads: atomic claiming and sorting add overhead, so neither improvement nor
neutrality is guaranteed. Confirm equivalent output in both arms. See
[performance-benchmarking](../performance-benchmarking/SKILL.md) for the measurement protocol.

Not worth it for work units of genuinely uniform cost, or units so small that the atomic claim dominates the
work itself.

## Related

- [core-patterns](../core-patterns/SKILL.md): decision criteria and the evidence tiers used above
- [workflow-patterns](../workflow-patterns/SKILL.md): output formats and checkpoint structure
- [execution-workflow](../execution-workflow/SKILL.md): where the partition is written and dispatched
- [performance-benchmarking](../performance-benchmarking/SKILL.md): measuring a scheduling change defensibly
