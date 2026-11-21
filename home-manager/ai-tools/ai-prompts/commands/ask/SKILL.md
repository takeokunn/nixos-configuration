---
argument-hint: [question]
description: Answer a question about the repository or diagnose a reported failure from evidence. Use when the user asks how or why something works or wants a root cause; read-only.
---

Answer a repository question or diagnose a failure from evidence. This command is read-only: do not edit files, apply fixes, or write memories; return memory candidates instead (see CLAUDE.md memory_policy).

## Investigate

1. Restate the question only if needed to fix its scope. Locate the code or configuration that can answer it.
2. Read only domain-matching memories, checking their claims against the current ref. Load investigation-patterns for hypothesis testing and tracing, and fact-check for external contracts. A direct lookup needs none of them.
3. Handle bounded lookups directly. Delegate substantial independent questions read-only (see CLAUDE.md delegation): the runtime's exploration agent for execution paths, design for boundaries, performance for measured cost, quality-assurance for behavior, or code-quality for complexity.
4. Cite the source governing the behavior. Before concluding absence or a production role, trace registration (see the investigation-patterns skill).
5. Resolve contradictions by the evidence each claim examined, not by agreement. If evidence contradicts the user's premise, name both. Reconcile a consequential claim still disputed after rereading (see the execution-workflow skill).

## Diagnose a failure

1. Before explaining source behavior, identify what actually ran (loaded module or binary path, build identity, timestamp or hash) and compare it with the current source, including caches, install prefixes, and containers. If they differ, report the mismatch and stop.
2. Reproduce with the smallest relevant probe. Distinguish a failure of the subject from a broken reproduction or unresolved setup.
3. Connect symptom, mechanism, and cause, citing each link. Call it the root cause only when an observed reproduction, logs, or an end-to-end source trace establishes the chain; otherwise it remains a hypothesis.
4. Name every other site of the same mechanism. Recommend the smallest sufficient fix and its risks without applying it.

## Answer

Reply in plain prose in the output style's form: the direct answer, file:line or command evidence for each consequential claim, and any recommendation kept apart from implementation. A delegated question without a usable result is a gap, not a negative finding.
