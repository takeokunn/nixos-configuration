---
argument-hint: [error-message]
description: Root cause investigation command
---

Diagnose a reported failure without fixing it. Files and memories are read-only. Apply CLAUDE.md's evidence, delegation, consensus, and output_contract.

## Establish the subject

Load investigation-patterns; add fact-check only for external contracts. Read task-matching memories, not an unrelated index sweep.

Before explaining source behavior, identify what actually ran: loaded module or binary path, daemon/build identity, and relevant timestamp or hash. Compare it with the current source/build, including caches, install prefixes, and containers. If they differ, stop and report the mismatch. A burst of failures after a small change is reason to inspect the harness before blaming the code.

Separate the observed symptom site from the proposed cause. Logs establish what occurred; source establishes what can occur. Classify syntax, runtime, logic, or configuration failure, and record the before/during/after timeline and whether the symptom is deterministic or conditional.

## Trace

1. Read the failing path, its dependencies/imports, effective configuration, and relevant recent diff. Follow registration before assigning a call site a production role. Prefer a generator over stale generated documentation.
2. Keep small investigations local. Delegate independent mechanism/hypothesis analysis, recurrence searches, or timeline/dependency tracing only when substantial. Every assignment is read-only.
3. Check external contracts against vendored source or version-matched official documentation. A remembered API contract is not evidence.
4. Reproduce with the smallest relevant probe. Distinguish a subject failure from a broken reproduction or unresolved setup. A failure before the target starts says nothing about its behavior; a changed error is not itself progress.
5. Narrow for at most three iterations. If the boundary remains unstable, repeat an identical probe once, state what was ruled out, and ask the user which remaining direction to pursue.

## Conclude

Use gate_discipline to connect symptom → mechanism → cause, with a citation and evidence tier for every link. An unsupported link remains a hypothesis. Call it a root cause only when observed reproduction, logs, or an end-to-end source trace establishes the chain.

Search the identified mechanism's recurrence scope and name every matching location; state exclusions. Resolve conflicting reports through the underlying evidence, not votes.

Return output_contract with:

- Subject: artifact identity, source match, and harness-side versus code-side evidence.
- Diagnosis: immediate cause, underlying cause, triggering conditions, and affected callers or other sites.
- Fix scope bracket: smallest sufficient change, largest justified change, constraints, and under-fix/over-fix risks.
- Recommended next action, without applying it, and the exact commands/results supporting the diagnosis.
- Remaining hypotheses, missing observations, and durable memory candidates for handoff.
