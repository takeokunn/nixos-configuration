---
argument-hint: [question]
description: Question and inquiry command
---

Answer a repository question from evidence. This command is read-only, including memories: do not implement recommendations or persist findings. Apply CLAUDE.md's evidence, delegation, consensus, and output_contract.

## Investigate

1. Restate the question only if needed to fix its scope. Locate the code or configuration that can answer it.
2. Read only domain-matching memories, checking their claims against the current ref. Load serena-usage for memory or symbol work, investigation-patterns for hypothesis testing, and fact-check for external claims. A direct lookup needs neither research workflow.
3. Handle bounded lookups directly. Delegate substantial independent questions, read-only: the runtime's exploration agent for execution paths, design for boundaries, performance for measured cost, quality-assurance for behavior, or code-quality for complexity.
4. Cite the source governing the behavior. Generated documentation may be stale; inspect its generator. A call site proves presence, not a production role. Check registration before claiming absence. Verify external contracts against the installed version's vendored source or official documentation.
5. Resolve contradictions by the evidence each claim examined, not by agreement. If evidence contradicts the user's premise, name both plainly. Dispatch the verification agent in its read-only reconcile mode only for a consequential claim still disputed after rereading.

## Answer

Discharge gate_discipline with file:line or command evidence for each consequential claim. Tag individual claims, not whole files. Every delegated question needs a usable result or an explicit gap; an unanswered question is not a negative finding.

Return output_contract with a direct answer, supporting evidence, and any recommendation clearly separate from implementation. Downgrade unsupported claims and name unanswered questions. Research further where possible; ask only when the user holds the missing fact.

Confirm the read-only boundary was kept. Hand off durable memory candidates with the topic search needed to avoid duplicates; do not write them.
