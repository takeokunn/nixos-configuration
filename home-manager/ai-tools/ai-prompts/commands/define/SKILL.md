---
argument-hint: [message]
description: Turn a request into an implementation-ready specification. Use when scope is unresolved before implementation.
---

Turn a request into an implementation-ready specification. This command is read-only: do not edit files, write memories, or implement; return memory candidates instead (see CLAUDE.md memory_policy).

Use this command for ambiguous scope, meaningful alternatives, cross-file impact, or high-risk decisions. A factual question or a diagnosis belongs in /ask, and understood implementation in /execute.

## Method

Load requirements-definition, plus fact-check when external claims matter. Follow prepare → analyze → investigate → clarify → verify → document; this command's read-only boundary governs any broader skill permissions.

1. Separate the user's problem from their proposed solution. Identify subject, object, and operation. Establish Why, How, and What at the relevant levels: system/cross-cutting, data, interfaces, business behavior, then affected files.
2. Verify current capabilities at the target ref before promising feasibility. State an impossible requirement plainly.
3. Specify observable behavior, options, constraints, and load-bearing decisions, not invented function names or algorithms.
4. Prefer the minimum scope meeting the need. Compare simpler alternatives and phased delivery. Require three evidenced cases before generalizing a one-off need. Trace the full impact, including callers, migrations, API compatibility, and tests.
5. Delegate substantial independent exploration, architecture, or data questions read-only (see CLAUDE.md delegation). Reconcile an unresolved consequential disagreement (see the execution-workflow skill). Synthesize results rather than forwarding agent reports.
6. Record decision options with a recommended choice and concrete tradeoffs in the specification.

### Questions

Ask the user about every design branch, meaning any decision that changes the specification; this command is the exception to CLAUDE.md work_selection. Use the runtime's question tool when available and put the recommended option first. Investigate unknowns the repository can answer instead of asking. Record each answer in the specification. A branch the user leaves open goes under Outstanding issues, and tasks depending on it are marked blocked.

### Signals to resolve

| Request signal | Establish |
|---|---|
| Proposed solution | Underlying problem and simpler alternatives |
| Desired behavior | Owner, threshold, and constraints |
| Regression | When it changed and other affected sites |
| “All” | Enumerated search scope and exclusions |
| Implied new capability | Dependencies, cost, and available primitives |
| “Just” or “simple” | Downstream callers, migrations, APIs, and tests |

## Specification

Include what the implementation needs and omit empty sections:

- Summary: problem, intended outcomes, and why they matter.
- Current state: verified capabilities and limitations.
- Functional requirements: stable identifiers such as FR-001, mandatory versus optional, and observable behavior.
- Technical specification: policy, affected surfaces, decisions, rationale, and rejected alternatives.
- Constraints.
- Test requirements: observable acceptance conditions, including failure behavior.
- Outstanding issues: priority, conflicting evidence or positions, and open design branches; omit when none remain.
- Task breakdown: phases, affected files, dependencies, references, and constraints. Do not hide unknowns inside implementation tasks.

Add nonfunctional requirements only when applicable, an architecture diagram for meaningful multilayer relationships, and data/schema/API contract changes when relevant.

## Check before returning

Check that scope and feasibility are grounded, blast radius and a simpler alternative were considered, acceptance is observable, and each task is ready or names its blocker. Keep decision points that affect implementation; omit obvious restatements.

Reply in plain prose in the output style's form, with the specification and any design branches still open.
