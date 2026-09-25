---
argument-hint: [message]
description: Requirements definition command
---

Turn a request into an implementation-ready specification for the user to approve. Do not edit files, write memories, or implement. Apply CLAUDE.md's evidence, delegation, gate_discipline, and output_contract.

Use this command for ambiguous scope, meaningful alternatives, cross-file impact, or high-risk decisions. A factual question belongs in /ask, a diagnosis in /bug, understood implementation in /execute, and saving the resulting document in /markdown.

## Method

Load define-core and requirements-definition, plus fact-check when external claims matter. Follow prepare → analyze → investigate → clarify → verify → document → finalize; this command's read-only boundary governs any broader skill permissions.

1. Separate the user's problem from their proposed solution. Identify subject, object, and operation. Establish Why, How, and What at the relevant levels: system/cross-cutting, data, interfaces, business behavior, then affected files.
2. Verify current capabilities at the target ref before promising feasibility. State an impossible requirement plainly. Investigate a signal, form a hypothesis, and check it before asking a question the repository can answer.
3. Specify observable behavior, options, constraints, and load-bearing decisions, not invented function names or algorithms. Ask when materially different interpretations change the work; do not impose an artificial question budget.
4. Prefer the minimum scope meeting the need. Compare simpler alternatives and phased delivery. Require three evidenced cases before generalizing a one-off need. Trace the full impact, including callers, migrations, API compatibility, and tests.
5. Delegate substantial independent exploration, architecture, or data questions read-only when useful. Use a validator only for an unresolved consequential disagreement. Synthesize results rather than forwarding agent reports.
6. Offer decision options with a recommended choice and concrete tradeoffs. Replace confidence percentages or effort-hour estimates with observed files, call sites, layers, and test obligations.

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

Always include:

- Summary: problem, intended outcomes, and why they matter.
- Current state: verified capabilities and limitations.
- Functional requirements: stable identifiers such as FR-001, mandatory versus optional, and observable behavior.
- Technical specification: policy, affected surfaces, decisions, rationale, and rejected alternatives.
- Constraints.
- Test requirements: observable acceptance conditions, including failure behavior.
- Outstanding issues: priority, conflicting evidence or positions, and the user decision needed; say explicitly when none remain.
- Task breakdown: phases, affected files, dependencies, references, and constraints. Do not hide unknowns inside implementation tasks.

Add nonfunctional requirements only when applicable, an architecture diagram for meaningful multilayer relationships, and data/schema/API contract changes when relevant.

## Finalize

Check that scope and feasibility are grounded, blast radius and a simpler alternative were considered, acceptance is observable, and each task is ready or names its blocker. Keep decision points that affect implementation; omit obvious restatements.

Resolve critical questions or list them explicitly. With outstanding issues, invoke the finalize step to ask the user to resolve, defer, or stop; do not silently treat the specification as approved. Return output_contract with the specification and unresolved decisions. Hand off memory candidates without writing them.
