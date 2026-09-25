---
name: design
description: "Use when a task needs architecture evaluation, dependency and layer-violation checking, requirements decomposition, or effort estimation: circular dependencies, module boundaries, coupling and cohesion, ADRs, and where a new component belongs. Use proactively before implementation starts, not only in review, because a placement mistake costs more to correct than the code it holds."
---

Evaluate architecture, component placement, dependency boundaries, and requirements before implementation;
revisit placement when new dependencies appear. Apply the shared contracts in CLAUDE.md.

## Skills and constraints

- Load serena-usage for symbol/dependency or memory operations and ADR work; investigation-patterns for
  evidence-gathering investigations.
- Establish dependencies from references, not directory names. Report cycles with their edges even if no layer
  policy exists; call a layer violation only against a cited explicit policy.
- Never weaken a stricter security, verification, or fail-closed requirement to achieve consistency.
- Estimate in checkable tree units: files, callers, dependency depth, or test cases. Give the search behind each
  estimate and its largest unknown; do not invent hours.
- For cross-owner operations, identify the commit point, undo versus best-effort repair, retry idempotency, and
  indeterminate outcomes. An indeterminate result is not evidence of absence.
- Record ADRs only when warranted by project scale and authorized under memory_policy.

## Workflow

1. Map component hierarchy, imports, ownership boundaries, and existing ADRs. Separate observed structure from
   inferred conventions.
2. Trace dependency edges, cycles, wrong-layer imports against stated policy, and fan-in/fan-out. Identify
   untraced components rather than extending conclusions to them.
3. Define functional and nonfunctional requirements, actors, goals, flows, observable acceptance conditions, and
   a task/dependency graph. Surface ambiguities that change implementation.
4. Propose placement and a bounded change sequence. Compare viable alternatives using the same evidence.
   When both match existing patterns, state the facts that would distinguish them instead of inventing a winner.
5. Follow gate_discipline before implementation: name the files and edges supporting the design, acceptance
   conditions, required changes, uncertainties, and technical, organizational, and quality risks.
6. Estimate the scope in tree units. If using past work as a comparison, cite its revision and paths and explain
   how its units map to this task. State what remains unknown.

## Escalation and output

Report cycle impact, the policy behind each layer violation, and staged steps for high-risk changes.
Clarify requirements when choosing an interpretation would change the work.

Use output_contract. Include the dependency map, findings with locations and evidence tiers, requirements and
acceptance conditions, proposed placement and alternatives, scoped estimates with derivation and unknowns,
risks, and next_actions.
