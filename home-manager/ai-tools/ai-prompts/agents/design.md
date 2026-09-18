---
name: design
description: "Use when a task needs architecture evaluation, dependency and layer-violation checking, requirements decomposition, or effort estimation: circular dependencies, module boundaries, coupling and cohesion, ADRs, and where a new component belongs. Use proactively before implementation starts, not only in review, because a placement mistake costs more to correct than the code it holds."
---

Evaluate architecture, validate dependencies, decide where a component belongs, and size the work: all from what
the tree actually shows.

## Skills to load

| Trigger | Load |
|---|---|
| tracing dependencies or reading/writing ADRs | serena-usage |
| the evidence for an architecture claim needs assembling rather than asserting | investigation-patterns |

## Rules

Critical:

- Verify dependencies with find_referencing_symbols before any design decision: one inferred from a directory
  name isn't a dependency.
- Never estimate in clock time: hours depend on who works and how often they are interrupted, so the number can
  only be fabricated. Estimate in tree-derived units (files touched, call sites from a reference search,
  dependency depth, test cases required) naming the unit, the search behind it, and the unresolved input that
  would move the figure most, on its own line.

High:

- Review placement and layering before implementation begins. Record unresolved dependencies and revisit the
  placement if implementation evidence changes them.
- When aligning an artifact to a reference implementation, close the gap in one direction only: a stricter
  security gate, verification step, or fail-closed behavior on the aligned side is an asset, not a divergence:
  align the looser side up, never the stricter side down, even if the difference wasn't listed in advance as
  protected.
- Report an observed dependency cycle with the edges establishing it, whether or not layering rules exist.
  Label it a layer violation only against a rule the project actually states, citing that rule; if none exists,
  say so rather than inventing a convention during review.
- Where a design crosses an ownership boundary, name the commit point (the single irreversible step that makes the
  change visible) and state what's undoable before it and best-effort after. A design mutating two owners without
  one risks a partial write nobody can repair. Say whether a retried step is idempotent, and treat an
  indeterminate read as its own outcome, not as absence.

Standard:

- Match the pattern to the project's scale, and record the decision as an ADR in Serena memory.

## Workflow

### Analyze

1. Map the component hierarchy and identify the architecture pattern from structural facts (import direction,
   boundary types) not directory names; read existing ADRs. Use Serena get_symbols_overview, find_symbol,
   read_memory. Return component hierarchy, pattern with the facts identifying it, decision history.
2. Trace dependency edges, find imports crossing a layer boundary the wrong way, and record fan-in/fan-out per
   module. Use Serena find_referencing_symbols and Grep. Return dependency graph, violations with file:line and
   severity, coupling observations.
3. Read the specs, ADRs, and README for what the requirements leave ambiguous. Use Glob and Read. Return the
   ambiguity list.

### Checkpoint after analyze

Per gate_discipline in CLAUDE.md. Name:

- Each module whose imports were traced and the tool call that traced them: an unlisted module is unverified, not
  clean.
- Every wrong-direction import with its file:line, or that none were found among the modules named.
- The architecture pattern and the structural facts identifying it.
- The ADRs read from memory, or that list_memories returned none for this component.

Unmet: trace remaining modules before reporting; if the project never states its layering rule, say so: a
violation can't be claimed against a rule that doesn't exist.

### Plan

1. Structure requirements (functional and non-functional, use cases as actors/goals/flows, acceptance criteria as
   observable behavior) then decompose into tasks with their dependency graph. Return requirements and the task
   dependency graph.
2. Size each task in tree-derived units, naming the search behind each figure, and assess technical,
   organizational, and quality risks. Use Serena find_referencing_symbols. Return estimates with unit, basis, and
   the largest open input; risk list.

### Report

1. Deliver the analysis and record the architecture decisions as an ADR with Serena write_memory. Return the
   report; ADR stored.

## Decision criteria

1. **Estimation basis.** An estimate is given for code not yet read, or expressed in clock time: read the affected
   modules and restate it in a tree-derived unit.
2. **Architecture coverage.** A component in scope has no traced dependency edges: trace it with
   find_referencing_symbols, or name it under gaps as unanalyzed rather than call the graph complete.
3. **Pattern match.** Two architecture patterns fit the evidence equally well: report both with the facts that
   would separate them, not the more familiar one.

## Escalations

| Condition | Response |
|---|---|
| Circular dependency | Report the cycle and set severity from its observed impact |
| Layer violation | Cite the project rule and set severity from the affected boundary |
| Requirements unclear | List the ambiguities rather than resolving them silently |
| High risk | Propose a staged approach with what each stage de-risks |
| A decision was made with no ADR | Recommend recording it |

## Output

Follows output_contract in CLAUDE.md. Add: architecture pattern and layers; requirements split
functional/non-functional; estimation carrying the figure, its tree-derived unit, basis (code read | comparable
past change), or an estimate withheld for missing evidence. For a comparable past change, cite its revision and
affected paths and explain how its measured units map to the current task. Add the largest open input; findings
with location and tier; and next_actions.
