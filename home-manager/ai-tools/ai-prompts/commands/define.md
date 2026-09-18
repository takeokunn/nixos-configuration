---
argument-hint: [message]
description: Requirements definition command
---

Produce the specification the user approves before work starts: clarify the problem, the constraints, and the
decisions that need deciding. Read-only.

## Scope

Use when:

- Unclear scope with several design choices: "add authentication", "refactor the data layer"
- A change spanning 3+ files or 2+ system layers
- Irreversible or high-risk work: schema migration, breaking API change, auth rework
- A request whose framing may not reflect the real need

Do not use when:

- A bug with a clear isolated cause: /bug
- A one-line change or an already-specified task: /execute
- Defined requirements needing only technical investigation: /ask
- Documentation-only changes: /markdown

## Rules

Critical:

- Never create or modify files or memories. Return memory candidates in the handoff for an authorized writer.
- Say plainly when a request is technically impossible or rests on a capability that isn't there: a specification
  that assumes it becomes wasted implementation, discovered late.
- Distinguish the user's problem from a proposed solution. Rephrase the request as subject →
  object → operation to expose hidden ambiguity; a rephrasing that differs from the original signals a
  clarification gap.
- Never score the document (feasibility, objectivity, confidence, completeness) on a numeric scale: a score has no
  derivation, can't be checked or disputed, and reads as measurement. State the observable condition: which
  capability was found at which file:line, which wasn't, and where searched. Express effort in counted quantities
  (files touched, call sites from find_referencing_symbols, layers crossed, tests affected), never clock hours,
  which depend on who works and what interrupts them.

Standard:

- Signal → hypothesis → verify → conclude: never jump from signal straight to a question investigation could have
  answered: that spends the user's turn. There's no question budget for what investigation can't settle, though;
  an ambiguity resolved now costs a sentence, not a rewrite.
- Reason Why → How → What, assessing system impact (L0) before implementation detail (L4): L0 systems and
  cross-cutting concerns, L1 data and schema, L2 interfaces and contracts, L3 business rules and flow, L4 files
  and configuration. Starting at What specifies the solution already in hand.
- Specify only what's load-bearing: detail spent on what any competent implementer would choose anyway crowds out
  the decisions that need deciding, and an exhaustive document glossing the hard parts is worse than a short one:
  length isn't quality. Specify behavior and constraints, never function names, variable names, or algorithms.
- Start from the minimum scope satisfying the core need, expanding only on demonstrated necessity, not "might be
  needed later": require three evidenced use cases before generalizing. Ask whether the need survives fewer
  components, which parts are nice-to-have, and whether a phased split delivers value in Phase 1.
- Account for the full blast radius: specifying one component while ignoring what it affects produces a document
  wrong at implementation time.
- Verify a capability exists at the current ref before designing around it: recall about past states goes stale.
- Mark one option (Recommended) whenever AskUserQuestion presents choices, so the user reviews a proposal, not the
  analysis.
- Internal investigation stays internal. The document is synthesis, never a paste of agent output.

## Request signals

Read the request for these before forming any question.

- **A solution is described: add X, change Y to Z, use library A**
  The real requirement may be hidden behind it: the user has already narrowed. Ask what problem it solves, whether
  simpler solutions exist, and whether it fits the existing architecture.
- **A behavior is described: make it faster, show errors, support format X**
  Acceptance criteria may be clear while scope and approach are open: find which component owns the behavior, the
  measurable threshold, and the constraints.
- **A regression is referenced: it broke, this stopped working, used to work**
  Cause and symptom may differ, and the fix scope may exceed the reported location: establish when it broke, what
  changed, and where else the same cause lives.
- **Vague scope words: everywhere, all, the whole**
  Enumerate the matching locations and search boundaries. Clarify exclusions that would change the scope.
- **A capability not yet in the codebase is implied**
  A hidden dependency on a library, service, or infrastructure: establish whether it exists, the cost of
  introducing it, and whether existing primitives suffice.
- **just or simple: just add a field, simple change**
  The user may be unaware of the blast radius: map dependents, migrations, API consumers, and test coverage.

## Workflow

1. **Load.** Load define-core and requirements-definition first: define-core holds the phase sequence (prepare,
   analyze, investigate, clarify, verify, document, finalize), requirements-definition the methodology within:
   question prioritization, FR format, acceptance-criteria shape; skipping either leaves no workflow. Load fact-check too
   when requirements need external confirmation rather than recall. Use Skill. Return the skills loaded, and the
   phase list define-core returned so the rest can be checked against it.
2. **Run core workflow.** Run define-core's phases in order, applying what this file adds: Why → How → What
   ordering and L0-before-L4 depth, the request_signals table, the minimum-scope rule, and the output contract
   below. Where the two conflict, this file governs: it's the narrower context. Dispatch investigation by need:
   explore for existing patterns and reference implementations, design for architectural consistency and
   alternatives, database for schema and migration implications, general-purpose for completeness and dependency
   risk, validator for contradictions between specifications: send the independent ones in one message. Return the
   phases run, agents dispatched and skipped, and any phase skipped with its reason.

## Checkpoint: group consistency

Per gate_discipline in CLAUDE.md. Name:

- Any workflow phase skipped, and why.
- That no file or memory was created or modified.

Unmet: resolve the gap before delivering the document.

## Decision criteria

1. **requirement_clarity.** A requirement admits two readings that would produce different implementations: ask
   with AskUserQuestion rather than writing the cheaper one.
2. **technical_feasibility.** The document assumes a capability (library, API, schema column) not located in this
   codebase or confirmed via Context7. Verify it, or record it as an outstanding issue.
3. **stakeholder_alignment.** A design decision the user hasn't answered is being written as settled: put it back
   to the user, or move it to outstanding issues so the finalize gate sees it.

## Output

Follows output_contract in CLAUDE.md, delivering a requirements document with these sections: use tables and
Mermaid where structure is easier seen than read, and order abstract before concrete.

- **Summary** (always): The request in one sentence, the Why behind it, expected outcomes
- **Current State** (always): Existing system, stack, affected components
- **Functional Requirements** (always): FR-001 format, marked mandatory or optional, at behavior level
- **Non-Functional Requirements** (when-applicable): Performance, security, maintainability
- **Technical Specifications** (always): Design policy, impact scope, each key decision with its rationale and the
  alternatives rejected
- **Architecture Impact** (when-multi-layer): Diagram when 2+ layers are affected; dependency changes
- **Data / Schema Changes** (when-applicable): ERD or schema diff
- **Interface / API Changes** (when-applicable): Endpoint table or contract diff
- **Constraints** (always): Technical and operational
- **Test Requirements** (always): Unit, integration, and acceptance criteria stated as observable behavior rather
  than internal mechanism
- **Outstanding Issues** (always): Unresolved questions and anything asked for that this document does not
  specify, with reasons; "none" is stated explicitly. A disagreement with the user goes here too: when the
  investigation reaches a different severity or priority than the user assigned, record both assessments and what
  each rests on, and hand the decision back. Silently deferring buries the risk; silently escalating overrides a
  call that was the user's to make.
- **Task Breakdown** (always): Phased tasks with files, overview, and dependencies, plus the decisions,
  references, and constraints /execute needs, including what it must NOT assume

## Done when

- Every critical question is answered or recorded as outstanding
- Every requirement is grounded in codebase evidence, and feasibility rests on a located capability rather than an
  assumed one
- Scope is bounded, the blast radius stated, and a simpler scope satisfying the core need has been considered and
  rejected for a reason
- Acceptance criteria are observable behavior
- The document specifies decision points rather than the obvious
- The handoff identifies ready tasks and the unresolved decisions blocking dependent tasks, so a fresh
  implementer can proceed on ready work without assuming those decisions
- When outstanding issues are non-empty, define-core's finalize gate ran and the user chose to resolve, defer, or
  stop, rather than the command silently ending. Correctly skipped when "none"
