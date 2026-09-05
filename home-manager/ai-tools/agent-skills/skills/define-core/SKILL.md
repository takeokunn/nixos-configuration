---
name: define-core
description: Shared workflow phases and patterns for the /define command. Use this skill when implementing /define to ensure consistent workflow structure, agent delegation, and requirements documentation patterns.
version: 3.0.0
---

The phase sequence /define executes, so the command file does not restate it. Question design and requirement
formatting live in [requirements-definition](../requirements-definition/SKILL.md), loaded alongside this one.

Read-only throughout: no file is created or modified, and no code is written. That is what makes the approval
step this command exists to create meaningful.

## Phases

**prepare**: Load requirements-definition, plus serena-usage before any memory operation and any other
companion this run needs: [fact-check](../fact-check/SKILL.md) when a claim needs an external source,
[context7-usage](../context7-usage/SKILL.md) when a library's current behavior is in question,
[core-patterns](../core-patterns/SKILL.md) for the shared decision-criteria structure. Then activate the
project, list memories, and read only the entries this task type calls for.

**analyze**: Extract the core requirements from the request, identify the technical constraints its context
implies, name the design decisions that will need user input, and take a first read on feasibility.

**investigate**: Dispatch in one message: explore for the relevant files and existing patterns, design for
architectural consistency and dependencies, database for schema implications where they exist. Then
general-purpose for completeness and dependency risk, which needs the others' output. Verify any external
claim against Context7 rather than recall.

**clarify**: Score the candidate questions by design branching, irreversibility, whether investigation could
have answered them, and effort impact. Classify each as spec confirmation, design choice, constraint, scope, or
priority. Ask the highest-scoring first, through AskUserQuestion with two to four structured options and one
marked (Recommended), including follow-ups, which go through the same tool rather than dropping to plain text.
Do not proceed on an assumption where a critical question is unanswered.

**verify**: Cross-check the user's answers against what the agents actually found, and read the
implementations the chosen approach depends on.

**document**: Produce the requirements document and the phased task breakdown for /execute.

**finalize**: The gate below.

## Gate after investigation

- The files and existing patterns the requirement will build on.
- The scope boundary: what is explicitly out of scope.
- Any technical blocker found, or that none was and what was checked.

Unmet: widen the investigation, or ask if only the user can supply it. **Never write a requirement around an
unexamined area.**

## The finalize gate

Read the Outstanding Issues section of the document just produced.

If it reads "none", **skip the gate entirely and finish**: do not prompt.

If it holds one or more items, ask with AskUserQuestion, offering exactly three dispositions:

- **Resolve now (Recommended)**: re-enter clarify, ask the outstanding questions, and patch the document.
- **Defer to /execute**: keep the issues documented and carry them explicitly into the handoff, so the
  implementer inherits them.
- **Stop and revise scope**: halt without finalizing the handoff, leaving the document visible so the user can
  revise the request.

**The resolution loop is bounded.** After "Resolve now", re-evaluate Outstanding Issues and re-present the gate
at most once more, after which only Defer and Stop remain. Never loop unbounded.

Choosing a disposition *is* a valid resolution of an issue that cannot be answered, so the rule that clarify
blocks until critical questions are answered is satisfied by this gate, not bypassed by it.

The purpose of the gate is that /define never ends by silently documenting a gap. Documenting an unresolved
question and stopping looks identical, in the output, to having resolved it.

## Agents

All read-only. Every delegation carries the scope, the target paths, the explicit prohibition on editing, and
the instruction to use AskUserQuestion for any user interaction rather than emitting a question as text.

- **explore**: relevant files and existing patterns
- **design**: architectural consistency, dependencies, API design
- **database**: schema design and query implications
- **general-purpose**: requirements completeness, dependency risk, effort in tree-derived units
- **validator**: cross-validation when findings conflict

explore, design, and database are independent and dispatch together; general-purpose consumes their output and
follows.

## Output

A requirements document carrying: the request in one sentence with its background and expected outcomes; the
current system and stack; functional requirements in FR-001 form marked mandatory or optional; non-functional
requirements; technical specifications with each decision's rationale; test requirements as observable
behavior; and Outstanding Issues.

Feasibility is stated as **the observable condition supporting it** (which capability was located at which
file:line, which was not found and where it was searched for), never as a score. Where a requirement rests on
an assumption rather than on investigation, say so at that requirement.

Outstanding Issues states "none" explicitly when there are none: **the finalize gate's skip branch keys off
that sentinel**, so an omitted section and an empty one are not the same thing.

Then the task breakdown: the dependency graph, phased tasks with files and dependencies, and the handoff
carrying the decisions made, the references, and the constraints, including what /execute must not assume.

## Related

- [requirements-definition](../requirements-definition/SKILL.md): question scoring and requirement formatting
- [investigation-patterns](../investigation-patterns/SKILL.md): evidence gathering for feasibility
- [serena-usage](../serena-usage/SKILL.md): the memory operations in prepare
- [fact-check](../fact-check/SKILL.md): verifying an external claim
- [execution-workflow](../execution-workflow/SKILL.md): what happens to the handoff afterwards
