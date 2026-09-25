---
name: requirements-definition
description: Use for requirement analysis, specification, and clarification, and when running /define. Covers its phase sequence (prepare through finalize), the investigation and finalize gates, read-only agent dispatch, question prioritization, and requirement formatting.
metadata:
  version: "4.0.0"
---

The phase sequence /define executes, so the command file does not restate it, plus question design and
requirement formatting.

Read-only throughout: no file or memory is created or modified, and no code is written. Return the document
in the response. Report any memory candidates for a later write-authorized phase.

## Phases

**prepare**: Load serena-usage before any memory operation and any other companion this run needs:
[fact-check](../fact-check/SKILL.md) when a claim needs an external source,
[workflow-patterns](../workflow-patterns/SKILL.md) for the shared decision-criteria structure. Then activate the
project, list memories, and read only the entries this task type calls for.

**analyze**: Extract the core requirements from the request, identify the technical constraints its context
implies, name the design decisions that will need user input, and take a first read on feasibility.

**investigate**: Dispatch in one message: explore for the relevant files and existing patterns, design for
architectural consistency and dependencies, infra for schema implications where they exist. After they
return, dispatch general-purpose with their output for completeness and dependency risk. Verify any external
claim against Context7 rather than recall.

**clarify**: Prioritize and classify the candidate questions as described under
[Which questions to ask first](#which-questions-to-ask-first). Ask the highest-priority first, through the
runtime's question tool with its supported options and one marked (Recommended), including follow-ups. If no
question tool is available, ask a concise question in text. Do not proceed on an assumption where a critical
question is unanswered.

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
- **Defer to /execute**: carry the issues into the handoff and mark dependent tasks blocked until their
  critical questions are answered. Independent tasks may proceed on their own verified requirements.
- **Stop and revise scope**: halt without finalizing the handoff, leaving the document visible so the user can
  revise the request.

**The resolution loop is bounded.** After "Resolve now", re-evaluate Outstanding Issues and re-present the gate
at most once more, after which only Defer and Stop remain. Never loop unbounded.

Deferral records an unresolved issue; it does not approve an assumption or clear a dependent task's gate.
State whether the handoff is ready, partially blocked, or stopped, naming the affected tasks.

## Agents

All read-only. Every delegation carries the scope, the target paths, the explicit prohibition on editing, and
the instruction to use the runtime's question tool when available, otherwise a concise question in text.

- **explore**: relevant files and existing patterns
- **design**: architectural consistency, dependencies, API design
- **infra**: schema, migration, and query implications
- **general-purpose**: requirements completeness, dependency risk, effort in tree-derived units
- **verification** (read-only reconcile mode): cross-validation when findings conflict

explore, design, and infra are independent and dispatch together; general-purpose consumes their output and
follows.

## Investigate before asking

Establish the current state first: directory structure, the symbol overview of the affected area, keyword and
symbol search for the feature's existing neighbours, the reference graph around anything that will change, then
the specific files. Verify external library behavior against Context7 rather than recall.

**A question that investigation could have answered spends the user's turn.** The point of investigating first
is not thoroughness for its own sake: it is that the remaining questions are then exactly the ones only the
user can settle.

## Which questions to ask first

Rank a candidate question by four considerations, in this order:

1. **Design branching**: how much the answer changes the shape of the solution.
2. **Irreversibility**: how expensive the wrong choice is to undo after implementation.
3. **Investigation impossibility**: whether code investigation could settle it instead. If it could,
   investigate rather than ask.
4. **Effort impact**: how much the answer moves the size of the work.

A question is **critical** when it branches the design *and* the wrong answer is expensive to undo. Ask those
first, and do not proceed on an assumption while one is unanswered.

### Classify each question

- **Spec confirmation**: "does the API return null or an empty array for no results?"
- **Design choice**: "REST or GraphQL?"
- **Constraint**: "must this support the legacy client?"
- **Scope**: "are admin features in the first version?"
- **Priority**: "which of these ships first?"

Ask through the runtime's question tool, respecting its supported option count and marking a recommendation
when supported. If no question tool is available, ask a concise question in the response.

## Writing the requirements

**Functional requirements** carry an identifier, a priority, and acceptance criteria specific enough to test:

```
FR-001: User Authentication
Priority: mandatory
- Users log in with email and password
- A session expires after 24 hours of inactivity
- Failed attempts are rate-limited to 5 per hour
```

Mark every requirement mandatory or optional **with the reason**: treating all requirements as equally
important is the same as prioritizing none.

**Non-functional requirements** carry a measurable target, not an adjective: a response-time percentile and its
threshold, a concurrency figure, the encryption at rest, the documented surface. "Fast" and "secure" are not
requirements.

**Technical specifications** carry each design decision *with its rationale and its impact scope*: what the
decision was, why it beat the alternatives, and which parts of the system now depend on it.

Describe **what must be achieved, not how to implement it.** A requirement that names function names or
algorithms has taken a decision away from the implementer under the guise of specifying it.

Document every assumption explicitly where a requirement is unclear, and identify the technical and operational
constraints rather than leaving them to be discovered.

## Feasibility is a condition, not a score

State which capability was located at which file:line, and which was not found and where it was searched for.

> Feasible: the export pipeline this depends on exists at `src/export/pipeline.ts:42`. No rate-limiting
> primitive exists in the codebase (searched `src/lib`, `src/middleware`), so that remains an open constraint
> recorded in Outstanding Issues.

A feasibility number produced in the same pass that wrote the requirement never contradicts that requirement,
so nothing downstream ever reads a low score and investigates further.

Record explicit user requests as desired behavior, not as unverified claims about the existing system.
Tag factual premises **verified** (grounded in direct investigation), **inferred** (derived from something
verified but not directly observed), or **assumed** (unchecked). **A factual premise still assumed at handoff
time is not ready to support dependent implementation.** A partial handoff must name those blocked
tasks separately from independent, ready tasks; deferral does not approve the assumption.

## Map every requirement to a test

Unit coverage expectations, integration scenarios, and the acceptance criteria as observable behavior. A
requirement with no test scenario is a requirement nobody will notice going unmet.

## Output

A requirements document carrying: the request in one sentence with its background and expected outcomes; the
current system and stack; functional requirements in FR-001 form marked mandatory or optional; non-functional
requirements; technical specifications with each decision's rationale; test requirements as observable
behavior; and Outstanding Issues.

Feasibility is stated as the observable condition supporting it, per the section above, never as a score.
Where a requirement rests on an assumption rather than on investigation, say so at that requirement.

Outstanding Issues states "none" explicitly when there are none: **the finalize gate's skip branch keys off
that sentinel**, so an omitted section and an empty one are not the same thing.

Then the task breakdown: the dependency graph, phased tasks with files and dependencies, and the handoff
carrying the decisions made, the references, and the constraints, including what /execute must not assume.

## Related

- [investigation-patterns](../investigation-patterns/SKILL.md): establishing current state and evidence for feasibility
- [testing-patterns](../testing-patterns/SKILL.md): turning acceptance criteria into tests
- [serena-usage](../serena-usage/SKILL.md): the memory operations in prepare
- [fact-check](../fact-check/SKILL.md): verifying an external claim
- [execution-workflow](../execution-workflow/SKILL.md): what happens to the handoff afterwards
- [workflow-patterns](../workflow-patterns/SKILL.md): the decision-criteria template and the numeric-self-assessment prohibition
