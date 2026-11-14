---
name: requirements-definition
description: Use for requirement analysis, specification, and clarification, and when running the define command (/define in Claude Code).
metadata:
  version: "4.2.0"
---

The phase sequence the define command (/define in Claude Code) executes, so the command file does not restate
it, plus question design and requirement formatting.

Read-only throughout, as the define command sets. Return the document in the response. Memory candidates:
CLAUDE.md memory_policy.

## Phases

**prepare**: Load serena-usage before any memory operation and any other companion this run needs:
[fact-check](../fact-check/SKILL.md) when a claim needs an external source,
[workflow-patterns](../workflow-patterns/SKILL.md) for the shared decision-criteria structure. Then read only
the memories relevant to this request, by the procedure in serena-usage. If the store is unavailable, continue
from repository evidence and report the limitation.

**analyze**: Extract the core requirements from the request, identify the technical constraints its context
implies, name the design decisions that will need user input, and take a first read on feasibility.

**investigate**: Establish the relevant files, patterns, dependencies, and schema implications. Delegate
bounded independent checks only when their value warrants coordination, using the available agent catalog.
If a needed role is unavailable, perform its checks yourself and report the limitation; do not substitute a
role whose runtime restrictions are unavailable. Verify external library claims by the fact-check skill.

**clarify**: Prioritize and classify the candidate questions as described under
[Which questions to ask first](#which-questions-to-ask-first). Which questions to ask: the define command when
it is running, otherwise CLAUDE.md work_selection. Ask with the runtime's question tool when available
(AskUserQuestion in Claude Code, request_user_input in Codex), within its supported option count and with the
recommended option marked in the user's language; otherwise ask a concise question in text. Every open question
not asked becomes a recorded assumption in the document, tagged inferred or assumed, and work proceeds.

**verify**: Cross-check the user's answers against the investigation findings, and read the
implementations the chosen approach depends on.

**document**: Produce the requirements document and the phased task breakdown for the execute command
(/execute in Claude Code).

## Gate after investigation

- The files and existing patterns the requirement will build on.
- The scope boundary: what is explicitly out of scope.
- Any technical blocker found, or that none was and what was checked.

Unmet: widen the investigation, or record the gap as an assumption. **Never write a requirement around an
unexamined area.**

## Unresolved items

Outstanding Issues lists each unresolved item with the assumption taken. State whether the handoff is ready or partially blocked, naming the tasks that depend on an
assumption that could not be checked. Recording an assumption does not verify it or clear a dependent task's
gate.

## Agents

All read-only. Brief contents: CLAUDE.md delegation. A delegated agent does not ask the user: it returns its
questions to the parent, which ranks them with its own under
[Which questions to ask first](#which-questions-to-ask-first).

- **explore**: relevant files and existing patterns
- **design**: architectural consistency, dependencies, API design
- **infra**: schema, migration, and query implications
- **general-purpose**: requirements completeness, dependency risk, effort in tree-derived units
- **verification** (read-only reconcile mode): cross-validation when findings conflict

When selected, independent explore, design, and infra checks dispatch together. A general-purpose check of
their combined findings follows only when completeness or dependency risk needs a separate review.

## Investigate before asking

Establish the current state first: directory structure, the symbol overview of the affected area, keyword and
symbol search for the feature's existing neighbours, the reference graph around anything that will change, then
the specific files. External library behavior: the fact-check skill.

## Which questions to ask first

Rank a candidate question by four considerations, in this order:

1. **Design branching**: how much the answer changes the shape of the solution.
2. **Irreversibility**: how expensive the wrong choice is to undo after implementation.
3. **Investigation impossibility**: whether code investigation could settle it instead. If it could,
   investigate rather than ask.
4. **Effort impact**: how much the answer moves the size of the work.

A question that branches the design *and* whose wrong answer is expensive to undo ranks first. When clarify
leaves it unasked, list its assumption first in Outstanding Issues.

### Classify each question

- **Spec confirmation**: "does the API return null or an empty array for no results?"
- **Design choice**: "REST or GraphQL?"
- **Constraint**: "must this support the legacy client?"
- **Scope**: "are admin features in the first version?"
- **Priority**: "which of these ships first?"

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

Record explicit user requests as desired behavior, not as unverified claims about the existing system.
Tag factual premises per CLAUDE.md evidence. **A factual premise still assumed at handoff
time is not ready to support dependent implementation.** A partial handoff must name those blocked
tasks separately from independent, ready tasks; recording the assumption does not approve it.

## Map every requirement to a test

Unit coverage expectations, integration scenarios, and the acceptance criteria as observable behavior. A
requirement with no test scenario is a requirement nobody will notice going unmet.

## Output

A requirements document carrying what the implementation needs, and omitting empty sections: the request in one
sentence with its background and expected outcomes; the current system and stack; functional requirements in
FR-001 form marked mandatory or optional; non-functional requirements when applicable; technical specifications
with each decision's rationale; test requirements as observable behavior; and Outstanding Issues when any
remain.

State feasibility as in [Feasibility is a condition, not a score](#feasibility-is-a-condition-not-a-score).
Where a requirement rests on an assumption rather than on investigation, say so at that requirement.

Then the task breakdown: the dependency graph, phased tasks with files and dependencies, and the handoff
carrying the decisions made, the references, and the constraints, including what the execute command must not assume.

## Related

- [investigation-patterns](../investigation-patterns/SKILL.md): establishing current state and evidence for feasibility
- [testing-patterns](../testing-patterns/SKILL.md): turning acceptance criteria into tests
- [serena-usage](../serena-usage/SKILL.md): the memory operations in prepare
- [fact-check](../fact-check/SKILL.md): verifying an external claim
- [execution-workflow](../execution-workflow/SKILL.md): what happens to the handoff afterwards
- [workflow-patterns](../workflow-patterns/SKILL.md): the decision-criteria template and the numeric-self-assessment prohibition
