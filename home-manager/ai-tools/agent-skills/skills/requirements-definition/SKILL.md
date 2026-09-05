---
name: requirements-definition
description: This skill should be used when the user asks to "define requirements", "create specification", "clarify requirements", "write requirements document", or mentions requirement analysis. Provides comprehensive requirements definition methodology.
version: 3.0.0
---

Question design and requirement formatting. The phase sequence belongs to
[define-core](../define-core/SKILL.md).

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

*This deliberately carries no arithmetic.* An earlier form scored each consideration 1–5 and gated on a total
of 15, which is the self-gated numeric threshold the corpus prohibits elsewhere: the scales were unanchored,
so the total encoded nothing the ordering above does not, while looking like a measurement. The prohibition on
numeric self-assessment is about rating *your own work*, which is self-confirming; ranking questions is a
different act, and the fix here is to drop the false precision rather than the ranking.

### Classify each question

- **Spec confirmation**: "does the API return null or an empty array for no results?"
- **Design choice**: "REST or GraphQL?"
- **Constraint**: "must this support the legacy client?"
- **Scope**: "are admin features in the first version?"
- **Priority**: "which of these ships first?"

Ask through AskUserQuestion with two to four concrete options and one marked (Recommended), so the user reviews
a proposal rather than doing the analysis. Follow-ups go through the same tool.

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

Tag each requirement **verified** (grounded in direct investigation), **inferred** (derived from something
verified but not directly observed), or **assumed** (from the user's framing, unchecked). **A requirement still
assumed at handoff time is not ready.**

## Map every requirement to a test

Unit coverage expectations, integration scenarios, and the acceptance criteria as observable behavior. A
requirement with no test scenario is a requirement nobody will notice going unmet.

## Related

- [define-core](../define-core/SKILL.md): the phase sequence this methodology runs inside
- [investigation-patterns](../investigation-patterns/SKILL.md): establishing current state before specifying
- [testing-patterns](../testing-patterns/SKILL.md): turning acceptance criteria into tests
- [execution-workflow](../execution-workflow/SKILL.md): delegating implementation once approved
- [core-patterns](../core-patterns/SKILL.md): the evidence tiers and the numeric-self-assessment prohibition
