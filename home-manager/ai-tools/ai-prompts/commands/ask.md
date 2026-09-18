---
argument-hint: [question]
description: Question and inquiry command
---

Answer a question about this project from evidence in it. Do not modify project files.

## Rules

Critical:

- Never modify, create, delete, or fix files or memories. Return memory candidates in the response for an
  authorized writer.
- Never justify the user's assumption: if evidence contradicts it, answer what the evidence supports and name
  both.
- Never answer from training data alone: recall reads as evidence, so the reader can't discount it.

Standard:

- Cite a file:line, command output, or external source for each finding and tag its evidence tier.
- Scale investigation to the question: a lookup (is X defined, does Y exist, what calls Z) is answered by your own
  Grep and Read, since an agent costs more than the work. Dispatch agents for subsystem-spanning questions,
  independent readings, or architecture/performance/quality judgment; send several in one message.

## Investigation hazards

Check these sources of error:

- **Generated document as source.** A document and its generator differ in reliability: the generator is
  evidence, the document a claim. Schema snapshots, OpenAPI files, generated clients, and architecture diagrams
  answer in the exact form asked and surface first; dangerous, since they go stale silently. A verified tier cites
  the generator itself (migration, handler, model), never the document describing it.
- **Call site role.** A call site proves a path exists, not its role: debug hooks and QA controls are easier to
  find than production code, so "only manual calls found" often means the feature lives elsewhere, not that it's
  unbuilt. Before reporting an absence, name and check where the production owner would be registered.
- **Tier scoped to file.** Tier the passage cited, not the file: one document can be accurate in its first half
  and describe nonexistent classes, columns, or features in its second, and a spec section can be aspirational
  rather than descriptive. A check in a sound section raises only that section's tier.
- **Stale recall.** A remembered pattern may have been removed, so verify it exists at the current ref before
  building on it.

## Workflow

### Prepare

1. Load investigation-patterns for a hypothesis to discharge, not a fact to locate: it governs evidence-gathering.
   Load fact-check too for external library or API behavior; a lookup needs neither. Use Skill. Return skills
   loaded, or why none was needed.
2. Activate the Serena project, call list_memories, and read entries matching this question's domain
   ({domain}-patterns, architecture-*, {project}-conventions), none if none match, since the index alone answers
   then. Use Serena activate_project, list_memories, read_memory. Return memories read, or "nothing in the index
   matched".

### Analyze

1. Restate the question in one sentence, name the claim answering it, locate the bearing code and documentation,
   and state the boundary: what's read, what's deliberately out of scope. Use Glob, Grep, Serena
   get_symbols_overview. Return the restated question, candidate files, scope boundary.
2. Answer it yourself for a few directly-readable files. Otherwise pick the agents the question requires (explore
   for structure, design for architecture and components, performance for cost and bottlenecks,
   quality-assurance or code-quality for judgment on the code), dispatched in one message. Name the skipped agents
   and why. Return the investigation plan, with the agents chosen and those deliberately skipped.

### Investigate

1. Execute the plan, verifying external claims (library behavior, API contract, version support) against Context7
   or the vendored source, not recall. Use Agent, Grep, Read, Context7. Return findings with file:line.

### Checkpoint on investigation quality

Per gate_discipline in CLAUDE.md. Name:

- The files read and the specific lines the answer will rest on.
- Each agent dispatched and the one claim it returned, or that it returned nothing usable.
- Any point where two readings disagree, and which cited a file:line.

Unmet: widen the investigation or re-dispatch with specific paths. If only the user can settle it, ask with
AskUserQuestion rather than picking a reading.

### Memory handoff

1. If memory_policy identifies a reusable finding, search the memory index by topic and return the candidate
   with any matching entry for an authorized writer. Do not persist it in this read-only command.

### Checkpoint on group consistency

- Any workflow phase skipped, and why.
- That no file or memory was modified.

Unmet: resolve the gap before returning the answer.

## Agents

Dispatched by need, not by default. Each is read-only here; each finding carries a file:line.

- **explore.** Structure and location across an unfamiliar area: reports what's searched for and not found, with
  the pattern used.
- **design.** Architectural relationships, dependency map, rationale and alternatives behind a pattern.
- **performance.** Bottlenecks and complexity, with file:line, or an explicit not-applicable.
- **quality-assurance.** Correctness and practice compliance across named files.
- **code-quality.** Complexity metrics and refactoring candidates ranked by impact.
- **validator**, dispatched on demand. Re-derive a disputed claim when two readings conflict and their evidence
  doesn't settle it.

## Decision criteria

1. **Evidence quality.** A claim names no file:line or command output: read the source and cite it, or tag it
   inferred or assumed with what would confirm it.
2. **Answer completeness.** Part of the question is unanswered: investigate it, or list it under gaps with the
   reason; never let it drop silently.
3. **Source verification.** An external claim rests on recall, not Context7 or the vendored source: verify it
   before stating it as fact.

## Output

Follows output_contract in CLAUDE.md, leading with the direct answer. Add:
recommendations, as actions without implementation, when implied, and any claim first written as verified that
couldn't name a command or file:line, with the tier it moved to.
