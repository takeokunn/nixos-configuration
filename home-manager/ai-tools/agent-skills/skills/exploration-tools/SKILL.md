---
name: exploration-tools
description: Search discipline for codebase exploration — scope, ranking, and the read-only boundary. Tool parameter schemas are injected by the harness and are not restated here; for symbol-level navigation load serena-usage instead.
version: 3.0.0
---

Glob, Grep, Read, and the LSP operations have their parameters injected by the harness. What is not injected is
when each is the right choice and what a result licenses you to claim.

## Choosing

- **Glob** for file discovery by name or extension.
- **Grep** for content. Prefer it to a shell `grep`/`rg` invocation, so behavior is consistent across
  environments and the output shape is predictable.
- **Read** after locating, never before. Reading files you have not first located is how a search turns into a
  sweep.
- **LSP `goToDefinition` / `findReferences` / `documentSymbol`**, or Serena's equivalents, whenever a language
  server is active. Text search cannot tell a definition from a mention, and cannot see a dynamically
  constructed reference. When you fall back to text search, say so and name the claim it weakens — see
  [serena-usage](../serena-usage/SKILL.md).

## Scope

Start narrow — one file or directory, then a file type across the project, then everything — and expand only
when the results are insufficient. Filter binary and generated paths out rather than ranking them down. Bound
the result size rather than returning a dump.

## What a result means

Rank by what the caller can act on: exact matches and definition sites first, usage sites next, comments and
test files last.

**A match is a location, not a behavior.** It proves the text exists; it does not prove the code is reached,
correctly ordered, or correctly parameterised. When the question underneath was behavioural, return the
locations and name the run that would settle it.

**A zero-match result is a fact about the pattern, not about the codebase.** Try the naming variants —
abbreviation, alternate casing, alternate extension, aliased import — before reporting an absence, and report
the patterns that returned nothing alongside the ones that hit.

Return every finding as file:line with enough surrounding context to judge it. A match with no context sends
the caller back to the file to reconstruct what you already had in front of you.

## Boundary

Exploration is read-only. Never modify a file during a search.

## Related

- [serena-usage](../serena-usage/SKILL.md) — symbol-level navigation, and what to do when it is unavailable
- [investigation-patterns](../investigation-patterns/SKILL.md) — the evidence methodology these tools serve
