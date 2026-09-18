---
name: docs
description: "Use when writing or updating a README, API reference, or OpenAPI/Swagger spec, or when documentation has drifted from the code it describes. Reads the implementation before documenting it, and reports which validators ran."
---

Write and maintain documentation that matches the code: READMEs, API references, OpenAPI specs, and the
synchronization between them and the implementation.

## Rules

Critical:

- Read the implementation before documenting it: a symbol name is not its behaviour.
- Detect breaking API changes and propose a versioning path.
- Never author a drift-prone number: a test count, file count, coverage percentage, or benchmark figure is wrong
  after the next commit, and wrong in the direction that makes a reader distrust the rest of the document. Name
  the command that produces the current number instead of transcribing today's value.
- Follow hard_rules in CLAUDE.md for Git operations and shared working-tree state. Do not assume this
  session is worktree-isolated.

## Workflow

1. **Analyze (Skill).** Load technical-documentation before drafting; consult Context7 or official documentation
   when a framework's own conventions decide the document's shape.
2. **Analyze.** Map the scope: module and symbol structure, endpoints with the file:line defining each, existing
   docs referencing this scope, and the audience and depth the package metadata and existing headings imply,
   using Serena get_symbols_overview and find_symbol, Grep for route registrations, Glob for README and
   `docs/**/*.md`, and Read. Return the symbol map, the endpoint list with definitions, the doc paths that will
   need updating, and the audience.
3. **Analyze.** Establish what changed publicly: diff against the base ref with Bash git diff and find the call
   sites of every changed signature with Serena find_referencing_symbols. Return the changed public signatures
   and their call sites.
4. **Evaluate.** Read the implementation of each symbol to be documented, and check route definitions against the
   framework's own conventions rather than against general REST or GraphQL habit, using Read and Context7. Return
   the behaviour per symbol cited to file:line, and convention deviations with file:line.
5. **Evaluate.** Validate any spec against its validator with Bash. Return the validator exit status and the
   errors it reported.

### Checkpoint on evaluation quality, before executing

Per gate_discipline in CLAUDE.md. Name:

- Every endpoint checked against the framework's conventions, with the file:line defining it.
- The documented examples that were executed or type-checked, and the ones that were not.
- Every statement in the draft taken from framework convention rather than from code actually read.
- Every drift-prone count, percentage, or timing figure in the draft, replaced by the command that regenerates
  it. Fixed protocol constants and documented limits are not measurements of current repository state.

Unmet: read the implementation behind the unnamed items, or omit the unsupported claim and report the gap.

### Execute

6. Write the authorized documentation changes, then run the link checker and spec validator over what was
   written, using Write, Edit, and Bash. Return the paths written, and the command and exit status per validated
   file.

## Decision criteria

1. **Accuracy.** A documented signature, example, status code, or default cannot be traced to a file:line. Trace
   it, or delete the claim.
2. **Documentation completeness.** An endpoint or exported symbol inside the requested scope has no entry.
   Document it, or list it under gaps as deliberately excluded.

## Escalations

- Source analysis failed: generate what the read scope supports and name what it does not cover.
- Endpoints cannot be parsed: identify the framework and ask for the route path rather than guessing.
- Breaking change detected: propose the deprecation and migration period before documenting the new shape as
  current.
- Spec validation failed: report the validator errors with the file:line each points at.

## Output

Follows output_contract in CLAUDE.md; verification carries every validator and link checker run with its exit
status. Add: mode (generate | sync | review); endpoints documented and issues found, each with defining
file:line; breaking changes and deprecations; and next_actions.
