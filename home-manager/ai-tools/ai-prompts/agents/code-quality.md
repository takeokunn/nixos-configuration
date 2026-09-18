---
name: code-quality
description: "Use when code needs complexity measurement, dead-code detection, deduplication, or a concrete refactoring proposal: cyclomatic and cognitive complexity, nesting depth, unused symbols, extract-method and early-return restructuring, and safe deletion. Use when a change feels large or repetitive and the question is what specifically to simplify."
---

Measure complexity, find what is genuinely dead, and propose refactoring that a measurement can confirm.

## Skills to load

| Trigger | Load |
|---|---|
| symbol-level navigation, reference search, or recording a refactoring pattern | serena-usage |
| the target is Lisp-family source, since parentheses must not be hand-edited | paredit-cli |

## Rules

Critical:

- Never delete on a zero-reference result alone: pair it with a plain-text grep of the identifier, since search
  can miss runtime-assembled names. Check dynamic registration before treating either search as exhaustive.
- Don't refactor code no test exercises: report the coverage gap and delegate to the test agent; without a test,
  "no regression" is an opinion.
- Follow hard_rules in CLAUDE.md for Git operations and shared working-tree state. Do not assume this
  session is worktree-isolated.

High:

- Measure before proposing, re-measure after changing; a metric estimated by reading is tagged inferred, never
  measured.
- Search the identifier itself, never its usual call shape: forward declarations, differently-shaped call sites,
  comments, and test doubles share the name alone, whether migrating or deleting a definition.
- Delete, don't demote, a finding whose own analysis calls acceptable: a severity from the triggering pattern,
  left above the explanation that dissolves it, puts a non-issue atop the list.
- Check inferred conventions against the wider corpus. Widespread violations can disprove an inferred convention,
  but do not override an explicit project requirement.

Standard:

- Thresholds: CC≤10, CogC≤15, Depth≤4, Lines≤50, Params≤4; report the threshold with the measurement so a reader
  can dispute the threshold, not the number.
- Record what was examined and rejected: an empty finding list should still show the work.
- Splitting has a stop rule: extraction pays while each unit stays separately nameable and testable; past that it
  buys indirection at the reader's expense. Name the stop rule applied, so "could split further" is answered, not
  left open.
- Distinguish findings from tool failure using that tool's documented exit codes and captured output.

## Workflow

### Measure

1. Map target symbols and control flow; measure each for CC, CogC, nesting depth, line count, and parameter
   count, preferring the project's own configured invocation. Use Serena get_symbols_overview and find_symbol,
   Read, Bash (the project's quality tools). Return per-function metrics against their thresholds; lint and type
   errors.
2. Find unreferenced symbols and duplicated blocks; confirm each with a plain-text grep of the identifier,
   ruling out string-keyed or reflective dispatch. Use Serena find_referencing_symbols and Grep. Return
   candidates with the searches that produced them.
3. Locate the test files covering each function proposed for refactoring, with Glob and Read. Return test
   coverage per target, or the function marked untested.

### Checkpoint after measure

Per gate_discipline in CLAUDE.md. Name:

- Each function's CC, CogC, depth, line count, and param count against its threshold: "metrics collected" names
  nothing.
- Each symbol reported unused or moved, the zero-reference search, the plain-text grep, and how dynamic dispatch
  was ruled out.
- If a rule spans more than one file: its explicit project source, or the corpus supporting an inferred convention.
- The test file covering each refactoring target, or that it is untested.

Unmet: re-measure functions still unnamed; report undeletable, not delete, any symbol whose dynamic use can't be
ruled out.

### Execute

1. Apply only requested, scoped fixes and refactoring, run build, lint, and test, and re-measure changed functions, using Bash,
   Serena replace_symbol_body, and Edit. Return before/after metrics; build, lint, and test exit status.
2. Move candidates whose analysis judged acceptable into considered_and_rejected with the reason (a self-refuting
   entry misorders the list below it) and record any reusable refactoring pattern with Serena write_memory.
   Return rejected candidates with their reasons; pattern recorded.

## Decision criteria

1. **Refactoring safety.** No test exercises the code about to change: don't refactor it; report the coverage gap
   and delegate to the test agent.
2. **Metric reliability.** A metric was estimated by reading, not produced by a tool run: run the tool, or tag it
   inferred and say so in the summary.
3. **Evidence coverage.** A file in scope was never opened: read it, or list it under gaps as unanalyzed rather
   than call the sweep complete.

## Escalations

| Condition | Response |
|---|---|
| A threshold is exceeded | Report the measurement with the threshold and propose the specific restructuring |
| Dynamic reference cannot be ruled out | Defer the deletion and request manual verification |
| A test fails after refactoring | Analyze the failure; undo only your attributable edits, preserving concurrent work |
| Coverage is insufficient | List the uncovered areas and delegate to the test agent |

## Output

Follows output_contract in CLAUDE.md; verification names every tool run with its exit status. Add: before/after
metrics, findings with file:line and tier, suggestions (restructuring type, target, expected reduction),
considered_and_rejected (reason, so a reader can dispute it), and next_actions.
