---
name: code-quality
description: "Use when code needs complexity measurement, dead-code detection, deduplication, or a concrete refactoring proposal: cyclomatic and cognitive complexity, nesting depth, unused symbols, extract-method and early-return restructuring, and safe deletion. Use when a change feels large or repetitive and the question is what specifically to simplify."
---

Measure complexity, confirm dead code, and propose the smallest refactoring that improves the measured result.
Apply the shared contracts in CLAUDE.md.

## Skills

Load serena-usage before symbol or memory operations, and paredit-cli for Lisp-family edits.

## Constraints

- Never delete a symbol solely because a reference tool returns zero. Search the identifier itself and inspect
  dynamic registration, reflection, configuration, and external entrypoints.
- Do not refactor code without relevant tests. Request test-agent coverage for the gap first.
- Distinguish measurements from estimated improvements. Reject findings whose own analysis shows the code is
  acceptable; retain the checkable reason under considered_and_rejected.
- Check inferred conventions against the wider corpus. Widespread counterexamples can refute an inferred
  convention, not an explicit project requirement.
- Extract a helper only when it names a coherent, independently testable responsibility. Stop when extraction
  adds indirection without reducing complexity.
- Distinguish a tool's findings from tool failure using its documented exit codes and output.

## Workflow

1. Bound the files and symbols. Measure per-function cyclomatic complexity, cognitive complexity, nesting,
   length, and parameter count; collect relevant lint and type errors.
2. Investigate duplication and unused-symbol candidates. Confirm each candidate with identifier searches and
   reachable callers, including dynamic uses. Read the affected tests before proposing a change.
3. Use these review thresholds unless the project defines its own: cyclomatic complexity ≤ 10, cognitive
   complexity ≤ 15, nesting ≤ 4, function lines ≤ 50, parameters ≤ 4. A threshold crossing starts an
   investigation; it is not a finding by itself.
4. Before editing, name the measured baseline, affected callers, coverage, proposed restructuring, and expected
   reduction (inferred until measured), following gate_discipline. If relevant coverage is missing, stop the
   refactor and request tests.
5. Make only authorized, scoped changes. Run the relevant build, lint, and tests; remeasure the same functions.
   Report the before/after metrics, residual complexity, and rejected candidates. Persist only what memory_policy
   permits.

## Output

Use output_contract. Include measured before/after metrics, findings with file:line and evidence tier,
proposed restructuring with its target and expected reduction, considered_and_rejected, and next_actions.
