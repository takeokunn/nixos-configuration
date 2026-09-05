---
name: code-quality
description: Use when code needs complexity measurement, dead-code detection, deduplication, or a concrete refactoring proposal: cyclomatic and cognitive complexity, nesting depth, unused symbols, extract-method and early-return restructuring, and safe deletion. Use when a change feels large or repetitive and the question is what specifically to simplify.
---

<purpose>
Measure complexity, find what is genuinely dead, and propose refactoring that a measurement can confirm.
</purpose>

<skills_to_load>
  <load trigger="symbol-level navigation, reference search, or recording a refactoring
    pattern">serena-usage</load>
  <load trigger="a library's current recommended idiom is in question">context7-usage</load>
  <load trigger="the target is Lisp-family source, since parentheses must not be hand-edited">paredit-cli</load>
</skills_to_load>

<rules priority="critical">
  <rule>Never delete on a zero-reference result alone: pair it with a plain-text grep of the identifier, since
    search misses runtime-assembled names and deletion is the one action a later review can't catch.</rule>
  <rule>Don't refactor code no test exercises: report the coverage gap and delegate to the test agent; without
    a test, "no regression" is an opinion.</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state (`git stash`, checkout
    of an existing branch, `switch`, a hard reset, `clean -f`) to escape a problem; this agent already runs
    inside an isolated worktree, and reaching outside it can destroy a concurrent session's uncommitted work.
    SSOT-EXEMPT: restated deliberately, because the failure is irreversible, so a later SSoT audit should not
    prune this back to a bare cross-reference</rule>
</rules>
<rules priority="high">
  <rule>Measure before proposing, re-measure after changing; a metric estimated by reading is tagged inferred,
    never measured.</rule>
  <rule>Search the identifier itself, never its usual call shape: forward declarations, differently-shaped call
    sites, comments, and test doubles share the name alone, whether migrating or deleting a definition.</rule>
  <rule>Delete, don't demote, a finding whose own analysis calls acceptable: a severity from the triggering
    pattern, left above the explanation that dissolves it, puts a non-issue atop the list.</rule>
  <rule>If most existing, working files violate a rule you're checking, it was never the rule; fix the check,
    not the corpus: a convention inferred from a subset yields a large, confident, wrong list whose repair is
    worse than the defect it imagined.</rule>
</rules>
<rules priority="standard">
  <rule>Thresholds: CC≤10, CogC≤15, Depth≤4, Lines≤50, Params≤4; report the threshold with the measurement so a
    reader can dispute the threshold, not the number.</rule>
  <rule>Record what was examined and rejected: an empty finding list should still show the work.</rule>
  <rule>Splitting has a stop rule: extraction pays while each unit stays separately nameable and testable; past
    that it buys indirection at the reader's expense. Name the stop rule applied, so "could split further" is
    answered, not left open.</rule>
  <rule>Distinguish a tool that found issues from one that failed to run: exit 1 vs exit 2 by convention. A
    crashed linter reported as a clean pass is the failure mode this agent prevents.</rule>
</rules>

<workflow>
  <phase name="measure">
    <step order="1">
      <action>Map target symbols and control flow; measure each for CC, CogC, nesting depth, line count, and
        parameter count, preferring the project's own configured invocation.</action>
      <tool>Serena get_symbols_overview and find_symbol, Read, Bash (the project's quality tools)</tool>
      <output>Per-function metrics against their thresholds; lint and type errors</output>
    </step>
    <step order="2">
      <action>Find unreferenced symbols and duplicated blocks; confirm each with a plain-text grep of the
        identifier, ruling out string-keyed or reflective dispatch.</action>
      <tool>Serena find_referencing_symbols, Grep</tool>
      <output>Candidates with the searches that produced them</output>
    </step>
    <step order="3">
      <action>Locate the test files covering each function proposed for refactoring.</action>
      <tool>Glob, Read</tool>
      <output>Test coverage per target, or the function marked untested</output>
    </step>
  </phase>
  <reflection_checkpoint id="measurement_complete" after="measure">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>Each function's CC, CogC, depth, line count, and param count against its threshold: "metrics
      collected" names nothing.</check>
    <check>Each symbol reported unused or moved, the zero-reference search, the plain-text grep, and how dynamic
      dispatch was ruled out.</check>
    <check>If a rule spans more than one file: how many existing files violate it and whether they still work;
      majority violation means the rule is yours, not the project's.</check>
    <check>The test file covering each refactoring target, or that it is untested.</check>
    <on_unmet>Re-measure functions still unnamed; report undeletable, not delete, any symbol whose dynamic use
      can't be ruled out.</on_unmet>
  </reflection_checkpoint>
  <phase name="execute">
    <step order="1">
      <action>Apply auto-fixes and refactoring, run build, lint, and test, and re-measure changed
        functions.</action>
      <tool>Bash, Serena replace_symbol_body, Edit</tool>
      <output>Before/after metrics; build, lint, and test exit status</output>
    </step>
    <step order="2">
      <action>Move candidates whose analysis judged acceptable into considered_and_rejected with the reason (a
        self-refuting entry misorders the list below it) and record any reusable refactoring pattern.</action>
      <tool>Serena write_memory</tool>
      <output>Rejected candidates with their reasons; pattern recorded</output>
    </step>
  </phase>
</workflow>

<decision_criteria>
  <factor name="refactoring_safety" precedence="1">
    <unmet>No test exercises the code about to change: don't refactor it; report the coverage gap and delegate
      to the test agent.</unmet>
  </factor>
  <factor name="metric_reliability" precedence="2">
    <unmet>A metric was estimated by reading, not produced by a tool run: run the tool, or tag it inferred and
      say so in the summary.</unmet>
  </factor>
  <factor name="evidence_coverage" precedence="3">
    <unmet>A file in scope was never opened: read it, or list it under gaps as unanalyzed rather than call the
      sweep complete.</unmet>
  </factor>
</decision_criteria>

<escalations>
  <escalation condition="A threshold is exceeded">Report the measurement with the threshold and propose the
    specific restructuring</escalation>
  <escalation condition="Dynamic reference cannot be ruled out">Defer the deletion and request manual
    verification</escalation>
  <escalation condition="A test fails after refactoring">Roll the change back and analyze before
    retrying</escalation>
  <escalation condition="Coverage is insufficient">List the uncovered areas and delegate to the test
    agent</escalation>
</escalations>

<output>Follows output_contract in CLAUDE.md; verification names every tool run with its exit status. Add:
  before/after metrics, findings with file:line and tier, suggestions (restructuring type, target, expected
  reduction), considered_and_rejected (reason, so a reader can dispute it), and next_actions.</output>
