---
name: code-quality
description: Use when code needs complexity measurement, dead-code detection, deduplication, or a concrete refactoring proposal — cyclomatic and cognitive complexity, nesting depth, unused symbols, extract-method and early-return restructuring, and safe deletion. Use when a change feels large or repetitive and the question is what specifically to simplify.
---

<purpose>
Measure complexity, find what is genuinely dead, and propose refactoring that a measurement can confirm.
</purpose>

<skills_to_load>
  Naming a skill here does not put it in context. Load it with the Skill tool when its trigger applies.
  <load trigger="symbol-level navigation, reference search, or recording a refactoring pattern">serena-usage</load>
  <load trigger="a specific linter or formatter invocation is needed and the project's own config does not settle it">quality-tools</load>
  <load trigger="a library's current recommended idiom is in question">context7-usage</load>
  <load trigger="the target is Lisp-family source — parentheses must not be hand-edited">paredit-cli</load>
</skills_to_load>

<rules priority="critical">
  <rule>Never delete a symbol on a zero-reference result alone. A symbol search cannot see a name assembled at
    runtime, so pair it with a plain-text grep of the identifier — deletion is the one action here that a later
    review cannot catch.</rule>
  <rule>Do not refactor code that no test exercises. Report the coverage gap and delegate to the test agent;
    without a test, "no regression" is an opinion.</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state — `git stash`, checkout
    of an existing branch, `switch`, a hard reset, `clean -f` — to escape a problem; this agent already runs
    inside an isolated worktree, and reaching outside it can destroy a concurrent session's uncommitted work.
    SSOT-EXEMPT: restated deliberately, because the failure is irreversible, so a later SSoT audit should not
    prune this back to a bare cross-reference</rule>
</rules>
<rules priority="high">
  <rule>Measure before proposing and re-measure after changing. A metric estimated by reading is tagged
    inferred, never reported as measured.</rule>
  <rule>Search the identifier itself, never the shape it is usually called in. Forward declarations,
    differently-shaped call sites, comments, and test doubles share the name and nothing else — this applies to
    migrating a definition as much as to deleting one.</rule>
  <rule>Delete a finding whose own analysis concludes it is acceptable; do not demote it. A severity assigned
    from the pattern that triggered the search, left standing above an explanation that dissolves it, puts a
    non-issue at the top of a priority list.</rule>
  <rule>If a rule you are checking against is violated by most existing files and they work, it was never the
    rule. Fix the check, not the corpus — a convention inferred from a subset produces a large, confident,
    wrong finding list whose natural repair is more destructive than the imagined defect.</rule>
</rules>
<rules priority="standard">
  <rule>Thresholds are CC≤10, CogC≤15, Depth≤4, Lines≤50, Params≤4. Report the threshold alongside the
    measurement so a reader can disagree with the threshold rather than the number.</rule>
  <rule>Record what was examined and rejected, so an empty finding list still carries evidence of the work.</rule>
</rules>

<workflow>
  <phase name="measure">
    <step order="1">
      <action>Map the target symbols and their control flow, then measure each: CC, CogC, nesting depth, line
        count, parameter count. Prefer the invocation the project's own config declares.</action>
      <tool>Serena get_symbols_overview and find_symbol, Read, Bash (the project's quality tools)</tool>
      <output>Per-function metrics against their thresholds; lint and type errors</output>
    </step>
    <step order="2">
      <action>Find the unreferenced symbols and the duplicated blocks, then confirm each with a plain-text grep
        of the identifier and rule out string-keyed or reflective dispatch.</action>
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
    <check>Each function measured with its CC, CogC, depth, line count, and param count, and the threshold each
      was compared against. "Metrics collected" names nothing.</check>
    <check>Each symbol reported unused or being moved, the search that returned zero references, the plain-text
      grep of the identifier, and how dynamic dispatch was ruled out.</check>
    <check>If a rule was applied across more than one file: how many existing files violate it and whether they
      currently work. A majority violating it means the rule is yours, not the project's.</check>
    <check>The test file covering each refactoring target, or that it is untested.</check>
    <on_unmet>Re-measure the functions still unnamed. If a symbol's dynamic use cannot be ruled out, report it
      as undeletable rather than proposing deletion.</on_unmet>
  </reflection_checkpoint>
  <phase name="execute">
    <step order="1">
      <action>Apply the auto-fixes, then the refactoring, then run the project's build, lint, and test
        commands. Re-measure the changed functions.</action>
      <tool>Bash, Serena replace_symbol_body, Edit</tool>
      <output>Before/after metrics; build, lint, and test exit status</output>
    </step>
    <step order="2">
      <action>Move every candidate whose own analysis concluded it was acceptable into considered_and_rejected
        with the reason. A self-refuting entry left in the list misorders everything below it. Record any
        refactoring pattern worth reusing.</action>
      <tool>Serena write_memory</tool>
      <output>Rejected candidates with their reasons; pattern recorded</output>
    </step>
  </phase>
</workflow>

<decision_criteria>
  <factor name="refactoring_safety" precedence="1">
    <unmet>No test exercises the code about to change. Do not refactor it — report the coverage gap and
      delegate to the test agent.</unmet>
  </factor>
  <factor name="metric_reliability" precedence="2">
    <unmet>A reported metric was estimated by reading rather than produced by a tool run. Run the tool, or tag
      the metric inferred and say so in the summary.</unmet>
  </factor>
  <factor name="evidence_coverage" precedence="3">
    <unmet>A file in the stated scope was never opened. Read it, or name it under gaps as unanalyzed instead of
      reporting the sweep as complete.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<escalations>
  <escalation condition="A threshold is exceeded">Report the measurement with the threshold and propose the specific restructuring</escalation>
  <escalation condition="Dynamic reference cannot be ruled out">Defer the deletion and request manual verification</escalation>
  <escalation condition="A test fails after refactoring">Roll the change back and analyze before retrying</escalation>
  <escalation condition="Coverage is insufficient">List the uncovered areas and delegate to the test agent</escalation>
</escalations>

<output>
  Follows output_contract in CLAUDE.md. verification names every tool run with its exit status. Add: the
  before/after metrics; the findings with file:line and tier; suggestions, each with its restructuring type,
  target, and expected reduction; considered_and_rejected, each with the reason stated so a reader can dispute
  it; and next_actions.
</output>
