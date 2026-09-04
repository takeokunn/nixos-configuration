---
name: design
description: Use when a task needs architecture evaluation, dependency and layer-violation checking, requirements decomposition, or effort estimation — circular dependencies, module boundaries, coupling and cohesion, ADRs, and where a new component belongs. Use proactively before implementation starts, not only in review, because a placement mistake costs more to correct than the code it holds.
---

<purpose>
Evaluate architecture, validate dependencies, decide where a component belongs, and size the work — all from
  what the tree actually shows.
</purpose>

<skills_to_load>
  <load trigger="tracing dependencies or reading/writing ADRs">serena-usage</load>
  <load trigger="the evidence for an architecture claim needs assembling rather than
    asserting">investigation-patterns</load>
  <load trigger="a framework's current recommended structure is in question">context7-usage</load>
</skills_to_load>

<rules priority="critical">
  <rule>Verify dependencies with find_referencing_symbols before any design decision — one inferred from a
    directory name isn't a dependency.</rule>
  <rule>Never estimate in clock time: hours depend on who works and how often they are interrupted, so the
    number can only be fabricated. Estimate in tree-derived units — files touched, call sites from a reference
    search, dependency depth, test cases required — naming the unit, the search behind it, and the unresolved
    input that would move the figure most, on its own line.</rule>
</rules>
<rules priority="high">
  <rule>Review placement and layering before implementation begins: everything needed to decide where a
    component belongs exists before the code does, and a layer violation found in review costs a dependency-wide
    move, not an edit.</rule>
  <rule>When aligning an artifact to a reference implementation, close the gap in one direction only: a stricter
    security gate, verification step, or fail-closed behavior on the aligned side is an asset, not a divergence
    — align the looser side up, never the stricter side down, even if the difference wasn't listed in advance as
    protected.</rule>
  <rule>Report a circular dependency or layer violation only against a layering rule the project actually
    states, and say where — if none exists, say so: a violation can't be claimed against a convention invented
    during the review.</rule>
  <rule>Where a design crosses an ownership boundary, name the commit point — the single irreversible step that
    makes the change visible — and state what's undoable before it and best-effort after. A design mutating two
    owners without one risks a partial write nobody can repair. Say whether a retried step is idempotent, and
    treat an indeterminate read as its own outcome, not as absence.</rule>
</rules>
<rules priority="standard">
  <rule>Match the pattern to the project's scale, and record the decision as an ADR in Serena memory.</rule>
</rules>

<workflow>
  <phase name="analyze">
    <step order="1">
      <action>Map the component hierarchy and identify the architecture pattern from structural facts — import
        direction, boundary types — not directory names; read existing ADRs.</action>
      <tool>Serena get_symbols_overview, find_symbol, read_memory</tool>
      <output>Component hierarchy, pattern with the facts identifying it, decision history</output>
    </step>
    <step order="2">
      <action>Trace dependency edges, find imports crossing a layer boundary the wrong way, and record
        fan-in/fan-out per module.</action>
      <tool>Serena find_referencing_symbols, Grep</tool>
      <output>Dependency graph, violations with file:line and severity, coupling observations</output>
    </step>
    <step order="3">
      <action>Read the specs, ADRs, and README for what the requirements leave ambiguous.</action>
      <tool>Glob, Read</tool>
      <output>Ambiguity list</output>
    </step>
  </phase>
  <reflection_checkpoint id="verification_complete" after="analyze">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>Each module whose imports were traced and the tool call that traced them — an unlisted module is
      unverified, not clean.</check>
    <check>Every wrong-direction import with its file:line, or that none were found among the modules
      named.</check>
    <check>The architecture pattern and the structural facts identifying it.</check>
    <check>The ADRs read from memory, or that list_memories returned none for this component.</check>
    <on_unmet>Trace remaining modules before reporting; if the project never states its layering rule, say so —
      a violation can't be claimed against a rule that doesn't exist.</on_unmet>
  </reflection_checkpoint>
  <phase name="plan">
    <step order="1">
      <action>Structure requirements — functional and non-functional, use cases as actors/goals/flows,
        acceptance criteria as observable behavior — then decompose into tasks with their dependency
        graph.</action>
      <output>Requirements and the task dependency graph</output>
    </step>
    <step order="2">
      <action>Size each task in tree-derived units, naming the search behind each figure, and assess technical,
        organizational, and quality risks.</action>
      <tool>Serena find_referencing_symbols</tool>
      <output>Estimates with unit, basis, and the largest open input; risk list</output>
    </step>
  </phase>
  <phase name="report">
    <step order="1">
      <action>Deliver the analysis and record the architecture decisions as an ADR.</action>
      <tool>Serena write_memory</tool>
      <output>Report; ADR stored</output>
    </step>
  </phase>
</workflow>

<decision_criteria>
  <factor name="estimation_basis" precedence="1">
    <unmet>An estimate is given for code not yet read, or expressed in clock time: read the affected modules and
      restate it in a tree-derived unit.</unmet>
  </factor>
  <factor name="architecture_coverage" precedence="2">
    <unmet>A component in scope has no traced dependency edges: trace it with find_referencing_symbols, or name
      it under gaps as unanalyzed rather than call the graph complete.</unmet>
  </factor>
  <factor name="pattern_match" precedence="3">
    <unmet>Two architecture patterns fit the evidence equally well: report both with the facts that would
      separate them, not the more familiar one.</unmet>
  </factor>
</decision_criteria>

<escalations>
  <escalation condition="Circular dependency">Fatal — report it before any other finding</escalation>
  <escalation condition="Layer violation">High severity, stated against the rule the project
    defines</escalation>
  <escalation condition="Requirements unclear">List the ambiguities rather than resolving them
    silently</escalation>
  <escalation condition="High risk">Propose a staged approach with what each stage de-risks</escalation>
  <escalation condition="A decision was made with no ADR">Recommend recording it</escalation>
</escalations>

<output>Follows output_contract in CLAUDE.md. Add: architecture pattern and layers; requirements split
  functional/non-functional; estimation carrying the figure, its tree-derived unit, basis (code read |
  comparable past change | nothing), and the largest open input; findings with location and tier; and
  next_actions.</output>
