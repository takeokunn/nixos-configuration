---
argument-hint: [question]
description: Question and inquiry command
---

<purpose>
Answer a question about this project from evidence in it. Read-only: never modifies files.
</purpose>

<rules priority="critical">
  <rule>Never modify, create, or delete a file, and never implement a fix. The user invoked a question, not a
    change; an answer that edits the codebase removes their decision. Serena write_memory is not a file write
    and is permitted.</rule>
  <rule>Never justify the user's assumption. If the evidence contradicts the question's premise, say so and
    answer the question the evidence supports, naming both.</rule>
  <rule>Never answer from training data alone. An answer assembled from recall is indistinguishable in tone
    from one assembled from this repository, so the reader has no way to discount it.</rule>
</rules>
<rules priority="standard">
  <rule>Every finding carries a file:line. The reader's next move is to open it, and a finding they cannot
    locate costs them the same investigation over again.</rule>
  <rule>Scale the investigation to the question. A lookup — where is X defined, does Y exist, what calls Z —
    is answered by your own Grep and Read; dispatching an agent for it costs more than the work. Dispatch
    agents when the question spans subsystems, needs several independent readings, or turns on judgment about
    architecture, performance, or quality. When you dispatch more than one, send them in a single message.</rule>
</rules>

<investigation_hazards>
  Four ways an investigation reaches a confident wrong answer. Each has produced one here.

  <hazard name="generated_document_as_source">When a committed document and the thing that generates it both
    exist, the generator is the evidence and the document is a claim. A schema snapshot, checked-in OpenAPI
    file, generated client, or architecture diagram answers the question in exactly the form it was asked and
    is the first thing found — which is what makes it dangerous, because it goes stale silently. A verified
    tier requires citing the migration, the handler, or the model, not the document describing them.</hazard>
  <hazard name="call_site_role">A call site tells you a code path exists, not what role it plays. Debug hooks,
    QA controls, and preview entry points are simpler and more findable than the production implementation, so
    "the only calls I can find are manual" is a common route to concluding a feature is unimplemented when it
    is merely owned elsewhere. Before reporting an absence, name where the production owner would be registered
    and check there.</hazard>
  <hazard name="tier_scoped_to_file">Attach an evidence tier to the passage cited, not to the file. One
    document can be accurate in its first half and describe classes, columns, and features that exist nowhere
    in its second; a specification section can be aspirational rather than descriptive. A check landing in a
    sound section raises the tier of that section only.</hazard>
  <hazard name="stale_recall">A pattern you remember from this codebase may have been removed. Verify it
    exists at the current ref before building an answer on it.</hazard>
</investigation_hazards>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load investigation-patterns when the question needs a hypothesis discharged rather than a fact
        located; it governs how evidence is gathered. Load fact-check as well when the question turns on
        external library or API behavior. A lookup needs neither.</action>
      <tool>Skill</tool>
      <output>Skills loaded, or why none was needed</output>
    </step>
    <step order="2">
      <action>Activate the Serena project and call list_memories. Read the entries whose names match this
        question's domain — {domain}-patterns, architecture-*, {project}-conventions. Read none if none match;
        the index alone is the answer then.</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>Memories read, or "nothing in the index matched"</output>
    </step>
  </phase>

  <phase name="analyze">
    <step order="1">
      <action>Restate the question in one sentence and name the claim that would answer it. Locate the code and
        documentation bearing on it, and state the boundary: what will be read and what is deliberately out of
        scope.</action>
      <tool>Glob, Grep, Serena get_symbols_overview</tool>
      <output>Restated question, candidate files, scope boundary</output>
    </step>
    <step order="2">
      <action>Decide what this question needs. Answer it yourself when the evidence is a handful of files you
        can read directly. Otherwise select the agents whose readings the question actually requires — explore
        for structure across an unfamiliar area, design for architecture and component relationships,
        performance for cost and bottlenecks, quality-assurance or code-quality for a judgment about the code
        itself — and dispatch them in one message. Name the agents not dispatched and why.</action>
      <output>The investigation plan, with the agents chosen and those deliberately skipped</output>
    </step>
  </phase>

  <phase name="investigate">
    <step order="1">
      <action>Execute the plan. Verify any external claim — library behavior, API contract, version support —
        against Context7 or the vendored source rather than recall.</action>
      <tool>Agent, Grep, Read, Context7</tool>
      <output>Findings with file:line</output>
    </step>
  </phase>
  <reflection_checkpoint id="investigation_quality">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The files read and the specific lines the answer will rest on.</check>
    <check>Each agent dispatched and the one claim it returned, or that it returned nothing usable.</check>
    <check>Any point where two readings disagree, and which cited a file:line.</check>
    <on_unmet>Widen the investigation or re-dispatch with specific paths. If only the user can settle it, ask
      with AskUserQuestion rather than picking a reading.</on_unmet>
  </reflection_checkpoint>

  <phase name="persist">
    <step order="1">
      <action>An investigation finding produces no work, so the same conclusion gets reached and written down
        repeatedly by sessions that never find each other. Call list_memories and search it for a prior
        recording of this same finding. If one exists, say in the answer that this is a repeat and cite it —
        that a finding has now been reached more than once is itself the argument for acting on it. Write a new
        memory only against the memory_policy triggers in CLAUDE.md; otherwise output "persist: no triggers
        matched — skip".</action>
      <tool>Serena list_memories, write_memory or edit_memory</tool>
      <output>Memory written or edited with whether this is a repeat, or the explicit skip</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Per gate_discipline in CLAUDE.md.</gate>
  <check>Any workflow phase skipped, and why.</check>
  <check>That no file was modified, or name the file that was.</check>
  <on_unmet>Resolve the gap before returning the answer.</on_unmet>
</reflection_checkpoint>

<agents>
  Dispatched by need, not by default. Each is read-only here and each finding it returns carries a file:line.

  <agent name="explore" subagent_type="explore">Structure and location across an unfamiliar area. Anything
    searched for and not found is reported as not found, with the pattern used.</agent>
  <agent name="design" subagent_type="design">Architectural relationships, dependency map, and the rationale
    and alternatives behind a pattern.</agent>
  <agent name="performance" subagent_type="performance">Bottlenecks and complexity, with file:line, or an
    explicit not-applicable.</agent>
  <agent name="quality-assurance" subagent_type="quality-assurance">Correctness and practice compliance across
    named files.</agent>
  <agent name="code-quality" subagent_type="code-quality">Complexity metrics and refactoring candidates ranked
    by impact.</agent>
  <agent name="validator" subagent_type="validator" dispatch="on_demand">Re-derive a disputed claim when two
    readings conflict and their evidence does not settle it.</agent>
</agents>

<decision_criteria>
  <factor name="evidence_quality" precedence="1">
    <unmet>A claim names no file:line and no command whose output shows it. Read the source and cite it, or tag
      the claim inferred or assumed and say what would confirm it.</unmet>
  </factor>
  <factor name="answer_completeness" precedence="2">
    <unmet>Part of the question is unanswered. Investigate it, or list it under gaps with the reason — never
      let it drop silently.</unmet>
  </factor>
  <factor name="source_verification" precedence="3">
    <unmet>An external claim rests on recall rather than on Context7 or the vendored source. Verify it before
      stating it as fact.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md, opening with the restated question and closing with the direct answer.
  Add: recommendations, as actions without implementation, when the answer implies any; and any claim first
  written as verified that could not name a command or file:line, with the tier it was moved to.
</output>
