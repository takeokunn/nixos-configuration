---
argument-hint: [question]
description: Question and inquiry command
---

<purpose>
Answer a question about this project from evidence in it. Read-only: never modifies files.
</purpose>

<rules priority="critical">
  <rule>Never modify, create, delete, or fix anything: editing removes the user's decision. Serena write_memory
    is permitted: not a file write.</rule>
  <rule>Never justify the user's assumption: if evidence contradicts it, answer what the evidence supports and
    name both.</rule>
  <rule>Never answer from training data alone: recall reads as evidence, so the reader can't discount
    it.</rule>
</rules>
<rules priority="standard">
  <rule>Every finding carries a file:line so the reader can open it instead of re-investigating.</rule>
  <rule>Scale investigation to the question: a lookup (is X defined, does Y exist, what calls Z) is answered by
    your own Grep and Read, since an agent costs more than the work. Dispatch agents for subsystem-spanning
    questions, independent readings, or architecture/performance/quality judgment; send several in one
    message.</rule>
</rules>

<investigation_hazards>
  Four ways an investigation reaches a confident wrong answer; each has occurred here.

  <hazard name="generated_document_as_source">A document and its generator differ in reliability: the generator
    is evidence, the document a claim. Schema snapshots, OpenAPI files, generated clients, and architecture
    diagrams answer in the exact form asked and surface first; dangerous, since they go stale silently. A
    verified tier cites the generator itself (migration, handler, model), never the document describing
    it.</hazard>
  <hazard name="call_site_role">A call site proves a path exists, not its role: debug hooks and QA controls are
    easier to find than production code, so "only manual calls found" often means the feature lives elsewhere,
    not that it's unbuilt. Before reporting an absence, name and check where the production owner would be
    registered.</hazard>
  <hazard name="tier_scoped_to_file">Tier the passage cited, not the file: one document can be accurate in its
    first half and describe nonexistent classes, columns, or features in its second, and a spec section can be
    aspirational rather than descriptive. A check in a sound section raises only that section's tier.</hazard>
  <hazard name="stale_recall">A remembered pattern may have been removed, so verify it exists at the current ref
    before building on it.</hazard>
</investigation_hazards>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load investigation-patterns for a hypothesis to discharge, not a fact to locate: it governs
        evidence-gathering. Load fact-check too for external library or API behavior; a lookup needs
        neither.</action>
      <tool>Skill</tool>
      <output>Skills loaded, or why none was needed</output>
    </step>
    <step order="2">
      <action>Activate the Serena project, call list_memories, and read entries matching this question's domain
        ({domain}-patterns, architecture-*, {project}-conventions), none if none match, since the index alone
        answers then.</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>Memories read, or "nothing in the index matched"</output>
    </step>
  </phase>

  <phase name="analyze">
    <step order="1">
      <action>Restate the question in one sentence, name the claim answering it, locate the bearing code and
        documentation, and state the boundary: what's read, what's deliberately out of scope.</action>
      <tool>Glob, Grep, Serena get_symbols_overview</tool>
      <output>Restated question, candidate files, scope boundary</output>
    </step>
    <step order="2">
      <action>Answer it yourself for a few directly-readable files. Otherwise pick the agents the question
        requires (explore for structure, design for architecture and components, performance for cost and
        bottlenecks, quality-assurance or code-quality for judgment on the code), dispatched in one message.
        Name the skipped agents and why.</action>
      <output>The investigation plan, with the agents chosen and those deliberately skipped</output>
    </step>
  </phase>

  <phase name="investigate">
    <action>Execute the plan, verifying external claims (library behavior, API contract, version support)
      against Context7 or the vendored source, not recall.</action>
    <tool>Agent, Grep, Read, Context7</tool>
    <output>Findings with file:line</output>
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
    <action>An investigation finding produces no work, so sessions that never meet reach and record the same
      conclusion repeatedly. Search list_memories for a prior recording; if found, flag the answer as a repeat
      and cite it: recurrence is itself the argument for acting. Write a new memory only against
      memory_policy's triggers in CLAUDE.md; otherwise output "persist: no triggers matched, skip".</action>
    <tool>Serena list_memories, write_memory or edit_memory</tool>
    <output>Memory written or edited with whether this is a repeat, or the explicit skip</output>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <check>Any workflow phase skipped, and why.</check>
  <check>That no file was modified, or name the file that was.</check>
  <on_unmet>Resolve the gap before returning the answer.</on_unmet>
</reflection_checkpoint>

<agents>
  Dispatched by need, not by default. Each is read-only here; each finding carries a file:line.

  <agent name="explore" subagent_type="explore">Structure and location across an unfamiliar area: reports
    what's searched for and not found, with the pattern used.</agent>
  <agent name="design" subagent_type="design">Architectural relationships, dependency map, rationale and
    alternatives behind a pattern.</agent>
  <agent name="performance" subagent_type="performance">Bottlenecks and complexity, with file:line, or an
    explicit not-applicable.</agent>
  <agent name="quality-assurance" subagent_type="quality-assurance">Correctness and practice compliance across
    named files.</agent>
  <agent name="code-quality" subagent_type="code-quality">Complexity metrics and refactoring candidates ranked
    by impact.</agent>
  <agent name="validator" subagent_type="validator" dispatch="on_demand">Re-derive a disputed claim when two
    readings conflict and their evidence doesn't settle it.</agent>
</agents>

<decision_criteria>
  <factor name="evidence_quality" precedence="1">
    <unmet>A claim names no file:line or command output: read the source and cite it, or tag it inferred or
      assumed with what would confirm it.</unmet>
  </factor>
  <factor name="answer_completeness" precedence="2">
    <unmet>Part of the question is unanswered: investigate it, or list it under gaps with the reason; never let
      it drop silently.</unmet>
  </factor>
  <factor name="source_verification" precedence="3">
    <unmet>An external claim rests on recall, not Context7 or the vendored source: verify it before stating it
      as fact.</unmet>
  </factor>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md, opening with the restated question and closing with the direct answer.
    Add: recommendations, as actions without implementation, when implied, and any claim first written as
    verified that couldn't name a command or file:line, with the tier it moved to.
</output>
