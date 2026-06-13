---
name: remember
description: Memory review and organization command
---

<purpose>
Review, audit, and organize all memory layers (Serena memories, CLAUDE.md rules, session context). Presents proposals for any changes before making them; read-only until the user approves.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="tools">serena-usage</skill>
</refs>
<rules priority="critical">
  <rule>Read-only until user explicitly approves proposed changes</rule>
  <rule>Never delete or modify memories without presenting a proposal and receiving user confirmation</rule>
  <rule>Present all proposals in a single structured list before taking any action</rule>
  <rule>After approval, apply all approved changes in one pass</rule>
</rules>
<rules priority="standard">
  <rule>Audit all memory layers: Serena memories, CLAUDE.md rules, session notes</rule>
  <rule>Identify duplicates, stale entries, contradictions, and missing entries</rule>
  <rule>Propose consolidations and updates; do not silently discard</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Reviewing memory entries one at a time in sequential rounds — AI can audit all layers simultaneously and surface a complete proposal in a single pass</practice>
    <practice>Treating memory review as lower priority than active tasks — accurate memory directly improves the quality of all future task execution</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Audit all layers in parallel; produce a single consolidated proposal rather than interleaving read and write steps</principle>
    <principle>Every proposed change must state the reason (duplicate, stale, contradicts current code, missing topic); unexplained changes will not be accepted</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="audit">
    <step order="1">
      <action>List all Serena memories with list_memories</action>
      <tool>Serena list_memories</tool>
      <output>Full memory index</output>
    </step>
    <step order="2">
      <action>Read all memories to assess content quality, relevance, and accuracy</action>
      <tool>Serena read_memory (all entries)</tool>
      <output>Full memory content for all entries</output>
    </step>
    <step order="3">
      <action>Identify: duplicates, stale/outdated entries, contradictions between entries, missing topics</action>
      <tool>Analysis (no external tools needed)</tool>
      <output>Categorized list of issues with evidence</output>
    </step>
  </phase>
  <phase name="propose">
    <step order="1">
      <action>Compile all proposed changes into a structured proposal:
        - KEEP (no change needed, reason)
        - UPDATE (existing entry, what changes and why)
        - MERGE (two entries to consolidate, result)
        - DELETE (stale/irrelevant entry, reason)
        - CREATE (missing topic, content outline)</action>
      <tool>AskUserQuestion with structured proposal</tool>
      <output>User approval or rejection of each proposed change</output>
    </step>
  </phase>
  <phase name="apply">
    <step order="1">
      <action>Apply all approved changes in one pass; skip rejected items</action>
      <tool>Serena edit_memory, write_memory, delete_memory</tool>
      <output>Summary of applied changes</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<output>
  <format>
    <audit_summary>
      <total_memories>Count reviewed</total_memories>
      <issues_found>Categorized issue list with evidence</issues_found>
    </audit_summary>
    <proposal>
      <change action="UPDATE|MERGE|DELETE|CREATE" target="memory_name" reason="why">Proposed content or description</change>
    </proposal>
    <applied_changes>Changes made after user approval</applied_changes>
  </format>
</output>
<constraints>
  <must>Present all proposals before making any changes</must>
  <must>Require explicit user confirmation before modifying or deleting any memory</must>
  <must>Justify every proposed change with a reason</must>
  <avoid>Modifying memories without user approval</avoid>
  <avoid>Silently discarding information that may still be relevant</avoid>
</constraints>
