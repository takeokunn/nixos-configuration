---
name: markdown
description: Markdown text update command
---

<purpose>
Output results from other commands (/define, /execute, /feedback) as well-structured markdown files. Does not generate original content; translates structured command output into documentation format.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">technical-documentation</skill>
  <skill use="tools">serena-usage</skill>
</refs>
<rules priority="critical">
  <rule>Never generate original analysis or requirements; only format existing command output</rule>
  <rule>Never add revision history sections or timestamps to documents</rule>
  <rule>Preserve all technical content from the source command output; omit nothing</rule>
  <rule>Infer target file path from context; ask only if genuinely ambiguous</rule>
</rules>
<rules priority="standard">
  <rule>Use technical-documentation skill for document structure patterns</rule>
  <rule>Match the document style to the document type (requirements vs. task list vs. review report)</rule>
  <rule>Overwrite the target file if it exists; do not create a separate versioned copy</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Treating markdown generation as a separate creative task — the content is already determined by the source command; this command only reformats</practice>
    <practice>Adding editorial commentary or expanding content beyond the source — the markdown file must faithfully represent the source output without embellishment</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Determine the correct file path from context (e.g., the task name or the file being documented) rather than asking; ask only if genuinely ambiguous</principle>
    <principle>Preserve the full content of the source output; every requirement, finding, or task must appear in the markdown — no silent omissions</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Identify source command output to be formatted (most recent /define, /execute, /feedback, etc.)</action>
      <tool>Conversation context</tool>
      <output>Source content confirmed</output>
    </step>
    <step order="2">
      <action>Determine target file path from context; confirm with user only if genuinely ambiguous</action>
      <tool>Context analysis; AskUserQuestion if ambiguous</tool>
      <output>Target file path</output>
    </step>
    <step order="3">
      <action>Select document template based on source type (requirements / task breakdown / review report)</action>
      <tool>technical-documentation skill pattern matching</tool>
      <output>Template selected</output>
    </step>
  </phase>
  <phase name="write">
    <step order="1">
      <action>Format source content using selected template; ensure all sections are present</action>
      <tool>Write tool</tool>
      <output>Markdown file written to target path</output>
    </step>
    <step order="2">
      <action>Verify written file matches source content; flag any omissions</action>
      <tool>Read tool</tool>
      <output>Verification complete or omission list</output>
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
    <confirmation>File path written and size in lines</confirmation>
    <omissions>Any source content not included (must be empty for complete output)</omissions>
  </format>
</output>
<constraints>
  <must>Write all source content to the markdown file without omission</must>
  <must>Overwrite existing target file rather than creating a versioned copy</must>
  <avoid>Generating original analysis or new requirements</avoid>
  <avoid>Adding revision history or timestamps</avoid>
  <avoid>Expanding or editorializing source content</avoid>
</constraints>
