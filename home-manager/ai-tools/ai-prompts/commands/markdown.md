---
argument-hint: [file-path]
description: Markdown text update command
---

<purpose>
Write the previous command's result (/define, /ask, /bug, …) to a markdown file.
</purpose>

<rules priority="critical">
  <rule>Never include a timestamp, revision history, change log, or discussion trace. The document is read
    later as a statement of what is true: a preserved deliberation trail reads as current fact, and a timestamp
    makes a drifted document look dated rather than wrong, so the reader discounts current content and trusts
    stale content.</rule>
  <rule>Write to the path the user gave. Their path is a decision, not a default to improve on.</rule>
</rules>
<rules priority="standard">
  <rule>Everything written traces to the previous command's output or to a file read this session. A statement
    or code example tracing to neither is verified against the source or cut — never softened into a hedge.</rule>
  <rule>Check each code example against the file it came from. An example that no longer matches its source is
    worse than no example, because a reader trusts it and acts on it.</rule>
  <rule>Reformatting may change presentation; it may not change what a section claims.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load technical-documentation only when the output needs more than transcription — a heading
        hierarchy, section ordering, or presented code examples. That skill governs those decisions and nothing
        else in context does. A plain transcription needs no skill and no memory read.</action>
      <tool>Skill</tool>
      <output>Skill loaded, or that this run is a plain transcription</output>
    </step>
  </phase>
  <phase name="determine">
    <step order="1">
      <action>Identify the previous command and the section of its output being documented. Resolve the target
        path: the user's if they gave one, otherwise from file_mapping, so two runs of the same command land in
        the same file instead of accumulating near-duplicates. Read the target if it already exists.</action>
      <output>Previous command, target path with its source, and what the path currently holds</output>
    </step>
    <step order="2">
      <action>Separate signal from process: keep conclusions, specifications, and decisions; drop deliberation
        and revision history. A conclusion, specification, or decision present in the previous output and absent
        from the draft is added, or named under gaps with the reason.</action>
      <output>What is kept and what is dropped</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Per gate_discipline in CLAUDE.md.</gate>
  <check>The path about to be written, whether it exists, and what it currently holds.</check>
  <check>The document's headings, confirming none introduces a timestamp, revision history, or discussion
    trace.</check>
  <on_unmet>Do not write. Resolve the conflict or strip the prohibited content first, or ask for the path with
    AskUserQuestion.</on_unmet>
</reflection_checkpoint>

<file_mapping>
  <default_output_dir>project root</default_output_dir>
  <mapping command="/define" output="EXECUTION.md" />
  <mapping command="/ask" output="RESEARCH.md" />
  <mapping command="/bug" output="RESEARCH.md" />
  <mapping command="other" output="MEMO.md" />
  <note>A user-specified path takes precedence.</note>
</file_mapping>

<output>
  Follows output_contract in CLAUDE.md. summary names the file written and whether it was created or
  overwritten; gaps names content from the previous command deliberately excluded, and why.
</output>
