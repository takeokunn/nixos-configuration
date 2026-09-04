---
argument-hint: [file-path]
description: Markdown text update command
---

<purpose>
Write the previous command's result (/define, /ask, /bug, …) to a markdown file.
</purpose>

<rules priority="critical">
  <rule>Never include a timestamp, revision history, change log, or discussion trace: the document is read later
    as fact, and a preserved deliberation trail reads as current — a timestamp makes drift look dated rather
    than wrong, so the reader trusts stale content over current.</rule>
  <rule>Write to the user's given path — a decision, not a default to improve on.</rule>
</rules>
<rules priority="standard">
  <rule>Everything written traces to the previous command's output or a file read this session — anything
    tracing to neither is verified against the source or cut, never softened into a hedge.</rule>
  <rule>Check each code example against its source file — one that no longer matches is worse than none, since
    the reader trusts and acts on it.</rule>
  <rule>Reformatting may change presentation; it may not change what a section claims.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <action>Load technical-documentation only when the output needs more than transcription — heading hierarchy,
      section ordering, or presented code examples. Nothing else in context governs those decisions. A plain
      transcription needs no skill and no memory read.</action>
    <tool>Skill</tool>
    <output>Skill loaded, or that this run is a plain transcription</output>
  </phase>
  <phase name="determine">
    <step order="1">
      <action>Identify the previous command and the section being documented. Resolve the target path — the
        user's if given, else file_mapping's, so repeat runs of the same command land in one file instead of
        near-duplicates. Read the target if it exists.</action>
      <output>Previous command, target path with its source, and what the path currently holds</output>
    </step>
    <step order="2">
      <action>Separate signal from process: keep conclusions, specifications, and decisions; drop deliberation
        and revision history. Anything present in the previous output but absent from the draft is added, or
        named under gaps with the reason.</action>
      <output>What is kept and what is dropped</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Per gate_discipline in CLAUDE.md.</gate>
  <check>The path about to be written, whether it exists, and what it currently holds.</check>
  <check>The document's headings — confirming none introduces a timestamp, revision history, or discussion
    trace.</check>
  <on_unmet>Do not write — resolve the conflict or strip the prohibited content, or ask for the path with
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
  Follows output_contract in CLAUDE.md — summary names the file written and whether created or overwritten; gaps
    names content deliberately excluded from the previous command, and why.
</output>
