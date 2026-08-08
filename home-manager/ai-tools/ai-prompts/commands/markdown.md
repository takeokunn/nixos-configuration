---
argument-hint: [file-path]
description: Markdown text update command
---

<purpose>
Output results from other commands (/define, /ask, /bug, etc.) as markdown files.
</purpose>
<rules priority="critical">
  <rule>Never include revision history, change logs, or discussion traces, because the document is read
    later as a statement of what is true, and a preserved deliberation trail is read as current fact.</rule>
  <rule>Write to the user-specified path when one was given, because the user's path is a decision, not
    a default to be improved on.</rule>
</rules>
<rules priority="standard">
  <rule>Resolve the output filename from the file_mapping table when the user gave no path, so that two
    runs of the same command land in the same file instead of accumulating near-duplicates.</rule>
  <rule>Keep the output reproducible and file-scoped: everything written traces to the previous
    command's output or to a file read this session.</rule>
  <rule>Preserve existing section semantics while formatting. Reformatting may change presentation; it
    may not change what a section claims.</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Manually reformatting content section by section — AI can analyze the entire previous command output and determine the correct structure in a single analysis pass</practice>
    <practice>Including revision history, change logs, or discussion traces in the output — documentation must be clean, forward-looking, and free of session artifacts</practice>
    <practice>Choosing filenames based on convention alone — AI should infer the correct filename from command type and content context, with user-specified paths taking precedence</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Extract only the signal from the previous command output: conclusions, specifications, and decisions — never the deliberation process or revision history</principle>
    <principle>Verify every code example in the documentation is syntactically correct before writing; stale or broken examples erode trust in documentation</principle>
    <principle>Select the appropriate output filename from the command type mapping (define→EXECUTION.md, ask/bug→RESEARCH.md, other→MEMO.md) unless the user explicitly specified a path</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>If the output is more than a transcription of the previous command — that is, if it needs
        a heading hierarchy, section ordering, or presented code examples — load the
        technical-documentation skill with the Skill tool, because that skill governs those decisions
        and nothing else in context does. A skill named in a reference attribute is not loaded; only
        the Skill tool loads it.</action>
      <tool>Skill</tool>
      <output>Skill loaded, or the statement that this run is a plain transcription and does not need it</output>
    </step>
    <step order="2">
      <action>Initialize Serena and load the documentation-pattern memories that match, following the
        serena-usage skill for the procedure</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>The memories read, named; or an explicit "nothing matched"</output>
    </step>
  </phase>
  <phase name="analyze">
    <step order="1">
      <action>What was the previous command?</action>
      <output>Command name, and where its output sits in the session</output>
    </step>
    <step order="2">
      <action>What is the appropriate output file?</action>
      <tool>file_mapping table in this command</tool>
      <output>Target filename</output>
    </step>
    <step order="3">
      <action>Was a specific file path provided?</action>
      <output>User-supplied path, or none</output>
    </step>
    <step order="4">
      <action>What content should be included/excluded?</action>
      <output>Content kept (conclusions, specifications, decisions) and content dropped (deliberation, revision history)</output>
    </step>
  </phase>
  <reflection_checkpoint id="analyze_quality">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name the previous command and the section of its output being documented.</check>
    <check>Name the target path, and say whether it came from the user or from file_mapping.</check>
    <check>Name what is being dropped as deliberation or revision history.</check>
    <on_unmet>Do not write. Re-read the previous command's output, or ask for the path with AskUserQuestion.</on_unmet>
  </reflection_checkpoint>
  <phase name="gather">
    <step order="1">
      <action>Retrieve previous command results</action>
      <output>Full text of the prior command's output</output>
    </step>
    <step order="2">
      <action>Collect relevant context</action>
      <tool>Read, Grep</tool>
      <output>Source files backing each code example and claim</output>
    </step>
  </phase>
  <phase name="determine">
    <step order="1">
      <action>Determine output filename based on command type</action>
      <tool>file_mapping table in this command</tool>
      <output>Resolved filename</output>
    </step>
    <step order="2">
      <action>Check if user specified file path</action>
      <output>Final path; a user-specified path wins</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
  <check>Name the path about to be written, and state whether it already exists and what it currently holds.</check>
  <check>Quote the document's headings, and confirm none of them introduces a timestamp, revision history, or discussion trace (MD-P001).</check>
  <on_unmet>Do not write. Resolve the conflict or strip the prohibited content first.</on_unmet>
</reflection_checkpoint>
<agents>
  <agent name="docs" subagent_type="docs" readonly="false">Documentation management</agent>
  <agent name="memory" subagent_type="general-purpose" readonly="false">Knowledge base recording to Serena memory</agent>
</agents>
<execution_graph>
  <sequential_phase id="output" depends_on="none">
    <agent>docs</agent>
    <reason>Creates markdown file first</reason>
  </sequential_phase>
  <sequential_phase id="memory_recording" depends_on="output">
    <agent>memory</agent>
    <reason>Records to knowledge base after file creation</reason>
  </sequential_phase>
</execution_graph>
<decision_criteria>
  <factor name="content_accuracy" precedence="1">
    <unmet>A statement or code example in the draft traces to neither the previous command's output nor
      a file read this session. Verify it against the source, or cut it — do not soften it into a hedge.</unmet>
  </factor>
  <factor name="completeness" precedence="2">
    <unmet>A conclusion, specification, or decision present in the previous command's output is absent
      from the draft. Add it, or name it in the gaps section with the reason it was excluded.</unmet>
  </factor>
  <factor name="structure_quality" precedence="3">
    <unmet>A section's content does not match its heading, or the document has no heading hierarchy.
      Restructure before writing.</unmet>
  </factor>
  <resolution>Apply in precedence order. The first factor whose `unmet` condition holds decides what
    happens next; later factors are not consulted.</resolution>
</decision_criteria>
<output>
  <format>
    <markdown_file>
      <header>Title based on command output</header>
      <content>Cleaned, formatted output from previous command</content>
      <footer>Optional: Related references</footer>
    </markdown_file>
    <report_to_user>
      <path>The file written, and whether it was created or overwritten</path>
      <gaps>Content from the previous command that was deliberately excluded, and why</gaps>
    </report_to_user>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="MD-B001" priority="standard">
      <trigger>Before writing documentation</trigger>
      <action>Read the source material the document will restate, rather than working from a summary of
        it, so that the written claims can be traced back rather than reconstructed</action>
      <verification>Source analysis in output</verification>
    </behavior>
    <behavior id="MD-B002" priority="high">
      <trigger>When including code examples</trigger>
      <action>Check each example against the file it came from. An example that no longer matches the
        source is worse than no example, because a reader trusts it and acts on it</action>
      <verification>Example validation noted</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="MD-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Adding timestamps to documents</action>
      <response>Block operation. A timestamp makes the document look dated rather than wrong once it
        drifts, so the reader discounts current content and trusts stale content.</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation>
  <examples>
    <example severity="low">Minor formatting inconsistency in output</example>
    <example severity="medium">Unclear output destination or ambiguous file mapping</example>
    <example severity="high">File path conflict or overwrite risk</example>
    <example severity="critical">Risk of overwriting critical documentation</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="define">Primary source for EXECUTION.md output</command>
  <command name="ask">Primary source for RESEARCH.md output</command>
  <command name="bug">Primary source for RESEARCH.md output</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="technical-documentation">Formatting and structuring markdown output</skill>
  <skill name="serena-usage">Recording knowledge to memory when appropriate</skill>
</related_skills>
<file_mapping>
  <default_output_dir>project root</default_output_dir>
  <mapping command="/define" output="EXECUTION.md" />
  <mapping command="/ask" output="RESEARCH.md" />
  <mapping command="/bug" output="RESEARCH.md" />
  <mapping command="other" output="MEMO.md" />
  <note>User-specified file path takes precedence</note>
</file_mapping>
<constraints>
  <must>Use context-appropriate filename</must>
  <must>Respect user-specified file path</must>
  <avoid>Including revision history/change logs</avoid>
  <avoid>Including consideration process/discussion history</avoid>
</constraints>
