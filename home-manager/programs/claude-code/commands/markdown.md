---
argument-hint: [file-path]
description: Markdown text update command
---

<purpose>
Output results from other commands (/define, /ask, /bug, etc.) as markdown files.
</purpose>

<rules priority="critical">
<rule>Retrieve previous command execution results</rule>
<rule>Determine output filename based on context</rule>
<rule>Use specified file path if provided</rule>
<rule>Never include revision history or discussion process</rule>
</rules>

<workflow>
<phase name="analyze">
<step>What was the previous command?</step>
<step>What is the appropriate output file?</step>
<step>Was a specific file path provided?</step>
<step>What content should be included/excluded?</step>
</phase>
<phase name="gather">
<step>Retrieve previous command results</step>
<step>Collect relevant context</step>
</phase>
<phase name="determine">
<step>Determine output filename based on command type</step>
<step>Check if user specified file path</step>
</phase>
<phase name="execute">
<step>Write file using Write/Edit tool</step>
<step>Verify output format</step>
</phase>
</workflow>

<agents>
<agent name="docs" subagent_type="docs" readonly="false">Documentation management</agent>
<agent name="memory" subagent_type="general-purpose" readonly="false">Knowledge base recording to Serena memory</agent>
</agents>

<parallel_execution>
<group name="output" execution="sequential">
<agent>docs</agent>
<agent>memory</agent>
</group>
<note>Sequential execution: docs creates file, then memory records if needed</note>
</parallel_execution>

<output>
<format>
<markdown_file>
<header>Title based on command output</header>
<content>Cleaned, formatted output from previous command</content>
<footer>Optional: Related references</footer>
</markdown_file>
</format>
</output>

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
