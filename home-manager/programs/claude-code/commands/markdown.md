---
argument-hint: [file-path]
description: Markdown text update command
agents:
  - name: docs
    description: Documentation management
    readonly: false
  - name: memory
    description: Knowledge base recording to Serena memory
    readonly: false
---

<purpose>
Output results from other commands (/define, /ask, /bug, etc.) as markdown files.
</purpose>

<instructions priority="critical">
<instruction>Retrieve previous command execution results</instruction>
<instruction>Determine output filename based on context</instruction>
<instruction>Use specified file path if provided</instruction>
<instruction>Never include revision history or discussion process</instruction>
</instructions>

<thinking_process>
<step>What was the previous command?</step>
<step>What is the appropriate output file?</step>
<step>Was a specific file path provided?</step>
<step>What content should be included/excluded?</step>
</thinking_process>

<workflow>
<phase name="retrieve">Get previous command results</phase>
<phase name="determine">Select output filename</phase>
<phase name="output">Write file using Write/Edit tool</phase>
</workflow>

<file_mapping>
<mapping previous_command="/define" output="EXECUTION.md" />
<mapping previous_command="/ask" output="RESEARCH.md" />
<mapping previous_command="/bug" output="RESEARCH.md" />
<mapping previous_command="other" output="MEMO.md" />
<note>User-specified file path takes precedence</note>
</file_mapping>

<constraints>
<must>Use context-appropriate filename</must>
<must>Respect user-specified file path</must>
<avoid>Including revision history/change logs</avoid>
<avoid>Including consideration process/discussion history</avoid>
</constraints>
