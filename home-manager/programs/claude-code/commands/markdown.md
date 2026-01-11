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

<parallelization>
  <capability>
    <parallel_safe>true</parallel_safe>
    <read_only>false</read_only>
    <modifies_state>local</modifies_state>
  </capability>
  <execution_strategy>
    <max_parallel_agents>16</max_parallel_agents>
    <timeout_per_agent>180000</timeout_per_agent>
  </execution_strategy>
</parallelization>

<decision_criteria>
  <criterion name="confidence_calculation">
    <factor name="content_accuracy" weight="0.4">
      <score range="90-100">All content verified against source</score>
      <score range="70-89">Core content verified</score>
      <score range="50-69">Partial verification</score>
      <score range="0-49">Unverified content</score>
    </factor>
    <factor name="structure_quality" weight="0.3">
      <score range="90-100">Clear hierarchy, proper formatting</score>
      <score range="70-89">Good structure</score>
      <score range="50-69">Basic structure</score>
      <score range="0-49">Poor structure</score>
    </factor>
    <factor name="completeness" weight="0.3">
      <score range="90-100">All requested content included</score>
      <score range="70-89">Main content included</score>
      <score range="50-69">Partial content</score>
      <score range="0-49">Incomplete</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="clear_delegation">
      <input>content_accuracy=95, structure_quality=90, completeness=95</input>
      <calculation>(95*0.4)+(90*0.3)+(95*0.3) = 38+27+28.5 = 93.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>Verified content with clear structure yields high confidence</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>content_accuracy=80, structure_quality=75, completeness=80</input>
      <calculation>(80*0.4)+(75*0.3)+(80*0.3) = 32+22.5+24 = 78.5</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Core content verified but some structural issues results in 78.5, triggers warning</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>content_accuracy=85, structure_quality=75, completeness=80</input>
      <calculation>(85*0.4)+(75*0.3)+(80*0.3) = 34+22.5+24 = 80.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>Weighted average 80.5, meets success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>content_accuracy=60, structure_quality=55, completeness=60</input>
      <calculation>(60*0.4)+(55*0.3)+(60*0.3) = 24+16.5+18 = 58.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Weighted average 58.5 is below 60, triggers error</reasoning>
    </test>
    <test name="unverified_content">
      <input>content_accuracy=50, structure_quality=55, completeness=45</input>
      <calculation>(50*0.4)+(55*0.3)+(45\*0.3) = 20+16.5+13.5 = 50</calculation>
      <expected_status>error</expected_status>
      <reasoning>Unverified content with poor structure results in 50, triggers error</reasoning>
    </test>
  </validation_tests>
</decision_criteria>

<workflow>
  <phase name="analyze">
    <objective>Understand previous command output and context</objective>
    <step>1. What was the previous command?</step>
    <step>2. What is the appropriate output file?</step>
    <step>3. Was a specific file path provided?</step>
    <step>4. What content should be included/excluded?</step>
  </phase>
  <reflection_checkpoint id="analyze_quality">
    <question>Have I correctly identified the previous command and its output?</question>
    <question>Do I understand what content needs to be documented?</question>
    <threshold>If confidence less than 70, seek more evidence or ask user</threshold>
  </reflection_checkpoint>
  <phase name="gather">
    <objective>Retrieve all relevant information for documentation</objective>
    <step>1. Retrieve previous command results</step>
    <step>2. Collect relevant context</step>
  </phase>
  <reflection_checkpoint id="analysis_quality">
    <question>Have I gathered sufficient evidence to proceed?</question>
    <question>Are there gaps in my understanding?</question>
    <threshold>If confidence less than 70, seek more evidence or ask user</threshold>
  </reflection_checkpoint>
  <phase name="determine">
    <objective>Decide on output file location and structure</objective>
    <step>1. Determine output filename based on command type</step>
    <step>2. Check if user specified file path</step>
  </phase>
  <phase name="failure_handling">
    <objective>Handle errors and edge cases gracefully</objective>
    <step>1. If sub-agent fails: Log error, attempt alternative approach</step>
    <step>2. If data unavailable: Document gap, proceed with partial analysis</step>
    <step>3. If contradictory evidence: Flag uncertainty, request user clarification</step>
  </phase>
  <phase name="execute">
    <objective>Write the documentation to file</objective>
    <step>1. Write file using Write/Edit tool</step>
    <step>2. Verify output format</step>
  </phase>
</workflow>

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

<output>
  <format>
    <markdown_file>
      <header>Title based on command output</header>
      <content>Cleaned, formatted output from previous command</content>
      <footer>Optional: Related references</footer>
    </markdown_file>
  </format>
</output>

<enforcement>
  <mandatory_behaviors>
    <behavior id="MD-B001" priority="critical">
      <trigger>Before writing documentation</trigger>
      <action>Understand the source material</action>
      <verification>Source analysis in output</verification>
    </behavior>
    <behavior id="MD-B002" priority="critical">
      <trigger>When including code examples</trigger>
      <action>Verify examples are correct and runnable</action>
      <verification>Example validation noted</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="MD-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Adding timestamps to documents</action>
      <response>Block operation, timestamps are prohibited</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<error_escalation>
  <level severity="low">
    <example>Minor formatting inconsistency in output</example>
    <action>Note in report, proceed</action>
  </level>
  <level severity="medium">
    <example>Unclear output destination or ambiguous file mapping</example>
    <action>Document issue, use AskUserQuestion for clarification</action>
  </level>
  <level severity="high">
    <example>File path conflict or overwrite risk</example>
    <action>STOP, present options to user</action>
  </level>
  <level severity="critical">
    <example>Risk of overwriting critical documentation</example>
    <action>BLOCK operation, require explicit user acknowledgment</action>
  </level>
</error_escalation>

<related_commands>
  <command name="define">Primary source for EXECUTION.md output</command>
  <command name="ask">Primary source for RESEARCH.md output</command>
  <command name="bug">Primary source for RESEARCH.md output</command>
</related_commands>

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
