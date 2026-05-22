---
argument-hint: [file-path]
description: Markdown text update command
---

<purpose>
Output results from other commands (/define, /ask, /bug, etc.) as markdown files.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="domain">technical-documentation</skill>
  <skill use="tools">serena-usage</skill>
</refs>
<rules priority="critical">
  <rule>Retrieve previous command execution results</rule>
  <rule>Determine output filename based on context</rule>
  <rule>Use specified file path if provided</rule>
  <rule>Never include revision history or discussion process</rule>
</rules>
<rules priority="standard">
  <rule>Keep output reproducible and file-scoped</rule>
  <rule>Preserve existing section semantics while formatting</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_execution" />
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
      <action>Activate Serena project with activate_project</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Check list_memories for documentation patterns</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="3">
      <action>Load applicable memories with read_memory</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <phase name="analyze">
    <step order="1">
      <action>What was the previous command?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>What is the appropriate output file?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="3">
      <action>Was a specific file path provided?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="4">
      <action>What content should be included/excluded?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <reflection_checkpoint id="analyze_quality">
    <question>Have I correctly identified the previous command and its output?</question>
    <question>Do I understand what content needs to be documented?</question>
    <threshold>If confidence less than 70, seek more evidence or ask user</threshold>
  </reflection_checkpoint>
  <phase name="gather">
    <step order="1">
      <action>Retrieve previous command results</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Collect relevant context</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
  <phase name="determine">
    <step order="1">
      <action>Determine output filename based on command type</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Check if user specified file path</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Detect and classify failures during command execution</action>
      <tool>Error analysis and severity assessment</tool>
      <output>Failure classification and impact summary</output>
    </step>
    <step order="2">
      <action>Apply recovery path or escalate with concrete blocker details</action>
      <tool>Retry policy and fallback strategy</tool>
      <output>Recovered flow or explicit blocker report</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
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
<decision_criteria inherits="core-patterns#decision_criteria">
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
    <test name="success_case">
      <input>content_accuracy=93, structure_quality=92, completeness=92</input>
      <calculation>(93*0.4)+(92*0.3)+(92*0.3) = 92.4</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>content_accuracy=80, structure_quality=80, completeness=80</input>
      <calculation>(80*0.4)+(80*0.3)+(80*0.3) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>content_accuracy=79, structure_quality=79, completeness=79</input>
      <calculation>(79*0.4)+(79*0.3)+(79*0.3) = 79</calculation>
      <expected_status>warning</expected_status>
      <reasoning>79 is below success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>content_accuracy=59, structure_quality=59, completeness=59</input>
      <calculation>(59*0.4)+(59*0.3)+(59*0.3) = 59</calculation>
      <expected_status>error</expected_status>
      <reasoning>59 is at error threshold</reasoning>
    </test>
    <test name="error_case">
      <input>content_accuracy=35, structure_quality=45, completeness=40</input>
      <calculation>(35*0.4)+(45*0.3)+(40*0.3) = 39.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores yield error status</reasoning>
    </test>
  </validation_tests>
</decision_criteria>
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
<error_escalation inherits="core-patterns#error_escalation">
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
