---
name: Workflow Patterns
description: Patterns for output formats, reflection checkpoints, agent references, and self-evaluation shared across agents and commands.
version: 1.0.0
---

<purpose>
  Provide standardized patterns for output formatting, workflow checkpoints, agent references, and self-evaluation shared across agents and commands.
</purpose>

<tools>
  <tool name="output_format_template">
    <description>Standard agent output format with status criteria</description>
    <use_case>Include in agents for consistent output structure</use_case>
  </tool>

  <tool name="reflection_checkpoint_template">
    <description>Standard analysis quality checkpoint for workflow phases</description>
    <use_case>Include in workflows for consistent quality gates</use_case>
  </tool>

  <tool name="self_evaluate_template">
    <description>Standard self-evaluation phase for agents producing reports</description>
    <use_case>Include in agents for consistent quality assessment</use_case>
  </tool>

  <tool name="prepare_phase_template">
    <description>Standard Serena initialization phase for commands</description>
    <use_case>Include at start of command workflows for Serena initialization</use_case>
  </tool>

  <tool name="serena_validation_template">
    <description>Serena validation elements for reflection checkpoints</description>
    <use_case>Include in checkpoints before code modifications or after investigations</use_case>
  </tool>
</tools>

<patterns>
  <pattern name="output_format">
    <description>Standard agent output format with status criteria</description>
    <example>
<output>
  <format>
{
  "status": "success|warning|error",
  "status_criteria": {
    "success": "All checks passed, confidence >= 80",
    "warning": "Minor issues OR confidence 60-79",
    "error": "Critical issues OR confidence less than 60"
  },
  "confidence": 0,
  "summary": "Brief summary of results",
  "metrics": {},
  "findings": [],
  "next_actions": []
}
  </format>
</output>
    </example>
  </pattern>

  <pattern name="output_status_criteria">
    <description>Standard status criteria for agent output format</description>
    <example>
"status_criteria": {
  "success": "All checks passed, confidence >= 80",
  "warning": "Minor issues OR confidence 60-79",
  "error": "Critical issues OR confidence less than 60"
}
    </example>
  </pattern>

  <pattern name="reflection_checkpoint">
    <description>Standard analysis quality checkpoint for workflow phases</description>
    <example>
<reflection_checkpoint id="analysis_quality">
  <question>Have I gathered sufficient evidence to proceed?</question>
  <question>Are there gaps in my understanding?</question>
  <threshold>If confidence less than 70, seek more evidence or ask user</threshold>
</reflection_checkpoint>
    </example>
  </pattern>

  <pattern name="prepare_phase">
    <description>Standard Serena initialization phase for workflows</description>
    <example>
<phase name="prepare">
  <objective>Initialize Serena and check existing patterns</objective>
  <step>1. Activate Serena project with activate_project</step>
  <step>2. Check list_memories for relevant patterns</step>
  <step>3. Load applicable memories with read_memory</step>
</phase>
    </example>
  </pattern>

  <pattern name="serena_validation_investigation">
    <description>Serena validation for investigation/analysis checkpoints</description>
    <example>
<serena_validation>
  <tool>think_about_collected_information</tool>
  <trigger>After investigation phase completes</trigger>
</serena_validation>
    </example>
  </pattern>

  <pattern name="serena_validation_pre_edit">
    <description>Serena validation before code modification operations</description>
    <example>
<serena_validation>
  <tool>think_about_task_adherence</tool>
  <trigger>Before code modification</trigger>
</serena_validation>
    </example>
  </pattern>

  <pattern name="serena_validation_completion">
    <description>Serena validation before reporting task completion</description>
    <example>
<serena_validation>
  <tool>think_about_whether_you_are_done</tool>
  <trigger>Before reporting completion to user</trigger>
</serena_validation>
    </example>
  </pattern>

  <pattern name="reflection_workflow_steps">
    <description>Mandatory reflection tool calls as explicit workflow steps (not just checkpoint definitions)</description>
    <example>
<!-- After any search/investigation sequence (3+ operations) -->
<step order="N">
  <action>Validate search completeness</action>
  <tool>Serena think_about_collected_information</tool>
  <output>Search quality validation</output>
  <mandatory>true</mandatory>
</step>

<!-- Before any code modification operation -->
<step order="N">
  <action>Validate task adherence</action>
  <tool>Serena think_about_task_adherence</tool>
  <output>Task alignment validation</output>
  <mandatory>true</mandatory>
</step>

<!-- Before final response/completion -->
<step order="N">
  <action>Validate task completion</action>
  <tool>Serena think_about_whether_you_are_done</tool>
  <output>Completion validation</output>
  <mandatory>true</mandatory>
</step>
    </example>
    <triggers>
      <trigger tool="think_about_collected_information">After search sequence of 3+ operations (find_symbol, search_for_pattern, get_symbols_overview, Grep, Glob)</trigger>
      <trigger tool="think_about_task_adherence">Before any symbol editing (replace_symbol_body, insert_before_symbol, insert_after_symbol, rename_symbol) or file modification</trigger>
      <trigger tool="think_about_whether_you_are_done">Before returning final result to user</trigger>
    </triggers>
    <on_failure>
      <think_about_collected_information>Expand search scope, use alternative search strategies, or ask user for clarification</think_about_collected_information>
      <think_about_task_adherence>Review requirements, document deviation rationale, or ask user before proceeding</think_about_task_adherence>
      <think_about_whether_you_are_done>Identify incomplete items, iterate on missing work, or report partial completion with remaining items</think_about_whether_you_are_done>
    </on_failure>
    <failure_detection>
      <think_about_collected_information>Tool output indicates incomplete coverage, missing critical areas, or low confidence in search results</think_about_collected_information>
      <think_about_task_adherence>Tool output indicates task deviation, requirements mismatch, or scope creep detected</think_about_task_adherence>
      <think_about_whether_you_are_done>Tool output indicates incomplete items, pending work, or unresolved blockers</think_about_whether_you_are_done>
    </failure_detection>
  </pattern>

  <pattern name="failure_handling">
    <description>Standard failure handling phase for workflows</description>
    <example>
<phase name="failure_handling">
  <objective>Handle errors and edge cases gracefully</objective>
  <step order="1">If tool call fails: Log error, attempt alternative approach</step>
  <step order="2">If data unavailable: Document gap, proceed with partial analysis</step>
  <step order="3">If contradictory evidence: Flag uncertainty, request user clarification</step>
</phase>
    </example>
  </pattern>

  <pattern name="agent_ref">
    <description>Standard agent reference syntax for commands</description>
    <example>
<agents>
  <agent ref="explore" readonly="true" />
  <agent ref="design" readonly="true" />
</agents>

Use ref attribute to reference agent defined in agents/ directory.
readonly attribute indicates whether agent can modify files.
    </example>
  </pattern>

  <pattern name="self_evaluate_phase">
    <description>Standard self-evaluation phase for commands and agents that produce reports</description>
    <example>
<phase name="self_evaluate">
  <objective>Brief quality assessment of output</objective>
  <step order="1">
    <action>Calculate confidence using decision_criteria factors</action>
    <tool>Decision criteria evaluation</tool>
    <output>Confidence score</output>
  </step>
  <step order="2">
    <action>Identify top 1-2 critical issues if confidence below 80</action>
    <tool>Gap analysis</tool>
    <output>Issue list</output>
  </step>
  <step order="3">
    <action>Append self_feedback section to output</action>
    <tool>Output formatting</tool>
    <output>Self-feedback section</output>
  </step>
</phase>
    </example>
  </pattern>

  <pattern name="self_feedback_output">
    <description>Standard self-feedback output section for commands that include self-evaluation</description>
    <example>
<self_feedback>
  <confidence>XX/100 (based on decision_criteria calculation)</confidence>
  <issues>
    - [Critical] Issue description (if any, max 2 total)
    - [Warning] Issue description (if any)
  </issues>
</self_feedback>
    </example>
  </pattern>
</patterns>

<best_practices>
  <practice priority="critical">Use output_format for all agents that return structured results</practice>
  <practice priority="critical">Include reflection_checkpoint at key workflow decision points</practice>
  <practice priority="critical">Include prepare_phase for Serena initialization in commands</practice>
  <practice priority="critical">Add serena_validation to checkpoints before code modifications</practice>
  <practice priority="critical">Use reflection_workflow_steps pattern for mandatory think_about_* calls</practice>
  <practice priority="high">Add self_evaluate_phase for agents producing reports or recommendations</practice>
  <practice priority="high">Use failure_handling phase in all workflows</practice>
  <practice priority="medium">Use agent_ref syntax for consistent agent references in commands</practice>
</best_practices>

<rules priority="critical">
  <rule>Output status must use standard criteria (success >= 80, warning 60-79, error less than 60)</rule>
  <rule>Reflection checkpoints must include confidence threshold</rule>
  <rule>Commands must include prepare_phase for Serena initialization</rule>
  <rule>Use serena_validation_pre_edit before code modification operations</rule>
  <rule>Call think_about_collected_information after any search sequence of 3+ operations</rule>
  <rule>Call think_about_task_adherence before any symbol editing or file modification</rule>
  <rule>Call think_about_whether_you_are_done before returning final result to user</rule>
</rules>

<rules priority="standard">
  <rule>Include failure_handling phase in complex workflows</rule>
  <rule>Use self_feedback_output format for self-evaluation results</rule>
  <rule>Use agent_ref with readonly attribute for clarity</rule>
  <rule>Add serena_validation_investigation after investigation phases</rule>
</rules>

<related_skills>
  <skill name="core-patterns">Base templates for error escalation, decision criteria, enforcement</skill>
  <skill name="parallelization-patterns">Parallel execution and timeout configuration</skill>
  <skill name="serena-usage">Serena MCP operations (memory, symbol search, code navigation)</skill>
</related_skills>

<constraints>
  <must>Use standard output_status_criteria thresholds</must>
  <must>Include confidence score in all structured outputs</must>
  <must>Define threshold in reflection_checkpoints</must>
  <must>Include prepare_phase in command workflows</must>
  <must>Use serena_validation before code modifications</must>
  <must>Call think_about_collected_information after 3+ search operations</must>
  <must>Call think_about_whether_you_are_done before final response</must>
  <avoid>Custom status thresholds that differ from standard</avoid>
  <avoid>Omitting failure_handling in complex workflows</avoid>
  <avoid>Omitting Serena initialization in commands</avoid>
  <avoid>Skipping reflection tool calls at mandatory checkpoints</avoid>
</constraints>
