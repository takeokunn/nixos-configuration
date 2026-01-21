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
  <practice priority="high">Add self_evaluate_phase for agents producing reports or recommendations</practice>
  <practice priority="high">Use failure_handling phase in all workflows</practice>
  <practice priority="medium">Use agent_ref syntax for consistent agent references in commands</practice>
</best_practices>

<rules priority="critical">
  <rule>Output status must use standard criteria (success >= 80, warning 60-79, error less than 60)</rule>
  <rule>Reflection checkpoints must include confidence threshold</rule>
</rules>

<rules priority="standard">
  <rule>Include failure_handling phase in complex workflows</rule>
  <rule>Use self_feedback_output format for self-evaluation results</rule>
  <rule>Use agent_ref with readonly attribute for clarity</rule>
</rules>

<related_skills>
  <skill name="core-patterns">Base templates for error escalation, decision criteria, enforcement</skill>
  <skill name="parallelization-patterns">Parallel execution and timeout configuration</skill>
</related_skills>

<constraints>
  <must>Use standard output_status_criteria thresholds</must>
  <must>Include confidence score in all structured outputs</must>
  <must>Define threshold in reflection_checkpoints</must>
  <avoid>Custom status thresholds that differ from standard</avoid>
  <avoid>Omitting failure_handling in complex workflows</avoid>
</constraints>
