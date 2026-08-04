---
name: Workflow Patterns
description: Patterns for output formats, reflection checkpoints, agent references, and self-evaluation shared across agents and commands.
version: 2.1.0
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
  <step order="1">
  <action>Activate Serena project with activate_project</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  <step order="2">
  <action>Check list_memories for relevant patterns</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  <step order="3">
  <action>Load applicable memories with read_memory</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
</phase>
    </example>
  </pattern>

  <pattern name="failure_handling">
    <description>Failure-handling phase template for exceptional paths aligned with shared workflow usage</description>
    <example>
<phase name="failure_handling">
  <objective>Handle errors and edge cases gracefully</objective>
  <step order="1">If tool call fails: log error and attempt alternative approach</step>
  <step order="2">If data unavailable: document gap and continue with bounded analysis</step>
  <step order="3">If contradictory evidence: flag uncertainty and request clarification</step>
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

  <pattern name="convention_adoption_gate">
    <description>A convention is not adopted until a machine gate enforces it. A rule that lives only in a document is advisory, and it erodes at the rate new code is written — so the definition of done for adding a rule includes its enforcement mechanism, not just the prose.</description>
    <rule>When adding a coding or process convention, the task is complete only once a matching automated check exists and runs in the project's normal verification set.</rule>
    <rule>If a rule cannot be mechanically checked, reconsider stating it. An unenforceable rule costs review attention on every change and buys compliance only while someone remembers it.</rule>
    <gate_categories>
      <category name="formatting_and_lint">Style and idiom rules, enforced by the project's formatter and linter configuration rather than by review comments</category>
      <category name="boundary_checks">Import and layering constraints, enforced by a dependency or import-boundary checker</category>
      <category name="unused_surface">Dead export and unreachable code detection, so a removal convention stays true over time</category>
      <category name="project_policy">Rules no off-the-shelf tool knows about, written as a test in the normal suite (see quality-tools for how to author one without it becoming noisy)</category>
    </gate_categories>
    <example>
      <note>Adding "all new modules must declare explicit exports"</note>
      <note>Not done: the rule is written in the conventions document</note>
      <note>Done: the rule is written down AND a lint rule or boundary check fails on a module that violates it</note>
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
  <practice priority="high">Add self_evaluate_phase for agents producing reports or recommendations</practice>
  <practice priority="high">Use failure_handling phase in all workflows</practice>
  <practice priority="medium">Use agent_ref syntax for consistent agent references in commands</practice>
  <practice priority="high">Treat a new convention as unadopted until a machine gate enforces it, and reconsider any rule that cannot be mechanically checked (convention_adoption_gate)</practice>
</best_practices>

<rules priority="critical">
  <rule>Output status must use standard criteria (success >= 80, warning 60-79, error less than 60)</rule>
  <rule>Reflection checkpoints must include confidence threshold</rule>
  <rule>Commands must include prepare_phase for Serena initialization</rule>
</rules>

<rules priority="standard">
  <rule>Include failure_handling phase in complex workflows</rule>
  <rule>Use self_feedback_output format for self-evaluation results</rule>
  <rule>Use agent_ref with readonly attribute for clarity</rule>
</rules>

<constraints>
  <must>Use standard output_status_criteria thresholds</must>
  <must>Include confidence score in all structured outputs</must>
  <must>Define threshold in reflection_checkpoints</must>
  <must>Include prepare_phase in command workflows</must>
  <avoid>Custom status thresholds that differ from standard</avoid>
  <avoid>Omitting failure_handling in complex workflows</avoid>
  <avoid>Omitting Serena initialization in commands</avoid>
</constraints>

<related_skills>
  <skill name="core-patterns">Base templates for error escalation, decision criteria, enforcement</skill>
  <skill name="parallelization-patterns">Parallel execution and timeout configuration</skill>
  <skill name="serena-usage">Serena MCP operations (memory, symbol search, code navigation)</skill>
  <skill name="quality-tools">Tool catalog and project-local policy gates that make a convention enforceable</skill>
</related_skills>

<related_agents>
  <agent name="validator">Verify workflow consistency and checkpoint completeness</agent>
</related_agents>
