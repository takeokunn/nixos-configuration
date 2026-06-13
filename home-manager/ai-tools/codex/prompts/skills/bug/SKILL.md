---
name: bug
description: Root cause investigation command
---

<purpose>
Identify root causes from error messages and anomalous behavior, providing fact-based analysis without performing fixes.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">investigation-patterns</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<rules priority="critical">
  <rule>Never modify, create, or delete files</rule>
  <rule>Never implement fixes; provide suggestions only</rule>
  <rule>Prioritize log analysis as primary information source</rule>
  <rule>Judge from facts, not user speculation</rule>
  <rule>Logs as primary information source</rule>
</rules>
<rules priority="standard">
  <rule>Use investigation-patterns skill for debugging methodology</rule>
  <rule>Delegate investigations to debug agent</rule>
  <rule>Report honestly if cause cannot be identified</rule>
  <rule>Verify similar implementations nearby</rule>
  <rule>Track occurrence path chronologically</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Reproducing bugs manually in isolation — AI can trace all call paths and state transitions from a stack trace in a single pass</practice>
    <practice>Treating the error location as the root cause — AI can distinguish symptom from root cause by mapping the full dependency chain</practice>
    <practice>One hypothesis at a time — AI can score multiple hypotheses in parallel and rule out lower-probability causes simultaneously</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Build a complete evidence chain (symptom → mechanism → root cause) before concluding — never jump from signal to verdict</principle>
    <principle>Verify hypotheses against code evidence, not user description alone — the reported location is often not the true source</principle>
    <principle>Map all recurrence locations in a single investigation pass — don't fix only the reported instance</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Activate Serena project with activate_project</action>
      <tool>Serena activate_project</tool>
      <output>Project activated</output>
    </step>
    <step order="2">
      <action>Check list_memories for relevant patterns</action>
      <tool>Serena list_memories</tool>
      <output>Full memory index</output>
    </step>
    <step order="3">
      <action>Classify task type as "investigation". Apply memory_reading_by_task_type filter
        (serena-usage skill): prioritize {domain}-patterns → architecture-* → {project}-conventions.
        Filter the memory index from step 2 against these categories; record matched names.</action>
      <tool>serena-usage#memory_reading_by_task_type (reference only)</tool>
      <output>Filtered priority memory list for investigation tasks</output>
    </step>
    <step order="4">
      <action>Load only memories matching the prioritized categories with read_memory;
        skip categories absent from the index</action>
      <tool>Serena read_memory</tool>
      <output>Prioritized patterns loaded</output>
    </step>

  </phase>
  <phase name="analyze">
    <step order="1">
      <observe>Error message text, exception type, and any provided stack trace or logs</observe>
      <reason>Error classification (syntax / runtime / logic / config) determines the investigation branch and which agents are most relevant</reason>
      <act>Classify error type; record classification as input to investigate phase delegation</act>
    </step>
    <step order="2">
      <observe>Stack trace line numbers, file names, and call chain depth</observe>
      <reason>The deepest non-library frame is the symptom location — not necessarily the root cause; the full chain reveals how control flow reached the failure point</reason>
      <act>Record primary error location (file:line) and call chain; flag the distinction between symptom site and likely root cause site</act>
    </step>
    <step order="3">
      <observe>Available log output: timestamps, log levels, preceding events</observe>
      <reason>Logs provide temporal context to distinguish a new failure from a recurring pattern, and reveal system state at failure time</reason>
      <act>Identify log lines directly preceding and surrounding the error; note any state anomalies</act>
    </step>
    <step order="4">
      <observe>Events immediately before, during, and after the error occurrence</observe>
      <reason>Temporal context distinguishes transient conditions (race, resource exhaustion) from deterministic bugs (logic error, missing null check)</reason>
      <act>Record the error trigger sequence; classify as deterministic or condition-dependent</act>
    </step>

  </phase>
  <phase name="investigate">
    <step order="1">
      <action>Delegate to quality-assurance agent: analyze stack trace, error patterns</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Delegate to explore agent: find error location and related code paths</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="3">
      <action>Delegate to general-purpose agent: analyze logs and dependencies</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="4">
      <action>Use fact-check skill patterns: verify external documentation references via Context7</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <reflection_checkpoint id="investigation_quality">
    <question>Have I built a complete evidence chain from symptom to cause?</question>
    <question>Can I explain the error mechanism with concrete evidence?</question>
    <threshold>If confidence less than 70, continue investigation or flag uncertainty</threshold>
  </reflection_checkpoint>
  <phase name="gather">
    <step order="1">
      <action>Collect runtime info (OS, versions, env vars)</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Check resources (disk, memory, network)</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <phase name="persist">
    <objective>Capture reusable debugging insights to Serena memory</objective>
    <step order="1">
      <action>Evaluate memory_auto_creation_triggers: did this investigation reveal a reusable debugging pattern,
        an architectural insight, or a recurring bug class?
        Call list_memories to check if a memory for this topic already exists.</action>
      <tool>Serena list_memories, evaluation against trigger list</tool>
      <output>Trigger match: yes/no; existing memory: yes/no</output>
    </step>
    <step order="2">
      <action>If trigger matched: use edit_memory (existing topic) or write_memory (new topic).
        If no trigger matched: output "persist: no triggers matched — skip"</action>
      <tool>Serena edit_memory or write_memory</tool>
      <output>Memory entry updated with frontmatter (name listed), or explicit skip reason</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">
    <role>Analyze error patterns, stack traces, and code defects to identify failure mechanisms</role>
    <receives>error_message, stack_trace, file_paths[], reproduction_steps</receives>
    <produces>error_classification{type, mechanism}, defects[]{location: file:line, description}, hypotheses[]{cause, confidence: 0-100}</produces>
    <done_when>Evidence chain from symptom to root cause established; confidence >= 70 or competing hypotheses explicitly ranked</done_when>
  </agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">
    <role>Analyze logs, runtime environment, and dependency relationships for contextual evidence</role>
    <receives>log_content, environment_info{os, versions, env_vars}, dependency_list[]</receives>
    <produces>log_analysis{critical_events[], timeline}, env_anomalies[], dependency_issues[]</produces>
    <done_when>All available log entries processed; environmental factors assessed with file:line references where applicable</done_when>
  </agent>
  <agent name="explore" subagent_type="explore" readonly="true">
    <role>Locate error sites, trace call paths, and find all recurrence locations for the same root cause</role>
    <receives>error_location{file, line}, symbol_names[], search_patterns[]</receives>
    <produces>error_site{surrounding_code, call_chain}, related_paths[], recurrences[]{file:line, similarity_reason}</produces>
    <done_when>Error site mapped; all locations sharing the same root cause pattern identified</done_when>
  </agent>
</agents>
<output>
  <format>
    <overview>Summary of error and investigation</overview>
    <log_analysis>Critical log information, error context</log_analysis>
    <code_analysis>Relevant code, identified issues</code_analysis>
    <root_cause>
- Direct cause
- Underlying cause
- Conditions</root_cause>
    <metrics>
- Confidence: 0-100
- Log Utilization: 0-100
- Objectivity: 0-100</metrics>
    <impact>Scope, similar errors</impact>
    <recommendations>Fix suggestions (no implementation), prevention</recommendations>
    <further_investigation>Unclear points, next steps</further_investigation>
  </format>
</output>
<constraints>
  <must>Keep all operations read-only</must>
  <must>Prioritize logs as primary information source</must>
  <must>Report honestly if cause cannot be identified</must>
  <avoid>Implementing fixes</avoid>
  <avoid>Accepting user speculation without verification</avoid>
  <avoid>Forcing contrived causes when evidence is insufficient</avoid>
</constraints>
