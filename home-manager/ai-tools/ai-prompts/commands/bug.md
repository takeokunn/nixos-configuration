---
argument-hint: [error-message]
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
    <step order="5">
      <action>Analyze error location details from agent findings</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="6">
      <action>Review dependencies and imports</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="7">
      <action>Check config files and recent changes</action>
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
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
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
  <phase name="self_evaluate">
    <step order="1">
      <action>Calculate confidence using decision_criteria: root_cause_certainty (50%), evidence_chain (30%), fix_viability (20%)</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Identify top 1-2 critical issues if confidence below 80 or evidence gaps detected</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="3">
      <action>Append self_feedback section to output</action>
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
        Note: write_memory is Serena memory only — this does not violate the read-only file constraint.
        For write_memory: prepend memory_content_format frontmatter (serena-usage skill)
        with domain, status=active, created=YYYY-MM, last-verified=YYYY-MM.
        For edit_memory on a memory lacking frontmatter: add it, updating last-verified.
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
<execution_graph>
  <parallel_group id="error_analysis" depends_on="none">
    <agent>quality-assurance</agent>
    <agent>explore</agent>
  </parallel_group>
  <parallel_group id="context_gathering" depends_on="none">
    <agent>general-purpose</agent>
  </parallel_group>
  <sequential_step id="synthesis" depends_on="error_analysis,context_gathering">
    <agent>quality-assurance</agent>
    <reason>Requires findings from both error analysis and context gathering</reason>
  </sequential_step>
</execution_graph>
<delegation>
  <requirement>Full error message/stack trace</requirement>
  <requirement>Reproduction steps (if known)</requirement>
  <requirement>Related file paths</requirement>
  <requirement>Explicit edit prohibition</requirement>
</delegation>
<decision_criteria inherits="core-patterns#decision_criteria">
  <criterion name="confidence_calculation">
    <factor name="root_cause_certainty" weight="0.5">
      <score range="90-100">Root cause confirmed with reproduction</score>
      <score range="70-89">Likely root cause identified</score>
      <score range="50-69">Possible causes identified</score>
      <score range="0-49">Root cause unclear</score>
    </factor>
    <factor name="evidence_chain" weight="0.3">
      <score range="90-100">Complete evidence chain from symptom to cause</score>
      <score range="70-89">Strong evidence trail</score>
      <score range="50-69">Partial evidence</score>
      <score range="0-49">Weak evidence</score>
    </factor>
    <factor name="fix_viability" weight="0.2">
      <score range="90-100">Clear, tested fix available</score>
      <score range="70-89">Fix approach defined</score>
      <score range="50-69">Possible fix identified</score>
      <score range="0-49">No clear fix</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="success_case">
      <input>root_cause_certainty=95, evidence_chain=90, fix_viability=90</input>
      <calculation>(95*0.5)+(90*0.3)+(90*0.2) = 92.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>root_cause_certainty=80, evidence_chain=80, fix_viability=80</input>
      <calculation>(80*0.5)+(80*0.3)+(80*0.2) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>root_cause_certainty=79, evidence_chain=79, fix_viability=79</input>
      <calculation>(79*0.5)+(79*0.3)+(79*0.2) = 79</calculation>
      <expected_status>warning</expected_status>
      <reasoning>79 is below success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>root_cause_certainty=59, evidence_chain=59, fix_viability=59</input>
      <calculation>(59*0.5)+(59*0.3)+(59*0.2) = 59</calculation>
      <expected_status>error</expected_status>
      <reasoning>59 is at error threshold</reasoning>
    </test>
    <test name="error_case">
      <input>root_cause_certainty=40, evidence_chain=50, fix_viability=30</input>
      <calculation>(40*0.5)+(50*0.3)+(30*0.2) = 41</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores yield error status</reasoning>
    </test>
  </validation_tests>
</decision_criteria>
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
    <self_feedback>
      <confidence>XX/100</confidence>
      <dimension name="root_cause_certainty">XX/100: one-line rationale</dimension>
      <dimension name="evidence_chain">XX/100: one-line rationale</dimension>
      <dimension name="fix_viability">XX/100: one-line rationale</dimension>
      <gaps>What additional evidence or context would raise confidence above current score</gaps>
      <issues>
- [Critical] Issue description (if any, max 2 total)
- [Warning] Issue description (if any)
      </issues>
    </self_feedback>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="BUG-B001" priority="critical">
      <trigger>Before concluding root cause</trigger>
      <action>Build evidence chain from symptom to cause</action>
      <verification>Evidence chain in output</verification>
    </behavior>
    <behavior id="BUG-B002" priority="critical">
      <trigger>When proposing fix</trigger>
      <action>Identify all affected code paths</action>
      <verification>Impact analysis in output</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="BUG-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Concluding without evidence</action>
      <response>Block conclusion, require investigation</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor log warning without impact</example>
    <example severity="medium">Unclear error context or missing stack trace</example>
    <example severity="high">System crash or data corruption detected</example>
    <example severity="critical">Security breach or critical data loss risk</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="ask">When investigation reveals architectural questions</command>
  <command name="define">When bug fix requires requirements specification</command>
  <command name="execute">When ready to implement fix after investigation</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="investigation-patterns">Core debugging methodology</skill>
  <skill name="serena-usage">Navigate error locations efficiently</skill>
  <skill name="testing-patterns">Understand test failures and coverage gaps</skill>
  <skill name="fact-check">External source verification using Context7 and WebSearch</skill>
</related_skills>
<constraints>
  <must>Keep all operations read-only</must>
  <must>Prioritize logs as primary information source</must>
  <must>Report honestly if cause cannot be identified</must>
  <avoid>Implementing fixes</avoid>
  <avoid>Accepting user speculation without verification</avoid>
  <avoid>Forcing contrived causes when evidence is insufficient</avoid>
</constraints>
