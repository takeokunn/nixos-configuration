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

<workflow>
  <phase name="prepare">
    <objective>Initialize Serena and check existing patterns</objective>
    <step>1. Activate Serena project with activate_project</step>
    <step>2. Check list_memories for relevant patterns</step>
    <step>3. Load applicable memories with read_memory</step>
  </phase>
  <phase name="analyze">
    <objective>Classify error type and establish investigation scope</objective>
    <step>1. What type of error is this? (syntax, runtime, logic)</step>
    <step>2. Where does it occur? (file, line, function)</step>
    <step>3. What logs are available?</step>
    <step>4. What is the error context? (before, during, after)</step>
  </phase>
  <phase name="investigate">
    <objective>Delegate parallel investigations to specialized agents</objective>
    <step>1. Delegate to quality-assurance agent: analyze stack trace, error patterns</step>
    <step>2. Delegate to explore agent: find error location and related code paths</step>
    <step>3. Delegate to general-purpose agent: analyze logs and dependencies</step>
    <step>4. Use fact-check skill patterns: verify external documentation references via Context7</step>
    <step>5. Analyze error location details from agent findings</step>
    <step>6. Review dependencies and imports</step>
    <step>7. Check config files and recent changes</step>
  </phase>
  <reflection_checkpoint id="investigation_quality">
    <question>Have I built a complete evidence chain from symptom to cause?</question>
    <question>Can I explain the error mechanism with concrete evidence?</question>
    <threshold>If confidence less than 70, continue investigation or flag uncertainty</threshold>
    <serena_validation>
      <tool>think_about_collected_information</tool>
      <trigger>After investigation phase completes</trigger>
    </serena_validation>
  </reflection_checkpoint>
  <phase name="gather">
    <objective>Collect environmental context and runtime conditions</objective>
    <step>1. Collect runtime info (OS, versions, env vars)</step>
    <step>2. Check resources (disk, memory, network)</step>
  </phase>
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
  <phase name="report">
    <objective>Synthesize findings into actionable root cause analysis</objective>
    <step>1. Compile agent findings with confidence metrics</step>
    <step>2. Identify root cause with supporting evidence</step>
  </phase>
  <phase name="self_evaluate">
    <objective>Brief quality assessment of investigation output</objective>
    <step>1. Calculate confidence using decision_criteria: root_cause_certainty (50%), evidence_chain (30%), fix_viability (20%)</step>
    <step>2. Identify top 1-2 critical issues if confidence below 80 or evidence gaps detected</step>
    <step>3. Append self_feedback section to output</step>
  </phase>
</workflow>

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
</decision_criteria>

<agents>
  <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">Error tracking, stack trace analysis, debugging</agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Log analysis, observability, dependency errors</agent>
  <agent name="explore" subagent_type="explore" readonly="true">Finding error locations, related code paths</agent>
</agents>

<execution_graph>
  <parallel_group id="error_analysis" depends_on="none">
    <agent>quality-assurance</agent>
    <agent>explore</agent>
  </parallel_group>
  <parallel_group id="context_gathering" depends_on="none">
    <agent>general-purpose</agent>
  </parallel_group>
  <sequential_phase id="synthesis" depends_on="error_analysis,context_gathering">
    <agent>quality-assurance</agent>
    <reason>Requires findings from both error analysis and context gathering</reason>
  </sequential_phase>
</execution_graph>

<delegation>
  <requirement>Full error message/stack trace</requirement>
  <requirement>Reproduction steps (if known)</requirement>
  <requirement>Related file paths</requirement>
  <requirement>Explicit edit prohibition</requirement>
</delegation>

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
      <confidence>XX/100 (based on root_cause_certainty)</confidence>
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
