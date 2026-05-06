---
argument-hint: [question]
description: Question and inquiry command
---

<purpose>
Provide accurate, evidence-based answers to project questions through fact-based investigation. Operates in read-only mode; never modifies files.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">investigation-patterns</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<rules priority="critical">
  <rule>NEVER modify, create, or delete files</rule>
  <rule>NEVER implement fixes; provide analysis and suggestions only</rule>
  <rule>ALWAYS base answers on factual investigation from code and documentation</rule>
  <rule>ALWAYS report confidence levels and unclear points honestly</rule>
  <rule>NEVER justify user assumptions; prioritize technical accuracy</rule>
</rules>
<rules priority="standard">
  <rule>Use investigation-patterns skill for systematic analysis</rule>
  <rule>Delegate to appropriate agents in parallel</rule>
  <rule>Provide file:line references for all findings</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<workflow>
  <phase name="prepare">
    <objective>Initialize Serena and check existing patterns</objective>
    <step>
      <action>Activate Serena project with activate_project</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Check list_memories for relevant patterns</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Load applicable memories with read_memory</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
  </phase>
  <phase name="analyze">
    <objective>Understand the question and determine investigation scope</objective>
    <step>
      <action>What is the user's core question?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Which code/documentation sources are relevant?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>What scope of investigation is appropriate?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Classify question type (architecture, implementation, debugging, design)</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
  </phase>
  <phase name="investigate">
    <objective>Gather evidence from codebase using parallel agent delegation</objective>
    <step>
      <action>Delegate to explore agent: find relevant files and codebase structure</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Delegate to design agent: evaluate architecture and component relationships</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Delegate to performance agent: identify performance-related aspects (if applicable)</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Use fact-check skill patterns: verify external references via Context7 and WebSearch</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
  </phase>
  <reflection_checkpoint id="investigation_quality">
    <question>Have I gathered sufficient evidence from investigation?</question>
    <question>Do findings from different agents align?</question>
    <question>Are there conflicting signals that require deeper analysis?</question>
    <threshold>If confidence less than 70, expand investigation scope or seek clarification</threshold>
    <serena_validation>
      <tool>think_about_collected_information</tool>
      <trigger>After investigation phase completes</trigger>
    </serena_validation>
  </reflection_checkpoint>
  <phase name="synthesize">
    <objective>Compile and verify findings with confidence metrics</objective>
    <step>
      <action>Delegate to quality-assurance agent: evaluate code quality findings</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Delegate to code-quality agent: analyze complexity metrics</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Compile agent findings with confidence metrics</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
  </phase>
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
  <phase name="self_evaluate">
    <objective>Brief quality assessment of answer output</objective>
    <step>
      <action>Calculate confidence using decision_criteria: evidence_quality (50%), answer_completeness (30%), source_verification (20%)</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Identify top 1-2 critical issues if confidence below 80 or gaps detected</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step>
      <action>Append self_feedback section to output</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="explore" subagent_type="explore" readonly="true">Finding files, exploring codebase structure</agent>
  <agent name="design" subagent_type="design" readonly="true">System design, architecture, API structure</agent>
  <agent name="performance" subagent_type="performance" readonly="true">Performance bottlenecks, optimization questions</agent>
  <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">Code quality evaluation, best practices</agent>
  <agent name="code-quality" subagent_type="code-quality" readonly="true">Code complexity analysis</agent>
</agents>
<execution_graph>
  <parallel_group id="investigation" depends_on="none">
    <agent>explore</agent>
    <agent>design</agent>
    <agent>performance</agent>
  </parallel_group>
  <parallel_group id="synthesis" depends_on="investigation">
    <agent>quality-assurance</agent>
    <agent>code-quality</agent>
  </parallel_group>
</execution_graph>
<decision_criteria inherits="core-patterns#decision_criteria">
  <criterion name="confidence_calculation">
    <factor name="evidence_quality" weight="0.5">
      <score range="90-100">Direct code evidence found</score>
      <score range="70-89">Strong inference from code</score>
      <score range="50-69">Indirect evidence</score>
      <score range="0-49">Speculation only</score>
    </factor>
    <factor name="answer_completeness" weight="0.3">
      <score range="90-100">All aspects of question addressed</score>
      <score range="70-89">Main question answered</score>
      <score range="50-69">Partial answer</score>
      <score range="0-49">Incomplete answer</score>
    </factor>
    <factor name="source_verification" weight="0.2">
      <score range="90-100">Multiple sources confirm answer</score>
      <score range="70-89">Single reliable source</score>
      <score range="50-69">Unverified source</score>
      <score range="0-49">No source cited</score>
    </factor>
  </criterion>
</decision_criteria>
<output>
  <format>
    <question>Restate the user's question for confirmation</question>
    <investigation>Evidence-based findings with file:line references
- Source 1: `path/to/file.ts:42` - finding
- Source 2: `path/to/other.ts:15` - finding</investigation>
    <conclusion>Direct answer based on evidence</conclusion>
    <metrics>
- Confidence: 0-100 (based on evidence quality)
- Evidence Coverage: 0-100 (how much relevant code was examined)</metrics>
    <recommendations>Optional: Suggested actions without implementation</recommendations>
    <unclear_points>Information gaps that would improve the answer</unclear_points>
    <self_feedback>
      <confidence>XX/100 (based on evidence_quality)</confidence>
      <issues>
- [Critical] Issue description (if any, max 2 total)
- [Warning] Issue description (if any)
      </issues>
    </self_feedback>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="ASK-B001" priority="critical">
      <trigger>When answering questions</trigger>
      <action>Cite specific file:line references</action>
      <verification>References included in answer</verification>
    </behavior>
    <behavior id="ASK-B002" priority="critical">
      <trigger>When uncertain</trigger>
      <action>Explicitly state uncertainty level</action>
      <verification>Confidence level in output</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="ASK-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Answering without code investigation</action>
      <response>Block answer, require investigation first</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor inconsistency in documentation or comments</example>
    <example severity="medium">Unclear code pattern or ambiguous architecture</example>
    <example severity="high">Conflicting evidence about system behavior</example>
    <example severity="critical">Potential security vulnerability or data integrity issue</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="bug">When investigating error-related questions</command>
  <command name="define">When question requires requirements clarification</command>
  <command name="execute">When answer leads to implementation needs</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="investigation-patterns">Core skill for systematic evidence-based analysis</skill>
  <skill name="serena-usage">Symbol-level search for efficient code navigation</skill>
  <skill name="context7-usage">Verify library documentation for accuracy</skill>
  <skill name="fact-check">External source verification using Context7 and WebSearch</skill>
</related_skills>
<constraints>
  <must>Keep all operations read-only</must>
  <must>Provide file:line references for findings</must>
  <must>Report confidence levels honestly</must>
  <must>Distinguish between facts and inferences</must>
  <avoid>Implementing or modifying any code</avoid>
  <avoid>Guessing when evidence is insufficient</avoid>
  <avoid>Confirming user assumptions without verification</avoid>
</constraints>
