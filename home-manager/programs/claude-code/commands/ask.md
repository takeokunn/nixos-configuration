---
argument-hint: [question]
description: Question and inquiry command
---

<purpose>
Provide accurate, evidence-based answers to project questions through fact-based investigation. Operates in read-only mode; never modifies files.
</purpose>

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

<parallelization>
  <capability>
    <parallel_safe>true</parallel_safe>
    <read_only>true</read_only>
    <modifies_state>none</modifies_state>
  </capability>
  <execution_strategy>
    <max_parallel_agents>16</max_parallel_agents>
    <timeout_per_agent>180000</timeout_per_agent>
  </execution_strategy>
</parallelization>

<workflow>
  <phase name="analyze">
    <objective>Understand the question and determine investigation scope</objective>
    <step>1. What is the user's core question?</step>
    <step>2. Which code/documentation sources are relevant?</step>
    <step>3. What scope of investigation is appropriate?</step>
    <step>4. Classify question type (architecture, implementation, debugging, design)</step>
  </phase>
  <phase name="investigate">
    <objective>Gather evidence from codebase using parallel agent delegation</objective>
    <step>1. Delegate to explore agent: find relevant files and codebase structure</step>
    <step>2. Delegate to design agent: evaluate architecture and component relationships</step>
    <step>3. Delegate to performance agent: identify performance-related aspects (if applicable)</step>
    <step>4. Delegate to fact-check agent: verify external references in question context</step>
  </phase>
  <reflection_checkpoint id="investigation_quality">
    <question>Have I gathered sufficient evidence from investigation?</question>
    <question>Do findings from different agents align?</question>
    <question>Are there conflicting signals that require deeper analysis?</question>
    <threshold>If confidence less than 70, expand investigation scope or seek clarification</threshold>
  </reflection_checkpoint>
  <phase name="synthesize">
    <objective>Compile and verify findings with confidence metrics</objective>
    <step>1. Delegate to quality-assurance agent: evaluate code quality findings</step>
    <step>2. Delegate to code-quality agent: analyze complexity metrics</step>
    <step>3. Compile agent findings with confidence metrics</step>
  </phase>
  <reflection_checkpoint id="analysis_quality">
    <question>Have I gathered sufficient evidence to proceed?</question>
    <question>Are there gaps in my understanding?</question>
    <threshold>If confidence less than 70, seek more evidence or ask user</threshold>
  </reflection_checkpoint>
  <phase name="failure_handling">
    <objective>Handle errors and edge cases gracefully</objective>
    <step>1. If tool call fails: Log error, attempt alternative approach</step>
    <step>2. If data unavailable: Document gap, proceed with partial analysis</step>
    <step>3. If contradictory evidence: Flag uncertainty, request user clarification</step>
  </phase>
  <phase name="self_evaluate">
    <objective>Brief quality assessment of answer output</objective>
    <step>1. Calculate confidence using decision_criteria: evidence_quality (50%), answer_completeness (30%), source_verification (20%)</step>
    <step>2. Identify top 1-2 critical issues if confidence below 80 or gaps detected</step>
    <step>3. Append self_feedback section to output</step>
  </phase>
</workflow>

<decision_criteria>
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
  <validation_tests>
    <test name="direct_evidence">
      <input>evidence_quality=95, answer_completeness=90, source_verification=95</input>
      <calculation>(95*0.5)+(90*0.3)+(95*0.2) = 47.5+27+19 = 93.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>Direct code evidence with verified sources yields high confidence</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>evidence_quality=75, answer_completeness=85, source_verification=80</input>
      <calculation>(75*0.5)+(85*0.3)+(80*0.2) = 37.5+25.5+16 = 79</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Strong inference without direct evidence results in 79, triggers warning</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>evidence_quality=80, answer_completeness=80, source_verification=80</input>
      <calculation>(80*0.5)+(80*0.3)+(80*0.2) = 40+24+16 = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Weighted average exactly 80, meets success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>evidence_quality=60, answer_completeness=55, source_verification=60</input>
      <calculation>(60*0.5)+(55*0.3)+(60*0.2) = 30+16.5+12 = 58.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Weighted average 58.5 is below 60, triggers error</reasoning>
    </test>
    <test name="speculation_only">
      <input>evidence_quality=45, answer_completeness=55, source_verification=40</input>
      <calculation>(45*0.5)+(55*0.3)+(40\*0.2) = 22.5+16.5+8 = 47</calculation>
      <expected_status>error</expected_status>
      <reasoning>Speculation without verified sources results in 47, triggers error</reasoning>
    </test>
  </validation_tests>
</decision_criteria>

<agents>
  <agent name="explore" subagent_type="explore" readonly="true">Finding files, exploring codebase structure</agent>
  <agent name="design" subagent_type="design" readonly="true">System design, architecture, API structure</agent>
  <agent name="performance" subagent_type="performance" readonly="true">Performance bottlenecks, optimization questions</agent>
  <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">Code quality evaluation, best practices</agent>
  <agent name="code-quality" subagent_type="code-quality" readonly="true">Code complexity analysis</agent>
  <agent name="fact-check" subagent_type="fact-check" readonly="true">External source verification for claims referencing libraries, documentation, standards</agent>
</agents>

<execution_graph>
  <parallel_group id="investigation" depends_on="none">
    <agent>explore</agent>
    <agent>design</agent>
    <agent>performance</agent>
    <agent>fact-check</agent>
  </parallel_group>
  <parallel_group id="synthesis" depends_on="investigation">
    <agent>quality-assurance</agent>
    <agent>code-quality</agent>
  </parallel_group>
</execution_graph>

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

<error_escalation>
  <level severity="low">
    <example>Minor inconsistency in documentation or comments</example>
    <action>Note in report, proceed</action>
  </level>
  <level severity="medium">
    <example>Unclear code pattern or ambiguous architecture</example>
    <action>Document issue, use AskUserQuestion for clarification</action>
  </level>
  <level severity="high">
    <example>Conflicting evidence about system behavior</example>
    <action>STOP, present options to user</action>
  </level>
  <level severity="critical">
    <example>Potential security vulnerability or data integrity issue</example>
    <action>BLOCK operation, require explicit user acknowledgment</action>
  </level>
</error_escalation>

<related_commands>
  <command name="bug">When investigating error-related questions</command>
  <command name="define">When question requires requirements clarification</command>
  <command name="execute">When answer leads to implementation needs</command>
</related_commands>

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
