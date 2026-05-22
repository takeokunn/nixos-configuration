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
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Investigating files one at a time before synthesizing — AI can survey all relevant files in a single parallel investigation pass</practice>
    <practice>Accepting the question framing as the correct framing — AI should verify whether the stated question matches the underlying need before answering</practice>
    <practice>Reporting uncertainty without evidence — every finding must be anchored to a specific file:line reference, not general impressions</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Map the full evidence surface (all relevant files, cross-references, documentation) before forming any conclusion</principle>
    <principle>Distinguish facts (from code evidence) from inferences (deduced) from speculation (no evidence) — label each finding explicitly</principle>
    <principle>Always verify claimed patterns exist in the current codebase; memory and training data about past states can be stale</principle>
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
      <action>Check list_memories for relevant patterns</action>
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
      <action>What is the user's core question?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Which code/documentation sources are relevant?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="3">
      <action>What scope of investigation is appropriate?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="4">
      <action>Classify question type (architecture, implementation, debugging, design)</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <phase name="investigate">
    <step order="1">
      <action>Delegate to explore agent: find relevant files and codebase structure</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Delegate to design agent: evaluate architecture and component relationships</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="3">
      <action>Delegate to performance agent: identify performance-related aspects (if applicable)</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="4">
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
  </reflection_checkpoint>
  <phase name="synthesize">
    <step order="1">
      <action>Delegate to quality-assurance agent: evaluate code quality findings</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Delegate to code-quality agent: analyze complexity metrics</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="3">
      <action>Compile agent findings with confidence metrics</action>
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
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="explore" subagent_type="explore" readonly="true">
    <role>Discover and map codebase structure relevant to the question</role>
    <receives>question_topic, suspected_file_paths[], search_keywords[]</receives>
    <produces>file_paths[], code_excerpts[]{path: file:line, content}, structure_summary</produces>
    <done_when>All relevant files and code paths identified; confidence >= 70 or explicit uncertainty reported</done_when>
  </agent>
  <agent name="design" subagent_type="design" readonly="true">
    <role>Evaluate system design, architectural decisions, and component relationships</role>
    <receives>component_names[], question_context, file_paths[]</receives>
    <produces>architecture_analysis, dependency_map, design_assessment{pattern, rationale, alternatives[]}</produces>
    <done_when>Architectural relationships mapped; all relevant components evaluated</done_when>
  </agent>
  <agent name="performance" subagent_type="performance" readonly="true">
    <role>Identify performance characteristics, bottlenecks, and optimization opportunities</role>
    <receives>code_paths[], performance_concern, context</receives>
    <produces>bottleneck_locations[]{file:line, description}, complexity_analysis, optimization_candidates[]</produces>
    <done_when>Performance-sensitive code paths analyzed; findings reported with file:line evidence</done_when>
  </agent>
  <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">
    <role>Evaluate code quality, best practices compliance, and correctness</role>
    <receives>file_paths[], code_excerpts[], quality_dimensions[]</receives>
    <produces>quality_assessment{score: 0-100, issues[]{severity, location: file:line, description}}, gaps[]</produces>
    <done_when>All provided files assessed; quality score and issue list produced with evidence</done_when>
  </agent>
  <agent name="code-quality" subagent_type="code-quality" readonly="true">
    <role>Analyze code complexity metrics and structural maintainability</role>
    <receives>file_paths[], complexity_threshold</receives>
    <produces>complexity_metrics{cyclomatic, cognitive}, refactoring_candidates[], maintainability_score: 0-100</produces>
    <done_when>Complexity metrics computed for all provided files; candidates ranked by impact</done_when>
  </agent>
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
  <validation_tests>
    <test name="success_case">
      <input>evidence_quality=95, answer_completeness=90, source_verification=90</input>
      <calculation>(95*0.5)+(90*0.3)+(90*0.2) = 92.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>evidence_quality=80, answer_completeness=80, source_verification=80</input>
      <calculation>(80*0.5)+(80*0.3)+(80*0.2) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>evidence_quality=79, answer_completeness=79, source_verification=79</input>
      <calculation>(79*0.5)+(79*0.3)+(79*0.2) = 79</calculation>
      <expected_status>warning</expected_status>
      <reasoning>79 is below success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>evidence_quality=59, answer_completeness=59, source_verification=59</input>
      <calculation>(59*0.5)+(59*0.3)+(59*0.2) = 59</calculation>
      <expected_status>error</expected_status>
      <reasoning>59 is at error threshold</reasoning>
    </test>
    <test name="error_case">
      <input>evidence_quality=40, answer_completeness=50, source_verification=30</input>
      <calculation>(40*0.5)+(50*0.3)+(30*0.2) = 41</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores yield error status</reasoning>
    </test>
  </validation_tests>
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
      <confidence>XX/100</confidence>
      <dimension name="evidence_quality">XX/100: one-line rationale</dimension>
      <dimension name="answer_completeness">XX/100: one-line rationale</dimension>
      <dimension name="source_verification">XX/100: one-line rationale</dimension>
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
