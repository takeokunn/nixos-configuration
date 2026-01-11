---
argument-hint: [message]
description: Requirements definition command
---

<purpose>
Conduct detailed requirements definition before implementation, clarifying technical constraints, design policies, and specifications.
</purpose>

<rules priority="critical">
  <rule>Never modify, create, or delete files</rule>
  <rule>Never implement code; requirements definition only</rule>
  <rule>Clearly identify technically impossible requests</rule>
  <rule>Prioritize technical validity over user preferences</rule>
  <rule>Technical evidence over speculation</rule>
</rules>

<rules priority="standard">
  <rule>Use requirements-definition skill for methodology</rule>
  <rule>Delegate investigations to sub-agents</rule>
  <rule>Ask questions without limit until requirements are clear</rule>
  <rule>Investigate and question before concluding</rule>
  <rule>Always include a (Recommended) option when presenting choices via AskUserQuestion</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Understand the user's request and identify technical constraints</objective>
    <step number="1">
      <action>Parse user request to extract core requirements</action>
      <tool>Text analysis</tool>
      <output>Initial requirements list</output>
    </step>
    <step number="2">
      <action>Identify technical constraints from request context</action>
      <tool>Codebase knowledge</tool>
      <output>Constraint list</output>
    </step>
    <step number="3">
      <action>Determine design decisions requiring user input</action>
      <tool>Requirements analysis</tool>
      <output>Question candidates list</output>
    </step>
    <step number="4">
      <action>Assess technical feasibility at high level</action>
      <tool>Technical knowledge</tool>
      <output>Initial feasibility assessment</output>
    </step>
  </phase>
  <phase name="investigate">
    <objective>Gather evidence from codebase and analyze architecture impact</objective>
    <step number="1">
      <action>Delegate to explore agent: find relevant files and existing patterns</action>
      <tool>Sub-agent delegation</tool>
      <output>File paths, patterns, code samples</output>
    </step>
    <step number="2">
      <action>Delegate to design agent: evaluate architecture consistency and dependencies</action>
      <tool>Sub-agent delegation</tool>
      <output>Architecture analysis, dependency graph</output>
    </step>
    <step number="3">
      <action>Delegate to database agent: analyze database design (if applicable)</action>
      <tool>Sub-agent delegation</tool>
      <output>Schema analysis, query patterns</output>
    </step>
    <step number="4">
      <action>Delegate to general-purpose agent: analyze requirements and estimate effort</action>
      <tool>Sub-agent delegation</tool>
      <output>Effort estimation, risk analysis</output>
    </step>
    <step number="5">
      <action>Delegate to fact-check agent: verify external documentation and standard references</action>
      <tool>Sub-agent delegation</tool>
      <output>Verification report, flagged claims</output>
    </step>
  </phase>
  <reflection_checkpoint id="investigation_complete" after="investigate">
    <questions>
      <question weight="0.4">Have all relevant files and patterns been identified?</question>
      <question weight="0.3">Is the scope clearly understood?</question>
      <question weight="0.3">Are there any technical blockers identified?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Expand investigation scope or ask user</below_threshold>
    </threshold>
  </reflection_checkpoint>
  <reflection_checkpoint id="analysis_quality">
    <question>Have I gathered sufficient evidence to proceed?</question>
    <question>Are there gaps in my understanding?</question>
    <threshold>If confidence less than 70, seek more evidence or ask user</threshold>
  </reflection_checkpoint>
  <phase name="clarify">
    <objective>Resolve ambiguities through structured user interaction</objective>
    <step number="1">
      <action>Score questions by: design branching, irreversibility, investigation impossibility, effort impact (1-5 each)</action>
      <tool>Question scoring algorithm</tool>
      <output>Prioritized question list</output>
    </step>
    <step number="2">
      <action>Classify questions: spec confirmation, design choice, constraint, scope, priority</action>
      <tool>Question taxonomy</tool>
      <output>Categorized questions</output>
    </step>
    <step number="3">
      <action>Use AskUserQuestion tool for all user interactions (2-4 structured options per question)</action>
      <tool>AskUserQuestion</tool>
      <output>User responses</output>
    </step>
    <step number="4">
      <action>For follow-up clarifications, continue using AskUserQuestion tool rather than plain text</action>
      <tool>AskUserQuestion</tool>
      <output>Additional user responses</output>
    </step>
    <step number="5">
      <action>Present high-score questions first; do not proceed without clear answers</action>
      <tool>Priority ordering</tool>
      <output>Confirmed requirements</output>
    </step>
  </phase>
  <phase name="verify">
    <objective>Validate user decisions against technical evidence</objective>
    <step number="1">
      <action>Verify constraints from answers using agent findings</action>
      <tool>Cross-reference analysis</tool>
      <output>Validated constraints</output>
    </step>
    <step number="2">
      <action>Check implementations related to chosen approach</action>
      <tool>Code analysis</tool>
      <output>Implementation validation</output>
    </step>
  </phase>
  <phase name="failure_handling">
    <objective>Handle errors and edge cases gracefully</objective>
    <step number="1">
      <action>If tool call fails: Log error, attempt alternative approach</action>
      <tool>Error handling</tool>
      <output>Fallback results or error report</output>
    </step>
    <step number="2">
      <action>If data unavailable: Document gap, proceed with partial analysis</action>
      <tool>Gap documentation</tool>
      <output>Partial analysis with gaps noted</output>
    </step>
    <step number="3">
      <action>If contradictory evidence: Flag uncertainty, request user clarification</action>
      <tool>Conflict resolution</tool>
      <output>Clarification questions</output>
    </step>
  </phase>
  <phase name="document">
    <objective>Create comprehensive requirements documentation and task breakdown</objective>
    <step number="1">
      <action>Create comprehensive requirements document</action>
      <tool>Requirements template</tool>
      <output>Complete requirements specification</output>
    </step>
    <step number="2">
      <action>Break down tasks for /execute handoff</action>
      <tool>Task decomposition</tool>
      <output>Phased task list with dependencies</output>
    </step>
  </phase>
  <phase name="self_evaluate">
    <objective>Brief quality assessment of requirements output</objective>
    <step number="1">
      <action>Calculate confidence using decision_criteria: requirement_clarity (40%), technical_feasibility (30%), stakeholder_alignment (30%)</action>
      <tool>Decision criteria evaluation</tool>
      <output>Confidence score</output>
    </step>
    <step number="2">
      <action>Identify top 1-2 critical issues if confidence below 80 or requirement gaps detected</action>
      <tool>Gap analysis</tool>
      <output>Issue list</output>
    </step>
    <step number="3">
      <action>Append self_feedback section to output</action>
      <tool>Output formatting</tool>
      <output>Self-feedback section</output>
    </step>
  </phase>
</workflow>

<agents>
  <agent name="design" subagent_type="design" readonly="true">Architecture consistency, dependency analysis, API design</agent>
  <agent name="database" subagent_type="database" readonly="true">Database design and optimization</agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Requirements analysis, estimation, dependency analysis</agent>
  <agent name="explore" subagent_type="explore" readonly="true">Finding relevant files and existing patterns</agent>
  <agent name="validator" subagent_type="validator" readonly="true">Cross-validation and consensus verification</agent>
  <agent name="fact-check" subagent_type="fact-check" readonly="true">External source verification for claims referencing libraries, documentation, standards</agent>
</agents>

<execution_graph>
  <parallel_group id="investigation" depends_on="none">
    <agent>explore</agent>
    <agent>design</agent>
    <agent>database</agent>
    <agent>fact-check</agent>
  </parallel_group>
  <parallel_group id="analysis" depends_on="investigation">
    <agent>general-purpose</agent>
  </parallel_group>
</execution_graph>

<delegation>
  <requirement>Scope overview</requirement>
  <requirement>Target file paths</requirement>
  <requirement>Explicit edit prohibition</requirement>
  <requirement>Sub-agents must use AskUserQuestion tool for any user interactions</requirement>
</delegation>

<parallelization>
  <capability>
    <parallel_safe>true</parallel_safe>
    <read_only>true</read_only>
    <modifies_state>none</modifies_state>
  </capability>
  <execution_strategy>
    <max_parallel_agents>16</max_parallel_agents>
    <timeout_per_agent>300000</timeout_per_agent>
  </execution_strategy>
</parallelization>

<decision_criteria>
  <criterion name="confidence_calculation">
    <factor name="requirement_clarity" weight="0.4">
      <score range="90-100">All requirements clear and documented</score>
      <score range="70-89">Core requirements clear</score>
      <score range="50-69">Some ambiguity remains</score>
      <score range="0-49">Many unclear requirements</score>
    </factor>
    <factor name="technical_feasibility" weight="0.3">
      <score range="90-100">Feasibility confirmed with evidence</score>
      <score range="70-89">Likely feasible</score>
      <score range="50-69">Uncertain feasibility</score>
      <score range="0-49">Likely infeasible</score>
    </factor>
    <factor name="stakeholder_alignment" weight="0.3">
      <score range="90-100">All questions answered by user</score>
      <score range="70-89">Most questions answered</score>
      <score range="50-69">Some questions pending</score>
      <score range="0-49">Many questions unanswered</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="clear_requirements">
      <input>requirement_clarity=95, technical_feasibility=90, stakeholder_alignment=95</input>
      <calculation>(95*0.4)+(90*0.3)+(95*0.3) = 38+27+28.5 = 93.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>All requirements documented with confirmed feasibility yields high confidence</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>requirement_clarity=80, technical_feasibility=75, stakeholder_alignment=80</input>
      <calculation>(80*0.4)+(75*0.3)+(80*0.3) = 32+22.5+24 = 78.5</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Core requirements clear but some questions pending results in 78.5, triggers warning</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>requirement_clarity=85, technical_feasibility=75, stakeholder_alignment=80</input>
      <calculation>(85*0.4)+(75*0.3)+(80*0.3) = 34+22.5+24 = 80.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>Weighted average 80.5 meets success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>requirement_clarity=60, technical_feasibility=55, stakeholder_alignment=60</input>
      <calculation>(60*0.4)+(55*0.3)+(60*0.3) = 24+16.5+18 = 58.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Weighted average 58.5 is below 60, triggers error</reasoning>
    </test>
    <test name="ambiguous_requirements">
      <input>requirement_clarity=50, technical_feasibility=55, stakeholder_alignment=45</input>
      <calculation>(50*0.4)+(55*0.3)+(45\*0.3) = 20+16.5+13.5 = 50</calculation>
      <expected_status>error</expected_status>
      <reasoning>Many unclear requirements with unanswered questions results in 50, triggers error</reasoning>
    </test>
  </validation_tests>
</decision_criteria>

<enforcement>
  <mandatory_behaviors>
    <behavior id="DEF-B001" priority="critical">
      <trigger>Before requirements documentation</trigger>
      <action>Investigate existing codebase patterns</action>
      <verification>Codebase analysis in output</verification>
    </behavior>
    <behavior id="DEF-B002" priority="critical">
      <trigger>For design decisions</trigger>
      <action>Use AskUserQuestion tool with structured options</action>
      <verification>User responses recorded</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="DEF-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Modifying or creating code files</action>
      <response>Block operation, this is read-only command</response>
    </behavior>
    <behavior id="DEF-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Proceeding without answering critical questions</action>
      <response>Block operation, require clarification first</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<output>
  <format>
    <requirements_document>
      <summary>One-sentence request, background, expected outcomes</summary>
      <current_state>Existing system, tech stack</current_state>
      <functional_requirements>FR-001 format (mandatory/optional)</functional_requirements>
      <non_functional_requirements>Performance, security, maintainability</non_functional_requirements>
      <technical_specifications>Design policies, impact scope, decisions</technical_specifications>
      <metrics>
        <metric name="feasibility">0-100</metric>
        <metric name="objectivity">0-100</metric>
      </metrics>
      <constraints>Technical, operational</constraints>
      <test_requirements>Unit, integration, acceptance criteria</test_requirements>
      <outstanding_issues>Unresolved questions</outstanding_issues>
    </requirements_document>
    <task_breakdown>
      <dependency_graph>Task dependencies visualization</dependency_graph>
      <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
      <execute_handoff>Decisions, references, constraints</execute_handoff>
    </task_breakdown>
    <self_feedback>
      <confidence>XX/100 (based on requirement_clarity)</confidence>
      <issues>
- [Critical] Issue description (if any, max 2 total)
- [Warning] Issue description (if any)
      </issues>
    </self_feedback>
  </format>
</output>

<error_escalation>
  <level severity="low">
    <example>Minor ambiguity in non-critical feature detail</example>
    <action>Note in report, proceed</action>
  </level>
  <level severity="medium">
    <example>Unclear requirement or ambiguous scope</example>
    <action>Document issue, use AskUserQuestion for clarification</action>
  </level>
  <level severity="high">
    <example>Technically infeasible request or breaking change</example>
    <action>STOP, present options to user</action>
  </level>
  <level severity="critical">
    <example>Request violates security policy or data integrity</example>
    <action>BLOCK operation, require explicit user acknowledgment</action>
  </level>
</error_escalation>

<related_commands>
  <command name="ask">When requirements raise technical questions</command>
  <command name="bug">When defining fix requirements for known issues</command>
  <command name="execute">Handoff point after requirements are defined</command>
</related_commands>

<related_skills>
  <skill name="requirements-definition">Core methodology for specification</skill>
  <skill name="investigation-patterns">Evidence gathering for feasibility</skill>
  <skill name="serena-usage">Check existing patterns and memories</skill>
  <skill name="fact-check">External source verification using Context7 and WebSearch</skill>
</related_skills>

<constraints>
  <must>Keep all operations read-only</must>
  <must>Delegate detailed investigation to sub-agents</must>
  <must>Use AskUserQuestion tool for structured user interactions</must>
  <must>Present questions before making assumptions</must>
  <avoid>Implementing or modifying code</avoid>
  <avoid>Justifying user requests over technical validity</avoid>
  <avoid>Proceeding without clear answers to critical questions</avoid>
  <avoid>Using plain text output for questions instead of AskUserQuestion tool</avoid>
</constraints>
