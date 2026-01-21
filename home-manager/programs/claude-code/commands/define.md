---
argument-hint: [message]
description: Requirements definition command
---

<purpose>
Conduct detailed requirements definition before implementation, clarifying technical constraints, design policies, and specifications.
</purpose>

<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">requirements-definition</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>

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
      <action>Use fact-check skill patterns: verify external documentation and standard references via Context7</action>
      <tool>Context7 MCP, WebSearch</tool>
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
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
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
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
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
</agents>

<execution_graph>
  <parallel_group id="investigation" depends_on="none">
    <agent>explore</agent>
    <agent>design</agent>
    <agent>database</agent>
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

<parallelization inherits="parallelization-patterns#parallelization_readonly" />

<decision_criteria inherits="core-patterns#decision_criteria">
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

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <low>Minor ambiguity in non-critical feature detail</low>
    <medium>Unclear requirement or ambiguous scope</medium>
    <high>Technically infeasible request or breaking change</high>
    <critical>Request violates security policy or data integrity</critical>
  </examples>
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
