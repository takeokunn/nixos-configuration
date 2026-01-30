---
name: Define Core
description: Shared workflow phases and patterns for requirements definition commands. Use this skill when implementing /define or /define-full commands to ensure consistent workflow structure, agent delegation, and requirements documentation patterns.
version: 0.1.0
---

<purpose>
Provide shared workflow phases, agent definitions, and patterns that are common to all requirements definition commands (define, define-full). This skill eliminates duplication and ensures consistency across requirements definition workflows.
</purpose>

<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">requirements-definition</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>

<workflow>
  <phase name="prepare" id="core_prepare">
    <objective>Initialize Serena and check existing patterns</objective>
    <step number="1">
      <action>Activate Serena project with activate_project</action>
      <tool>Serena activate_project</tool>
      <output>Project activated</output>
    </step>
    <step number="2">
      <action>Check list_memories for relevant patterns</action>
      <tool>Serena list_memories</tool>
      <output>Available memory list</output>
    </step>
    <step number="3">
      <action>Load applicable memories with read_memory</action>
      <tool>Serena read_memory</tool>
      <output>Relevant patterns loaded</output>
    </step>
  </phase>

  <phase name="analyze" id="core_analyze">
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

  <phase name="investigate" id="core_investigate">
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
    <serena_validation>
      <tool>think_about_collected_information</tool>
      <trigger>After investigation phase completes</trigger>
    </serena_validation>
  </reflection_checkpoint>

  <phase name="clarify" id="core_clarify">
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

  <phase name="verify" id="core_verify">
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

  <phase name="document" id="core_document">
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
</workflow>

<agents>
  <agent name="design" subagent_type="design" readonly="true">Architecture consistency, dependency analysis, API design</agent>
  <agent name="database" subagent_type="database" readonly="true">Database design and optimization</agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Requirements analysis, estimation, dependency analysis</agent>
  <agent name="explore" subagent_type="explore" readonly="true">Finding relevant files and existing patterns</agent>
  <agent name="validator" subagent_type="validator" readonly="true">Cross-validation and consensus verification</agent>
</agents>

<execution_graph id="core_execution_graph">
  <parallel_group id="investigation" depends_on="none">
    <agent>explore</agent>
    <agent>design</agent>
    <agent>database</agent>
  </parallel_group>
  <sequential_step id="analysis" depends_on="investigation">
    <agent>general-purpose</agent>
  </sequential_step>
</execution_graph>

<delegation>
  <requirement>Scope overview</requirement>
  <requirement>Target file paths</requirement>
  <requirement>Explicit edit prohibition</requirement>
  <requirement>Sub-agents must use AskUserQuestion tool for any user interactions</requirement>
</delegation>

<rules priority="critical">
  <rule id="DC-C001">Never modify, create, or delete files</rule>
  <rule id="DC-C002">Never implement code; requirements definition only</rule>
  <rule id="DC-C003">Clearly identify technically impossible requests</rule>
  <rule id="DC-C004">Prioritize technical validity over user preferences</rule>
  <rule id="DC-C005">Technical evidence over speculation</rule>
</rules>

<rules priority="standard">
  <rule id="DC-S001">Use requirements-definition skill for methodology</rule>
  <rule id="DC-S002">Delegate investigations to sub-agents</rule>
  <rule id="DC-S003">Ask questions without limit until requirements are clear</rule>
  <rule id="DC-S004">Investigate and question before concluding</rule>
  <rule id="DC-S005">Always include a (Recommended) option when presenting choices via AskUserQuestion</rule>
</rules>

<enforcement>
  <mandatory_behaviors>
    <behavior id="DC-B001" priority="critical">
      <trigger>Before requirements documentation</trigger>
      <action>Investigate existing codebase patterns</action>
      <verification>Codebase analysis in output</verification>
    </behavior>
    <behavior id="DC-B002" priority="critical">
      <trigger>For design decisions</trigger>
      <action>Use AskUserQuestion tool with structured options</action>
      <verification>User responses recorded</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="DC-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Modifying or creating code files</action>
      <response>Block operation, this is read-only command</response>
    </behavior>
    <behavior id="DC-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Proceeding without answering critical questions</action>
      <response>Block operation, require clarification first</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<output>
  <format id="requirements_document">
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
  </format>
  <format id="task_breakdown">
    <dependency_graph>Task dependencies visualization</dependency_graph>
    <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
    <execute_handoff>Decisions, references, constraints</execute_handoff>
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

<best_practices>
  <practice priority="critical">Always initialize Serena and check memories before starting requirements definition</practice>
  <practice priority="critical">Investigate existing codebase patterns before documenting any requirements</practice>
  <practice priority="critical">Use AskUserQuestion tool with structured options (2-4 choices) for all user interactions</practice>
  <practice priority="critical">Always include a (Recommended) option when presenting choices</practice>
  <practice priority="high">Score questions using 4-criteria system (design branching, irreversibility, investigation impossibility, effort impact)</practice>
  <practice priority="high">Delegate investigation tasks to specialized agents in parallel</practice>
  <practice priority="high">Verify external documentation claims via Context7 and fact-check patterns</practice>
  <practice priority="medium">Classify questions by type (spec confirmation, design choice, constraint, scope, priority)</practice>
  <practice priority="medium">Document all assumptions explicitly when requirements are unclear</practice>
</best_practices>

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

<related_skills>
  <skill name="requirements-definition">Core methodology for specification (question scoring, requirement formatting)</skill>
  <skill name="investigation-patterns">Evidence gathering for feasibility assessment</skill>
  <skill name="serena-usage">Check existing patterns and memories via Serena MCP</skill>
  <skill name="fact-check">External source verification using Context7 and WebSearch</skill>
  <skill name="execution-workflow">Handoff methodology after requirements are defined</skill>
</related_skills>

<usage_in_commands>
  <command name="define">
    <inherits>All phases (prepare, analyze, investigate, clarify, verify, document)</inherits>
    <inherits>All agents</inherits>
    <inherits>Core execution graph</inherits>
    <inherits>All delegation requirements</inherits>
    <inherits>All rules and enforcement</inherits>
  </command>
  <command name="define-full">
    <inherits>All phases as subphases within define_initial</inherits>
    <inherits>All agents plus plan agent</inherits>
    <inherits>Core execution graph as base, extended with feedback and regenerate phases</inherits>
    <inherits>All delegation requirements</inherits>
    <inherits>All rules and enforcement, plus feedback-specific behaviors</inherits>
  </command>
</usage_in_commands>
