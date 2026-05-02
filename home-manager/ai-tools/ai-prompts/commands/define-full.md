---
argument-hint: [message]
description: Full requirements definition with feedback loop
---

<purpose>
Conduct detailed requirements definition with automatic feedback and regeneration cycle. Executes the complete define workflow, collects feedback from multiple agents, and regenerates an improved specification in a single automated flow.
</purpose>

<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">requirements-definition</skill>
  <skill use="workflow">execution-workflow</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
  <skill use="workflow">define-core</skill>
</refs>

<rules priority="critical">
  <rule>Never modify, create, or delete files</rule>
  <rule>Never implement code; requirements definition only</rule>
  <rule>Complete full cycle: define -> feedback -> regenerate</rule>
  <rule>Maximum one iteration (no infinite loops)</rule>
  <rule>Automatic flow between phases (no user confirmation between phases)</rule>
</rules>

<rules priority="standard">
  <rule>Use requirements-definition skill for methodology</rule>
  <rule>Delegate investigations to sub-agents</rule>
  <rule>Ask questions without limit until requirements are clear</rule>
  <rule>Investigate and question before concluding</rule>
  <rule>Always include a (Recommended) option when presenting choices via AskUserQuestion</rule>
</rules>

<workflow>
  <phase name="define_initial">
    <objective>Execute core define phases to produce initial requirements document</objective>
    <subphase name="prepare">
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
    </subphase>
    <subphase name="analyze">
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
    </subphase>
    <subphase name="investigate">
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
    </subphase>
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
        <trigger>After investigation subphase completes</trigger>
      </serena_validation>
    </reflection_checkpoint>
    <subphase name="clarify">
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
    </subphase>
    <subphase name="verify">
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
    </subphase>
    <subphase name="document">
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
    </subphase>
  </phase>

  <phase name="collect_feedback">
    <objective>Launch feedback agents in define mode and collect evaluation results</objective>
    <step number="1">
      <action>Launch plan agent: evaluate execution plan quality</action>
      <tool>Sub-agent delegation (plan)</tool>
      <aspects>Step granularity, dependencies, risk identification, completeness, feasibility</aspects>
      <output>Plan evaluation report</output>
    </step>
    <step number="2">
      <action>Launch estimation agent: evaluate estimation validity</action>
      <tool>Sub-agent delegation (general-purpose)</tool>
      <aspects>Effort accuracy, risk assessment, dependency completeness</aspects>
      <output>Estimation evaluation report</output>
    </step>
    <step number="3">
      <action>Launch validator agent: cross-validate requirements consistency</action>
      <tool>Sub-agent delegation (validator)</tool>
      <aspects>Internal consistency, completeness, contradiction detection</aspects>
      <output>Validation report</output>
    </step>
    <step number="4">
      <action>Use fact-check skill patterns: verify external source claims via Context7</action>
      <tool>Context7 MCP, WebSearch</tool>
      <output>Fact-check report</output>
    </step>
    <execution_mode>All agents in parallel</execution_mode>
    <reflection_checkpoint id="feedback_quality" after="collect_feedback">
      <questions>
        <question weight="0.4">Did all feedback agents complete successfully?</question>
        <question weight="0.3">Is the feedback specific and actionable?</question>
        <question weight="0.3">Are there critical issues requiring regeneration?</question>
      </questions>
      <threshold min="70" action="proceed">
        <below_threshold>Re-run failed agents or gather additional context</below_threshold>
      </threshold>
      <serena_validation>
        <tool>think_about_collected_information</tool>
        <trigger>After feedback collection completes</trigger>
      </serena_validation>
    </reflection_checkpoint>
  </phase>

  <phase name="regenerate">
    <objective>Incorporate feedback and generate complete updated specification</objective>
    <step number="1">
      <action>Synthesize feedback from all agents</action>
      <tool>Feedback synthesis</tool>
      <output>Consolidated feedback summary</output>
    </step>
    <step number="2">
      <action>Identify critical issues requiring specification changes</action>
      <tool>Issue prioritization</tool>
      <output>Prioritized issue list</output>
    </step>
    <step number="3">
      <action>Update requirements document addressing critical and warning issues</action>
      <tool>Requirements revision</tool>
      <output>Updated requirements specification</output>
    </step>
    <step number="4">
      <action>Update task breakdown reflecting specification changes</action>
      <tool>Task revision</tool>
      <output>Updated phased task list</output>
    </step>
    <step number="5">
      <action>Calculate final confidence score</action>
      <tool>Decision criteria evaluation</tool>
      <output>Final confidence score</output>
    </step>
    <reflection_checkpoint id="regeneration_complete" after="regenerate">
      <questions>
        <question weight="0.4">Have all critical feedback items been addressed?</question>
        <question weight="0.3">Is the regenerated specification internally consistent?</question>
        <question weight="0.3">Is the final confidence score acceptable (>=70)?</question>
      </questions>
      <threshold min="70" action="complete">
        <below_threshold>Flag remaining issues for user review</below_threshold>
      </threshold>
      <serena_validation>
        <tool>think_about_whether_you_are_done</tool>
        <trigger>Before final output</trigger>
      </serena_validation>
    </reflection_checkpoint>
  </phase>

  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
</workflow>

<agents>
  <agent name="design" subagent_type="design" readonly="true">Architecture consistency, dependency analysis, API design</agent>
  <agent name="database" subagent_type="database" readonly="true">Database design and optimization</agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Requirements analysis, estimation, dependency analysis</agent>
  <agent name="explore" subagent_type="explore" readonly="true">Finding relevant files and existing patterns</agent>
  <agent name="validator" subagent_type="validator" readonly="true">Cross-validation and consensus verification</agent>
  <agent name="plan" subagent_type="Plan" readonly="true">Execution plan review and evaluation</agent>
</agents>

<execution_graph>
  <parallel_group id="investigation" depends_on="none">
    <agent>explore</agent>
    <agent>design</agent>
    <agent>database</agent>
  </parallel_group>
  <sequential_step id="analysis" depends_on="investigation">
    <agent>general-purpose</agent>
  </sequential_step>
  <sequential_step id="document" depends_on="analysis">
    <action>Create initial requirements document</action>
  </sequential_step>
  <parallel_group id="feedback" depends_on="document">
    <agent>plan</agent>
    <agent>general-purpose</agent>
    <agent>validator</agent>
  </parallel_group>
  <sequential_step id="regenerate" depends_on="feedback">
    <action>Synthesize feedback and regenerate specification</action>
  </sequential_step>
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
    <factor name="requirement_clarity" weight="0.3">
      <score range="90-100">All requirements clear and documented</score>
      <score range="70-89">Core requirements clear</score>
      <score range="50-69">Some ambiguity remains</score>
      <score range="0-49">Many unclear requirements</score>
    </factor>
    <factor name="technical_feasibility" weight="0.25">
      <score range="90-100">Feasibility confirmed with evidence</score>
      <score range="70-89">Likely feasible</score>
      <score range="50-69">Uncertain feasibility</score>
      <score range="0-49">Likely infeasible</score>
    </factor>
    <factor name="stakeholder_alignment" weight="0.2">
      <score range="90-100">All questions answered by user</score>
      <score range="70-89">Most questions answered</score>
      <score range="50-69">Some questions pending</score>
      <score range="0-49">Many questions unanswered</score>
    </factor>
    <factor name="feedback_incorporation" weight="0.25">
      <score range="90-100">All critical feedback addressed</score>
      <score range="70-89">Most feedback addressed</score>
      <score range="50-69">Some feedback addressed</score>
      <score range="0-49">Feedback not incorporated</score>
    </factor>
  </criterion>
  <criterion name="regeneration_validation">
    <factor name="critical_issues_resolved" weight="0.5">All critical issues from feedback addressed</factor>
    <factor name="internal_consistency" weight="0.3">No contradictions in regenerated specification</factor>
    <factor name="completeness" weight="0.2">All required sections present and populated</factor>
  </criterion>
  <validation_tests>
    <test name="success_case">
      <input>requirement_clarity=95, technical_feasibility=90, stakeholder_alignment=90, feedback_incorporation=95</input>
      <calculation>(95*0.3)+(90*0.25)+(90*0.2)+(95*0.25) = 92.25</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>requirement_clarity=80, technical_feasibility=80, stakeholder_alignment=80, feedback_incorporation=80</input>
      <calculation>(80*0.3)+(80*0.25)+(80*0.2)+(80*0.25) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>requirement_clarity=79, technical_feasibility=79, stakeholder_alignment=79, feedback_incorporation=79</input>
      <calculation>(79*0.3)+(79*0.25)+(79*0.2)+(79*0.25) = 79</calculation>
      <expected_status>warning</expected_status>
      <reasoning>79 is below success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>requirement_clarity=59, technical_feasibility=59, stakeholder_alignment=59, feedback_incorporation=59</input>
      <calculation>(59*0.3)+(59*0.25)+(59*0.2)+(59*0.25) = 59</calculation>
      <expected_status>error</expected_status>
      <reasoning>59 is at error threshold</reasoning>
    </test>
    <test name="error_case">
      <input>requirement_clarity=40, technical_feasibility=50, stakeholder_alignment=45, feedback_incorporation=40</input>
      <calculation>(40*0.3)+(50*0.25)+(45*0.2)+(40*0.25) = 43.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores yield error status</reasoning>
    </test>
  </validation_tests>
</decision_criteria>

<enforcement>
  <mandatory_behaviors>
    <behavior id="DEFF-B001" priority="critical">
      <trigger>Before requirements documentation</trigger>
      <action>Investigate existing codebase patterns</action>
      <verification>Codebase analysis in output</verification>
    </behavior>
    <behavior id="DEFF-B002" priority="critical">
      <trigger>For design decisions</trigger>
      <action>Use AskUserQuestion tool with structured options</action>
      <verification>User responses recorded</verification>
    </behavior>
    <behavior id="DEFF-B003" priority="critical">
      <trigger>After initial requirements document</trigger>
      <action>Execute feedback collection phase</action>
      <verification>Feedback results in output</verification>
    </behavior>
    <behavior id="DEFF-B004" priority="critical">
      <trigger>After feedback collection</trigger>
      <action>Execute regeneration phase</action>
      <verification>Regenerated specification in output</verification>
    </behavior>
    <behavior id="DEFF-B005" priority="critical">
      <trigger>During feedback phase</trigger>
      <action>Launch all feedback agents in parallel</action>
      <verification>Parallel execution confirmed</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="DEFF-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Modifying or creating code files</action>
      <response>Block operation, this is read-only command</response>
    </behavior>
    <behavior id="DEFF-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Proceeding without answering critical questions</action>
      <response>Block operation, require clarification first</response>
    </behavior>
    <behavior id="DEFF-P003" priority="critical">
      <trigger>Always</trigger>
      <action>Skipping feedback or regeneration phases</action>
      <response>Block operation, full cycle required</response>
    </behavior>
    <behavior id="DEFF-P004" priority="critical">
      <trigger>Always</trigger>
      <action>Multiple regeneration iterations</action>
      <response>Block operation, maximum one iteration</response>
    </behavior>
    <behavior id="DEFF-P005" priority="critical">
      <trigger>Between phases</trigger>
      <action>Requesting user confirmation to proceed</action>
      <response>Proceed automatically between phases</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<output>
  <format>
    <initial_requirements_document>
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
      <task_breakdown>
        <dependency_graph>Task dependencies visualization</dependency_graph>
        <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
      </task_breakdown>
    </initial_requirements_document>
    <feedback_summary>
      <evaluation_scores>
        <metric name="plan_quality">XX/100</metric>
        <metric name="estimation_validity">XX/100</metric>
        <metric name="internal_consistency">XX/100</metric>
        <metric name="overall">XX/100</metric>
      </evaluation_scores>
      <critical_issues>
        <issue>
          <category>Category</category>
          <description>Issue description</description>
          <problem>Description</problem>
          <impact>What this affects</impact>
        </issue>
      </critical_issues>
      <warnings>
        <warning>
          <category>Category</category>
          <description>Issue description</description>
          <problem>Description</problem>
          <recommendation>Suggested change</recommendation>
        </warning>
      </warnings>
      <good_practices>
        <practice>
          <category>Category</category>
          <description>Commendable aspects</description>
        </practice>
      </good_practices>
      <fact_check_results>
        <verified_claims>Claims confirmed against external sources</verified_claims>
        <flagged_claims>Claims with verification confidence below 80</flagged_claims>
      </fact_check_results>
    </feedback_summary>
    <final_requirements_document>
      <changes_from_initial>Summary of changes made based on feedback</changes_from_initial>
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
      <outstanding_issues>Unresolved questions (if any remain)</outstanding_issues>
      <task_breakdown>
        <dependency_graph>Task dependencies visualization</dependency_graph>
        <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
        <execute_handoff>Decisions, references, constraints</execute_handoff>
      </task_breakdown>
      <self_feedback>
        <confidence>XX/100 (based on decision_criteria)</confidence>
        <feedback_addressed>
          <item>
            <status>Addressed</status>
            <issue>Critical issue 1</issue>
            <resolution>How resolved</resolution>
          </item>
          <item>
            <status>Addressed</status>
            <issue>Warning 1</issue>
            <resolution>How resolved</resolution>
          </item>
        </feedback_addressed>
        <remaining_issues>
          <item>
            <status>Note</status>
            <description>Any unresolved items requiring user attention</description>
          </item>
        </remaining_issues>
      </self_feedback>
    </final_requirements_document>
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
  <command name="define">Basic requirements definition without feedback loop</command>
  <command name="execute">Handoff point after requirements are defined</command>
  <command name="feedback">Standalone feedback command for reviewing work</command>
  <command name="ask">When requirements raise technical questions</command>
  <command name="bug">When defining fix requirements for known issues</command>
</related_commands>

<related_skills>
  <skill name="requirements-definition">Core methodology for specification</skill>
  <skill name="execution-workflow">Understanding work review methodology</skill>
  <skill name="investigation-patterns">Evidence gathering for feasibility</skill>
  <skill name="serena-usage">Check existing patterns and memories</skill>
  <skill name="fact-check">External source verification using Context7 and WebSearch</skill>
</related_skills>

<constraints>
  <must>Keep all operations read-only</must>
  <must>Delegate detailed investigation to sub-agents</must>
  <must>Use AskUserQuestion tool for structured user interactions</must>
  <must>Present questions before making assumptions</must>
  <must>Complete all three phases: define, feedback, regenerate</must>
  <must>Execute feedback agents in parallel</must>
  <must>Automatically proceed between phases without user confirmation</must>
  <avoid>Implementing or modifying code</avoid>
  <avoid>Justifying user requests over technical validity</avoid>
  <avoid>Proceeding without clear answers to critical questions</avoid>
  <avoid>Using plain text output for questions instead of AskUserQuestion tool</avoid>
  <avoid>Multiple regeneration iterations (exactly one allowed)</avoid>
  <avoid>Sequential execution of independent feedback agents</avoid>
</constraints>
