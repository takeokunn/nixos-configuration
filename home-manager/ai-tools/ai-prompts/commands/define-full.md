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
  <rule>Automatic flow between phases (no user confirmation at inter-phase transitions). EXCEPTION: the terminal finalize gate (define-core#core_finalize) runs after the last phase, not between phases — it is permitted to prompt the user when Outstanding Issues remain.</rule>
</rules>
<rules priority="standard">
  <rule>Use requirements-definition skill for methodology</rule>
  <rule>Delegate investigations to sub-agents</rule>
  <rule>Ask questions without limit until requirements are clear</rule>
  <rule>Investigate and question before concluding</rule>
  <rule>Always include a (Recommended) option when presenting choices via AskUserQuestion</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Conducting a single requirements review pass before finalizing — AI runs the full define → feedback → regenerate cycle automatically in one flow, incorporating multi-agent critique before the specification is considered complete</practice>
    <practice>Asking questions only at the start and then proceeding — AI must continue asking until all requirements are clear, pausing at any ambiguity regardless of which phase the workflow is in</practice>
    <practice>Treating feedback as optional post-processing — the feedback collection and regeneration phases are mandatory parts of the cycle, not enhancements; skipping them is prohibited</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Run all feedback agents (plan, estimation, validator, fact-check) in parallel after the initial specification is produced; no agent should block another since they evaluate independent dimensions</principle>
    <principle>Treat the regenerated specification as the authoritative output; the initial document is an intermediate artifact that must incorporate all critical and warning feedback before delivery</principle>
    <principle>Limit the cycle to exactly one iteration; a second regeneration pass indicates scope creep or insufficient initial clarification, both of which require user intervention rather than more automation</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="core_workflow" inherits="define-core#workflow" />

  <phase name="collect_feedback">
    <step order="1">
      <action>Launch plan agent: evaluate execution plan quality</action>
      <tool>Sub-agent delegation (plan)</tool>
      <output>Plan evaluation report</output>
    </step>
    <step order="2">
      <action>Launch estimation agent: evaluate estimation validity</action>
      <tool>Sub-agent delegation (general-purpose)</tool>
      <output>Estimation evaluation report</output>
    </step>
    <step order="3">
      <action>Launch validator agent: cross-validate requirements consistency</action>
      <tool>Sub-agent delegation (validator)</tool>
      <output>Validation report</output>
    </step>
    <step order="4">
      <action>Use fact-check skill patterns: verify external source claims via Context7</action>
      <tool>Context7 MCP, WebSearch</tool>
      <output>Fact-check report</output>
    </step>
  </phase>

  <reflection_checkpoint id="feedback_quality" after="collect_feedback">
      <questions>
        <question weight="0.4">Did all feedback agents complete successfully?</question>
        <question weight="0.3">Is the feedback specific and actionable?</question>
        <question weight="0.3">Are there critical issues requiring regeneration?</question>
      </questions>
      <threshold min="70" action="proceed">
        <below_threshold>Re-run failed agents or gather additional context</below_threshold>
      </threshold>
    </reflection_checkpoint>

  <phase name="regenerate">
    <step order="1">
      <action>Synthesize feedback from all agents</action>
      <tool>Feedback synthesis</tool>
      <output>Consolidated feedback summary</output>
    </step>
    <step order="2">
      <action>Identify critical issues requiring specification changes</action>
      <tool>Issue prioritization</tool>
      <output>Prioritized issue list</output>
    </step>
    <step order="3">
      <action>Update requirements document addressing critical and warning issues</action>
      <tool>Requirements revision</tool>
      <output>Updated requirements specification</output>
    </step>
    <step order="4">
      <action>Update task breakdown reflecting specification changes</action>
      <tool>Task revision</tool>
      <output>Updated phased task list</output>
    </step>
    <step order="5">
      <action>Calculate final confidence score</action>
      <tool>Decision criteria evaluation</tool>
      <output>Final confidence score</output>
    </step>
  </phase>

  <reflection_checkpoint id="regeneration_complete" after="regenerate">
    <questions>
      <question weight="0.4">Have all critical feedback items been addressed?</question>
      <question weight="0.3">Is the regenerated specification internally consistent?</question>
      <question weight="0.3">Is the final confidence score acceptable (>=70)?</question>
    </questions>
    <threshold min="70" action="complete">
      <below_threshold>Flag remaining issues for user review</below_threshold>
    </threshold>
  </reflection_checkpoint>

  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Handle execution errors and apply fallback strategy</action>
      <tool>Error analysis and retry policy</tool>
      <output>Recovered execution path or documented blocker</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="design" subagent_type="design" readonly="true">
    <role>Evaluate architecture consistency, component dependencies, and API design feasibility</role>
    <receives>component_names[], request_context, existing_architecture_paths[]</receives>
    <produces>architecture_assessment{consistency: 0-100, concerns[]}, dependency_impact[], design_alternatives[]</produces>
    <done_when>All affected architectural layers assessed; design alternatives identified for non-obvious decisions</done_when>
  </agent>
  <agent name="database" subagent_type="database" readonly="true">
    <role>Analyze data model requirements, schema implications, and query feasibility</role>
    <receives>entity_descriptions[], relationship_requirements, performance_constraints</receives>
    <produces>schema_proposal, migration_complexity: low|medium|high, query_feasibility_assessment</produces>
    <done_when>Data model changes fully specified; migration path and complexity assessed</done_when>
  </agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">
    <role>Analyze requirements completeness, estimate implementation effort, and identify dependency risks</role>
    <receives>functional_requirements[], technical_constraints[], existing_codebase_context</receives>
    <produces>effort_estimate{level: low|medium|high, rationale}, risk_assessment[], missing_requirements[], dependency_graph</produces>
    <done_when>All requirements analyzed for completeness; effort estimate justified with evidence</done_when>
  </agent>
  <agent name="explore" subagent_type="explore" readonly="true">
    <role>Find existing implementations, patterns, and code relevant to the requirements</role>
    <receives>feature_keywords[], suspected_file_paths[], pattern_descriptions[]</receives>
    <produces>existing_patterns[]{path: file:line, description}, similar_implementations[], reference_files[]</produces>
    <done_when>All relevant existing code located; similar patterns identified for reuse consideration</done_when>
  </agent>
  <agent name="validator" subagent_type="validator" readonly="true">
    <role>Cross-validate requirements consistency and flag contradictions between specifications</role>
    <receives>requirements_document, technical_constraints[], agent_findings[]</receives>
    <produces>consistency_report{consistent: bool, contradictions[]}, ambiguities[], validation_confidence: 0-100</produces>
    <done_when>All requirements cross-checked; no unresolved contradictions in final document</done_when>
  </agent>
  <agent name="plan" subagent_type="general-purpose" readonly="true">
    <role>Review and evaluate the execution plan quality for implementability and completeness</role>
    <receives>requirements_document, task_breakdown, dependency_graph, effort_estimates[]</receives>
    <produces>plan_assessment{score: 0-100, gaps[], risks[]}, phasing_recommendation, critical_path[]</produces>
    <done_when>Plan assessed for completeness and implementability; critical path identified</done_when>
  </agent>
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
  <sequential_step id="finalize" depends_on="regenerate">
    <action>Run the terminal finalize gate (define-core#core_finalize) on the FINAL regenerated document: if its Outstanding Issues section is non-empty, prompt the user once via AskUserQuestion (Resolve now / Defer to /execute / Stop &amp; revise scope) per DEFF-B007. This is the authoritative position of the gate — it runs after regenerate, never inside core_workflow.</action>
  </sequential_step>
</execution_graph>
<delegation>
  <requirement>Scope overview</requirement>
  <requirement>Target file paths</requirement>
  <requirement>Explicit edit prohibition</requirement>
  <requirement>Sub-agents must use AskUserQuestion tool for any user interactions</requirement>
</delegation>
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
      <calculation>(95*0.3)+(90*0.25)+(90*0.2)+(95*0.25) = 92.75</calculation>
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
      <outstanding_issues>Unresolved questions (if any remain); state "none" explicitly when there are none. This is the canonical section the finalize gate (DEFF-B007) inspects.</outstanding_issues>
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
    <behavior id="DEFF-B006" priority="critical">
      <trigger>After completing each requirements definition cycle (including regeneration cycles)</trigger>
      <action>Evaluate memory_auto_creation_triggers (serena-usage skill); if any trigger matched
        (architectural decisions discovered, conventions identified, novel patterns found),
        call list_memories then use edit_memory (existing topic) or write_memory (new topic).
        Note: write_memory is Serena memory only — this does not violate the read-only file constraint.</action>
      <verification>Memory operation recorded in output, or "persist: no triggers matched — skip"</verification>
    </behavior>
    <behavior id="DEFF-B007" priority="critical">
      <trigger>After the regenerate phase, when the final requirements document's remaining/outstanding issues are non-empty</trigger>
      <action>Run the inherited terminal finalize gate (define-core#core_finalize) EXACTLY ONCE, evaluating the FINAL (regenerated) document — never the initial document. The CANONICAL trigger section is the final document's &lt;outstanding_issues&gt; element (not the &lt;self_feedback&gt;&lt;remaining_issues&gt; summary, which is non-authoritative); fire when it is non-empty (not "none"). Offer "Resolve now (Recommended)" / "Defer to /execute" / "Stop &amp; revise scope". If the user picks "Resolve now", collect answers and patch the final document directly; do NOT trigger a second feedback/regenerate cycle (preserves DEFF-P004 maximum-one-iteration).</action>
      <verification>Finalize gate appears once at the end of output when final outstanding issues >= 1; no second regeneration cycle is run</verification>
    </behavior>
    <behavior id="DEFF-B008" priority="high">
      <trigger>After completing each requirements definition cycle (including regeneration cycles)</trigger>
      <action>Apply memory_staleness_verification (serena-usage skill) to any memory read via read_memory during this cycle; bump last-verified, correct, or archive as appropriate. Skip if no memories were read.</action>
      <verification>Staleness check outcome recorded in output, or "no memories read this cycle"</verification>
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
      <response>Block operation, maximum one iteration. EXCEPTION: the finalize gate's "Resolve now" path (DEFF-B007) edits the already-final document in place and is NOT a regeneration iteration — it does not re-run collect_feedback/regenerate, so it is permitted.</response>
    </behavior>
    <behavior id="DEFF-P005" priority="critical">
      <trigger>Between phases (inter-phase transitions only; NOT the terminal finalize gate)</trigger>
      <action>Requesting user confirmation to proceed</action>
      <response>Proceed automatically between phases. This does not apply to the terminal finalize gate (define-core#core_finalize), which runs after the final phase and is allowed to prompt when Outstanding Issues remain.</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor ambiguity in non-critical feature detail</example>
    <example severity="medium">Unclear requirement or ambiguous scope</example>
    <example severity="high">Technically infeasible request or breaking change</example>
    <example severity="critical">Request violates security policy or data integrity</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="define">Basic requirements definition without feedback loop</command>
  <command name="execute">Handoff point after requirements are defined</command>
  <command name="feedback">Standalone feedback command for reviewing work</command>
  <command name="ask">When requirements raise technical questions</command>
  <command name="bug">When defining fix requirements for known issues</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
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
  <must>Automatically proceed between phases without user confirmation, EXCEPT the terminal finalize gate (define-core#core_finalize), which may prompt once after the final phase when Outstanding Issues remain</must>
  <avoid>Implementing or modifying code</avoid>
  <avoid>Justifying user requests over technical validity</avoid>
  <avoid>Proceeding without clear answers to critical questions</avoid>
  <avoid>Using plain text output for questions instead of AskUserQuestion tool</avoid>
  <avoid>Multiple regeneration iterations (exactly one allowed)</avoid>
  <avoid>Sequential execution of independent feedback agents</avoid>
</constraints>
