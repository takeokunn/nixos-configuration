---
argument-hint: [message]
description: Full requirements definition with feedback loop
---

<purpose>
Conduct detailed requirements definition with automatic feedback and regeneration cycle. Executes the complete define workflow, collects feedback from multiple agents, and regenerates an improved specification in a single automated flow.
</purpose>
<rules priority="critical">
  <rule>Never modify a file and never write code. This command produces the specification the user
    approves before work starts; implementing during it removes the approval step it exists to create.</rule>
  <rule>Run the whole cycle — define, then feedback, then regenerate. The regenerated document is the
    deliverable; the initial one is an intermediate that has not yet been critiqued.</rule>
  <rule>One iteration, no more. A second regeneration pass means the initial clarification was
    insufficient or the scope moved, and both are decisions for the user rather than problems to
    automate around.</rule>
</rules>
<rules priority="standard">
  <rule>Move between phases automatically, without asking the user to confirm each transition — the
    point of this command over /define is that the cycle completes in one flow. The terminal finalize
    gate is not an inter-phase transition: it runs after the last phase and may prompt.</rule>
  <rule>Keep asking until the requirements are unambiguous; there is no question budget here, and an
    ambiguity resolved now costs a sentence rather than a rewrite.</rule>
  <rule>Mark one option (Recommended) whenever AskUserQuestion presents choices, so the user is
    reviewing a proposal rather than doing the analysis themselves.</rule>
</rules>
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
  <phase name="load">
    <objective>Put the workflow this command runs into context, since it lives in a skill</objective>
    <step order="1">
      <action>Load define-core and requirements-definition with the Skill tool, before anything else.
        define-core holds the phase sequence the first third of this command executes — prepare,
        analyze, investigate, clarify, verify, document — plus the terminal finalize gate that
        DEFF-B007 runs at the very end. requirements-definition holds the methodology inside those
        phases. Neither is in context until the Skill tool loads it, so skipping this step leaves the
        command with no core workflow and no gate. Load fact-check as well, since the collect_feedback
        phase runs an external-claim check that depends on it.</action>
      <tool>Skill</tool>
      <output>The skills loaded, named; and the phase list define-core returned</output>
    </step>
  </phase>

  <phase name="core_workflow">
    <objective>Produce the initial requirements document</objective>
    <step order="1">
      <action>Run define-core's phases in its order, stopping before its finalize gate — the gate runs
        once at the end of this command against the regenerated document, never against this initial
        one. This phase's output is an intermediate artifact.</action>
      <output>Initial requirements document, marked as not yet critiqued</output>
    </step>
  </phase>

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
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name each feedback agent dispatched and whether it returned, so a silent failure is visible.</check>
    <check>For each agent, quote one finding that names a section of the document or a file:line. An agent
      that returned only general praise returned nothing actionable.</check>
    <check>List the critical issues that the regenerate phase must address, or state that none was raised.</check>
    <on_unmet>Re-run the agent that returned nothing usable, with the specific sections named in its prompt.
      Do not enter regenerate on feedback that cannot be acted on.</on_unmet>
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
      <action>Tag each requirement in the final document; downgrade any marked verified that cannot name
        the command run or the file:line read</action>
      <output>Tagged requirements, over-claims downgraded</output>
    </step>
  </phase>

  <reflection_checkpoint id="regeneration_complete" after="regenerate">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>For each critical feedback item, name the section of the final document that changed in response,
      or state why it was rejected. An unlisted item was dropped, not addressed.</check>
    <check>Name any requirement that still contradicts another, or state that the pairs flagged by the
      validator were re-read and are consistent.</check>
    <check>Name every requirement still resting on `assumed` evidence — these belong in Outstanding Issues,
      which is what the finalize gate reads.</check>
    <on_unmet>Address the unmet item in the document before finishing, or move it to Outstanding Issues so
      the finalize gate (DEFF-B007) puts it to the user.</on_unmet>
  </reflection_checkpoint>

</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
  <check>State that all three phases ran — define, feedback, regenerate — or name the one that did not and why.</check>
  <check>State that no file was created or modified — this command is read-only — or name the file that was.</check>
  <check>State that the finalize gate ran exactly once on the final document, or that Outstanding Issues read "none".</check>
  <on_unmet>Resolve the structural gap before delivering the final document.</on_unmet>
</reflection_checkpoint>
<agents>
  <agent name="design" subagent_type="design" readonly="true">
    <role>Evaluate architecture consistency, component dependencies, and API design feasibility</role>
    <receives>component_names[], request_context, existing_architecture_paths[]</receives>
    <produces>architecture_assessment{consistent: bool, concerns[]{file:line, description}}, dependency_impact[], design_alternatives[]</produces>
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
    <done_when>All requirements analyzed for completeness; effort estimate justified with evidence, and
      expressed in quantities that were counted — files touched, call sites returned by
      find_referencing_symbols, layers crossed, tests affected — never in clock hours. Wall-clock
      effort depends on who does the work and what interrupts them, neither observable from here, so
      an hour figure can only be borrowed from a training-data average and will be stated confidently
      beside the admission that the complexity is unknown</done_when>
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
    <produces>consistency_report{consistent: bool, contradictions[]}, ambiguities[], unchecked_requirements[]</produces>
    <done_when>All requirements cross-checked; no unresolved contradictions in final document</done_when>
  </agent>
  <agent name="plan" subagent_type="general-purpose" readonly="true">
    <role>Review and evaluate the execution plan quality for implementability and completeness</role>
    <receives>requirements_document, task_breakdown, dependency_graph, effort_estimates[]</receives>
    <produces>plan_assessment{gaps[]{section, what_is_missing}, risks[]}, phasing_recommendation, critical_path[]</produces>
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
<decision_criteria>
  <factor name="requirement_clarity" precedence="1">
    <unmet>A requirement admits two readings that would produce different implementations. Ask with
      AskUserQuestion; do not write the reading that is cheaper to specify.</unmet>
  </factor>
  <factor name="technical_feasibility" precedence="2">
    <unmet>The document assumes a capability — a library, an API, a schema column — that was not located
      in this codebase or confirmed via Context7. Verify it, or record it as an Outstanding Issue.</unmet>
  </factor>
  <factor name="stakeholder_alignment" precedence="3">
    <unmet>A design decision the user has not answered is being written as settled. Put it back to the
      user, or move it to Outstanding Issues so the finalize gate sees it.</unmet>
  </factor>
  <factor name="feedback_incorporation" precedence="4">
    <unmet>A critical or warning item from collect_feedback cannot be traced to a changed section of the
      final document or to a stated reason for rejection. Address it or record the rejection.</unmet>
  </factor>
  <resolution>Apply in precedence order. The first factor whose `unmet` condition holds decides what
    happens next; later factors are not consulted.</resolution>
</decision_criteria>
<output>
  <format>
    <initial_requirements_document>
      <summary>One-sentence request, background, expected outcomes</summary>
      <current_state>Existing system, tech stack</current_state>
      <functional_requirements>FR-001 format (mandatory/optional)</functional_requirements>
      <non_functional_requirements>Performance, security, maintainability</non_functional_requirements>
      <technical_specifications>Design policies, impact scope, decisions</technical_specifications>
      <constraints>Technical, operational</constraints>
      <test_requirements>Unit, integration, acceptance criteria</test_requirements>
      <outstanding_issues>Unresolved questions</outstanding_issues>
      <task_breakdown>
        <dependency_graph>Task dependencies visualization</dependency_graph>
        <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
      </task_breakdown>
    </initial_requirements_document>
    <feedback_summary>
      <agents_run>Each feedback agent named, with whether it returned and the one finding it contributed — a listed agent with no finding is a silent failure</agents_run>
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
        <verified_claims>Claims confirmed against external sources, each naming the source consulted</verified_claims>
        <flagged_claims>Claims that could not be confirmed against any source, and what would confirm them</flagged_claims>
      </fact_check_results>
    </feedback_summary>
    <final_requirements_document>
      <changes_from_initial>Summary of changes made based on feedback</changes_from_initial>
      <summary>One-sentence request, background, expected outcomes</summary>
      <current_state>Existing system, tech stack</current_state>
      <functional_requirements>FR-001 format (mandatory/optional)</functional_requirements>
      <non_functional_requirements>Performance, security, maintainability</non_functional_requirements>
      <technical_specifications>Design policies, impact scope, decisions</technical_specifications>
      <constraints>Technical, operational</constraints>
      <test_requirements>Unit, integration, acceptance criteria</test_requirements>
      <verification_performed>The exact command(s) run during investigation and their exit status, or
        "none run". State feasibility as the observable condition that supports it — which capability
        was located where, which one was not found — never as a score.</verification_performed>
      <outstanding_issues>Unresolved questions (if any remain); state "none" explicitly when there are none. This is the canonical section the finalize gate (DEFF-B007) inspects. It is also where a disagreement with the user goes: when the investigation reaches a different severity or priority than the user assigned, record both assessments and what each rests on, and hand the decision back rather than deferring silently (which buries the risk) or escalating silently (which overrides a call that was the user's).</outstanding_issues>
      <task_breakdown>
        <dependency_graph>Task dependencies visualization</dependency_graph>
        <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
        <execute_handoff>Decisions, references, constraints</execute_handoff>
      </task_breakdown>
      <self_feedback>
        <weakest_claim>The requirement resting on the thinnest evidence, and what would confirm it</weakest_claim>
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
            <description>Anything asked for that this document does not answer, with the reason — not attempted, blocked, or out of scope</description>
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
    <behavior id="DEFF-B002" priority="high">
      <trigger>For design decisions</trigger>
      <action>Use AskUserQuestion tool with structured options</action>
      <verification>User responses recorded</verification>
    </behavior>
    <behavior id="DEFF-B003" priority="high">
      <trigger>After initial requirements document</trigger>
      <action>Execute feedback collection phase</action>
      <verification>Feedback results in output</verification>
    </behavior>
    <behavior id="DEFF-B004" priority="high">
      <trigger>After feedback collection</trigger>
      <action>Execute regeneration phase</action>
      <verification>Regenerated specification in output</verification>
    </behavior>
    <behavior id="DEFF-B005" priority="high">
      <trigger>During feedback phase</trigger>
      <action>Launch all feedback agents in parallel</action>
      <verification>Parallel execution confirmed</verification>
    </behavior>
    <behavior id="DEFF-B006" priority="standard">
      <trigger>After completing each requirements definition cycle (including regeneration cycles)</trigger>
      <action>Evaluate memory_auto_creation_triggers (serena-usage skill); if any trigger matched
        (architectural decisions discovered, conventions identified, novel patterns found),
        call list_memories then use edit_memory (existing topic) or write_memory (new topic).
        Note: write_memory is Serena memory only — this does not violate the read-only file constraint.</action>
      <verification>Memory operation recorded in output, or "persist: no triggers matched — skip"</verification>
    </behavior>
    <behavior id="DEFF-B007" priority="critical">
      <trigger>After the regenerate phase, when the final requirements document's remaining/outstanding issues are non-empty</trigger>
      <action>Run the terminal finalize gate that define-core defines (core_finalize, available because
        the load phase loaded that skill) EXACTLY ONCE, evaluating the FINAL (regenerated) document — never the initial document. The CANONICAL trigger section is the final document's &lt;outstanding_issues&gt; element (not the &lt;self_feedback&gt;&lt;remaining_issues&gt; summary, which is non-authoritative); fire when it is non-empty (not "none"). Offer "Resolve now (Recommended)" / "Defer to /execute" / "Stop &amp; revise scope". If the user picks "Resolve now", collect answers and patch the final document directly; do NOT trigger a second feedback/regenerate cycle (preserves DEFF-P004 maximum-one-iteration).</action>
      <verification>Finalize gate appears once at the end of output when final outstanding issues >= 1; no second regeneration cycle is run</verification>
    </behavior>
    <behavior id="DEFF-B008" priority="standard">
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
    <behavior id="DEFF-P002" priority="high">
      <trigger>Always</trigger>
      <action>Proceeding without answering critical questions</action>
      <response>Block operation, require clarification first</response>
    </behavior>
    <behavior id="DEFF-P003" priority="high">
      <trigger>Always</trigger>
      <action>Skipping feedback or regeneration phases</action>
      <response>Block operation, full cycle required</response>
    </behavior>
    <behavior id="DEFF-P004" priority="critical">
      <trigger>Always</trigger>
      <action>Multiple regeneration iterations</action>
      <response>Block operation, maximum one iteration. EXCEPTION: the finalize gate's "Resolve now" path (DEFF-B007) edits the already-final document in place and is NOT a regeneration iteration — it does not re-run collect_feedback/regenerate, so it is permitted.</response>
    </behavior>
    <behavior id="DEFF-P005" priority="standard">
      <trigger>Between phases (inter-phase transitions only; NOT the terminal finalize gate)</trigger>
      <action>Requesting user confirmation to proceed</action>
      <response>Proceed automatically between phases. This does not apply to the terminal finalize gate (define-core#core_finalize), which runs after the final phase and is allowed to prompt when Outstanding Issues remain.</response>
    </behavior>
    <behavior id="DEFF-P006" priority="critical">
      <trigger>Always</trigger>
      <action>Scoring either document — feasibility, objectivity, confidence, completeness — on a
        numeric scale</action>
      <response>Block. A score has no derivation, so it cannot be checked or disputed, and it reads as
        a measurement. State the observable condition instead: which capability was found at which
        file:line, which one was not found and where it was searched for.</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation>
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
