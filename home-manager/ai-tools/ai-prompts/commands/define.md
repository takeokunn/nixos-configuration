---
argument-hint: [message]
description: Requirements definition command
---

<purpose>
Conduct detailed requirements definition before implementation, clarifying technical constraints, design policies, and specifications.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="patterns">parallelization-patterns</skill>
  <skill use="workflow">requirements-definition</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="workflow">define-core</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<scope>
  <when_to_use>
    <case>Unclear scope with multiple design choices (e.g., "add authentication", "refactor data layer")</case>
    <case>Multi-component changes spanning 3+ files or 2+ system layers</case>
    <case>Irreversible or high-risk changes (schema migrations, API breaking changes, auth rework)</case>
    <case>Before large implementations where misunderstanding = wasted work</case>
    <case>When the user's request has ambiguous framing that may not reflect the real need</case>
  </when_to_use>
  <when_not_to_use>
    <case>Simple bug with a clear, isolated cause — use /bug instead</case>
    <case>Minor one-line changes or already fully-specified tasks — use /execute instead</case>
    <case>Already-defined requirements that just need technical investigation — use /ask instead</case>
    <case>Documentation-only changes — use /markdown instead</case>
  </when_not_to_use>
</scope>
<rules priority="critical">
  <rule>Never modify, create, or delete files</rule>
  <rule>Never implement code; requirements definition only</rule>
  <rule>Clearly identify technically impossible requests</rule>
  <rule>Prioritize technical validity over user preferences</rule>
  <rule>Technical evidence over speculation</rule>
  <rule>Challenge the user's framing — the stated problem may not be the real problem</rule>
  <rule>Form hypotheses before concluding; signal detection → hypothesis → verification → conclusion</rule>
</rules>
<rules priority="standard">
  <rule>Use requirements-definition skill for methodology</rule>
  <rule>Delegate investigations to sub-agents</rule>
  <rule>Ask questions without limit until requirements are clear</rule>
  <rule>Investigate and question before concluding</rule>
  <rule>Always include a (Recommended) option when presenting choices via AskUserQuestion</rule>
  <rule>Think in Why → How → What order (Golden Circle); output may differ, but reasoning must start from Why</rule>
  <rule>Specify only what is necessary; do not over-specify obvious implementation details</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Refining requirements incrementally through many small meetings — AI can gather and synthesize all available signals in a single investigation pass</practice>
    <practice>Writing requirements documents as the primary artifact of a meeting process — requirements must be grounded in codebase evidence, not just stakeholder conversation</practice>
    <practice>Deferring technical feasibility to a later phase — AI can verify feasibility during the same session as requirement gathering</practice>
    <practice>One question at a time, waiting for async responses — AI can batch-score all questions and ask the highest-priority ones first in structured form</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Investigate the full blast radius in a single pass before formulating any questions — don't ask what can be verified</principle>
    <principle>When scope is unclear, always start with the minimum viable scope and expand only when necessity is demonstrated</principle>
    <principle>Parallelise independent investigation dimensions (architecture, data, API, effort) rather than proceeding sequentially</principle>
    <principle>The harder and more irreversible a design decision, the more it deserves a dedicated question; trivial decisions should be resolved by investigation, not conversation</principle>
  </applicable_ai_principles>
</ai_principles>
<thinking_framework>
  <golden_circle>
    <principle>Always reason in this order: Why (motivation, constraints, goals) → How (approach, design decisions) → What (specific requirements, acceptance criteria). Output structure may vary, but internal reasoning must start from Why.</principle>
  </golden_circle>
  <abstraction_levels>
    <level id="L0" name="System/Architecture">Which systems, services, or components are affected? What cross-cutting concerns exist?</level>
    <level id="L1" name="Data/Schema">What data structures, database schemas, or state changes are involved?</level>
    <level id="L2" name="Interface/API">What APIs, contracts, or public interfaces change?</level>
    <level id="L3" name="Business Logic/Flow">What business rules, processing flows, or state transitions change?</level>
    <level id="L4" name="Implementation Detail">What specific files, functions, or configuration need to change?</level>
  </abstraction_levels>
  <investigation_depth>
    <rule>Start from L0 (big picture) before diving into L4 (details). Never jump to implementation detail before understanding system impact.</rule>
    <rule>Delegate investigation to appropriate sub-agents per level; synthesize findings yourself.</rule>
  </investigation_depth>
</thinking_framework>
<bias_correction>
  <known_bias id="BC-001">
    <pattern>Accepting the user's stated solution as the requirement</pattern>
    <correction>Distinguish the problem (what needs to change) from the proposed solution (how they think it should change). Requirements define the problem; solutions come later.</correction>
  </known_bias>
  <known_bias id="BC-002">
    <pattern>Jumping to implementation-level requirements before understanding system impact</pattern>
    <correction>Always assess L0 (system/architecture impact) before specifying L4 (implementation details).</correction>
  </known_bias>
  <known_bias id="BC-003">
    <pattern>Over-specifying obvious details while under-specifying the hard parts</pattern>
    <correction>Focus specification effort on decision points (irreversible choices, design branches, non-obvious constraints). Don't document what any competent implementer would naturally do.</correction>
  </known_bias>
  <known_bias id="BC-004">
    <pattern>Treating the first framing as the correct framing</pattern>
    <correction>Rephrase the request in "subject → object → operation" form to expose hidden ambiguity. If the rephrasing feels different from the original, there is a clarification gap.</correction>
  </known_bias>
  <known_bias id="BC-005">
    <pattern>Assuming all capabilities exist before verifying</pattern>
    <correction>Before designing around a framework feature, library, or system capability, verify it exists in the current codebase. Memory about past states may be stale.</correction>
  </known_bias>
</bias_correction>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<workflow inherits="define-core#workflow" />
<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<common_investigation_workflows>
  <playbook id="A" name="New Feature Definition">
    <step order="1">
      <action>Identify which L0 systems are affected (new vs. extending existing)</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="2">
      <action>Find similar existing implementations as reference patterns</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="3">
      <action>Map data model changes (L1) and API changes (L2)</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="4">
      <action>Identify acceptance criteria from the user's goal, not their proposed solution</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
  </playbook>
  <playbook id="B" name="Refactor / Architecture Change">
    <step order="1">
      <action>Map current boundaries, dependencies, and change reasons (change-axis analysis)</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="2">
      <action>Identify what stays the same (stable) vs. what changes (variable)</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="3">
      <action>Detect invariants that must not break across the refactor</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="4">
      <action>Estimate blast radius: how many modules are affected by each design choice</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
  </playbook>
  <playbook id="C" name="Bug Fix Specification">
    <step order="1">
      <action>Reproduce and confirm the failure mode with evidence from code/logs</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="2">
      <action>Distinguish root cause from symptoms</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="3">
      <action>Identify all places where the same root cause could recur</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="4">
      <action>Specify acceptance criteria as observable behavior, not internal mechanism</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
  </playbook>
  <playbook id="D" name="Integration / External System">
    <step order="1">
      <action>Verify external system capabilities via Context7 or documentation (don't assume)</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="2">
      <action>Map authentication, rate limits, and error contracts</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="3">
      <action>Identify data translation boundaries (what transforms, what passes through)</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
    <step order="4">
      <action>Define fallback behavior for external failures</action>
      <tool>Requirements analysis and evidence gathering tools</tool>
      <output>Investigation step result</output>
    </step>
  </playbook>
</common_investigation_workflows>
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
  <validation_tests>
    <test name="success_case">
      <input>requirement_clarity=95, technical_feasibility=90, stakeholder_alignment=90</input>
      <calculation>(95*0.4)+(90*0.3)+(90*0.3) = 92</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>requirement_clarity=80, technical_feasibility=80, stakeholder_alignment=80</input>
      <calculation>(80*0.4)+(80*0.3)+(80*0.3) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>requirement_clarity=80, technical_feasibility=78, stakeholder_alignment=78</input>
      <calculation>(80*0.4)+(78*0.3)+(78*0.3) = 78.8</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Score below 80 but above 60 triggers warning</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>requirement_clarity=60, technical_feasibility=58, stakeholder_alignment=58</input>
      <calculation>(60*0.4)+(58*0.3)+(58*0.3) = 58.8</calculation>
      <expected_status>error</expected_status>
      <reasoning>Score below 60 triggers error status</reasoning>
    </test>
    <test name="error_case">
      <input>requirement_clarity=40, technical_feasibility=30, stakeholder_alignment=30</input>
      <calculation>(40*0.4)+(30*0.3)+(30*0.3) = 34</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores across all factors result in error</reasoning>
    </test>
  </validation_tests>
</decision_criteria>
<anti_patterns>
  <pattern id="AP-001" name="Solution acceptance without problem validation">
    Accepting the user's proposed solution as the requirement. Requirements must describe the problem to solve; solutions are proposed during /execute, not /define.
  </pattern>
  <pattern id="AP-002" name="Investigation skip">
    Jumping to requirement documentation without codebase investigation. Every requirement must be grounded in evidence from the existing system.
  </pattern>
  <pattern id="AP-003" name="Implementation-level over-specification">
    Writing requirements that dictate function names, variable names, or algorithmic details. Specify behavior and constraints; leave implementation to the implementer.
  </pattern>
  <pattern id="AP-004" name="Confirmation without challenge">
    Confirming requirements without questioning whether the user's framing is correct. Always probe the Why before accepting the What.
  </pattern>
  <pattern id="AP-005" name="Capability assumption">
    Designing requirements around a library feature or system capability without verifying it exists in the current codebase state.
  </pattern>
  <pattern id="AP-006" name="Completeness theater">
    Writing an exhaustively detailed document that covers obvious things but glosses over the genuinely hard design decisions. Length is not quality.
  </pattern>
  <pattern id="AP-007" name="Partial scope definition">
    Defining requirements for one component while ignoring related components that will be affected. Requirements must account for the full blast radius.
  </pattern>
</anti_patterns>
<request_signals>
  <description>When reading a user's request, detect these signals before forming questions. Signal → hypothesis → verify → conclude. Never skip directly from signal to question.</description>
  <signal pattern="User describes a solution ('add X', 'change Y to Z', 'use library A')">
    <indicates>The real requirement may be hidden behind the proposed solution. The user has already narrowed to an approach.</indicates>
    <investigate>What problem does this solution solve? Are there simpler solutions? Does the proposed solution fit the existing architecture?</investigate>
  </signal>
  <signal pattern="User describes behavior they want ('make it faster', 'show errors', 'support X format')">
    <indicates>Acceptance criteria may be clear, but scope and implementation approach are open.</indicates>
    <investigate>Which components own the behavior? What are the measurable thresholds? What constraints apply?</investigate>
  </signal>
  <signal pattern="User references a bug or regression ('it broke', 'this stopped working', 'used to work')">
    <indicates>Root cause and symptom may differ. Fix scope may be broader than the reported location.</indicates>
    <investigate>When did it break? What changed? Are there other locations with the same root cause?</investigate>
  </signal>
  <signal pattern="User uses vague scope words ('everywhere', 'all', 'the whole', 'everywhere we do X')">
    <indicates>Scope is likely under-defined. 'All' almost never means all — it means the places the user is aware of.</indicates>
    <investigate>Enumerate the actual locations. Verify completeness by searching the codebase, not by trusting the description.</investigate>
  </signal>
  <signal pattern="User requests something that requires a capability not yet in the codebase">
    <indicates>Hidden dependency on a library, service, or infrastructure that doesn't exist yet.</indicates>
    <investigate>Does this capability exist? What is the introduction cost? Is there a simpler alternative using existing primitives?</investigate>
  </signal>
  <signal pattern="User says 'just' or 'simple' ('just add a field', 'simple change')">
    <indicates>The user may be unaware of blast radius. 'Simple' changes often have non-simple dependencies.</indicates>
    <investigate>Map all dependents. Check schema migrations, API consumers, test coverage, and downstream effects.</investigate>
  </signal>
</request_signals>
<minimum_viable_scope>
  <principle>Always start requirements with the minimum scope that satisfies the user's core need. Expand only when a concrete necessity is demonstrated — not when it seems useful or might be needed later.</principle>
  <checklist>
    <item>Can the core need be satisfied with fewer components than initially described?</item>
    <item>Are any parts of the request "nice to have" rather than load-bearing for the stated goal?</item>
    <item>Does any requirement exist only because the user assumed it was needed, not because the goal requires it?</item>
    <item>Is there a phased approach where Phase 1 delivers value and Phase 2 can be deferred?</item>
  </checklist>
  <anti_scope_creep>Do not include requirements that address hypothetical future needs. Three similar future cases are needed before generalising — specifying for one imagined future case creates premature scope.</anti_scope_creep>
</minimum_viable_scope>
<output>
  <format>
    <requirements_document>
      <section name="Summary" required="always">One-sentence request, background (Why), expected outcomes</section>
      <section name="Current State" required="always">Existing system, tech stack, affected components</section>
      <section name="Functional Requirements" required="always">FR-001 format (mandatory/optional); behavior-level, not implementation-level</section>
      <section name="Non-Functional Requirements" required="when-applicable">Performance, security, maintainability</section>
      <section name="Technical Specifications" required="always">Design policies, impact scope, key decisions and rationale</section>
      <section name="Architecture Impact" required="when-multi-layer">System diagram (Mermaid) if 2+ layers affected; dependency changes</section>
      <section name="Data / Schema Changes" required="when-applicable">ERD or schema diff if data model changes</section>
      <section name="Interface / API Changes" required="when-applicable">Endpoint table or contract diff if public interfaces change</section>
      <section name="Metrics" required="always">
        <metric name="feasibility">0-100 with evidence</metric>
        <metric name="objectivity">0-100</metric>
      </section>
      <section name="Constraints" required="always">Technical, operational</section>
      <section name="Test Requirements" required="always">Unit, integration, acceptance criteria as observable behavior</section>
      <section name="Outstanding Issues" required="always">Unresolved questions; "none" must be explicitly stated</section>
    </requirements_document>
    <task_breakdown>
      <dependency_graph>Task dependencies visualization (Mermaid preferred for complex graphs)</dependency_graph>
      <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
      <execute_handoff>Decisions made, references, constraints, what /execute must NOT assume</execute_handoff>
    </task_breakdown>
  </format>
  <output_rules>
    <rule>Internal investigation analysis is separate from the output document. Do not paste agent outputs into requirements.</rule>
    <rule>Use Mermaid diagrams and tables to convey structure; prefer visual over prose for architectural impacts.</rule>
    <rule>Abstract → Concrete ordering: system impact before implementation detail.</rule>
    <rule>Outstanding issues must always be written — "none" explicitly stated if truly none.</rule>
    <rule>Test requirements must be expressed as observable behavior, not internal mechanism.</rule>
    <rule>Do not include implementation detail that any competent developer would naturally choose.</rule>
  </output_rules>
</output>
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
    <behavior id="DEF-B003" priority="critical">
      <trigger>Before finalizing requirements</trigger>
      <action>Run pre-completion self-check (see completion_conditions)</action>
      <verification>All checklist items answered</verification>
    </behavior>
    <behavior id="DEF-B004" priority="critical">
      <trigger>After completing requirements definition</trigger>
      <action>Evaluate memory_auto_creation_triggers (serena-usage skill); if any trigger matched
        (architectural decisions discovered, conventions identified, novel patterns found),
        call list_memories then use edit_memory (existing) or write_memory (new topic).
        Note: write_memory is Serena memory only — this does not violate the read-only file constraint.</action>
      <verification>Memory operation recorded in output, or "persist: no triggers matched — skip"</verification>
    </behavior>
    <behavior id="DEF-B005" priority="high">
      <trigger>After completing requirements definition</trigger>
      <action>Apply memory_staleness_verification (serena-usage skill) to any memory read via read_memory during this task; bump last-verified, correct, or archive as appropriate. Skip if no memories were read.</action>
      <verification>Staleness check outcome recorded in output, or "no memories read this task"</verification>
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
    <behavior id="DEF-P003" priority="critical">
      <trigger>Always</trigger>
      <action>Copying investigation findings directly into the requirements document without synthesis</action>
      <response>Internal analysis stays internal. The output must be a synthesized, coherent document — not a dump of agent outputs.</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<completion_conditions>
  <pre_completion_checklist>
    <item>Have I verified the user's framing reflects the real problem, not just the stated solution?</item>
    <item>Is every requirement grounded in evidence from the codebase investigation?</item>
    <item>Are design decision rationales documented (why this choice, what alternatives were rejected)?</item>
    <item>Have all acceptance criteria been expressed as observable behavior?</item>
    <item>Is the blast radius (which systems/files are affected) clearly stated?</item>
    <item>Are outstanding issues documented, even if the answer is "none"?</item>
    <item>If outstanding issues are non-empty, did I run the finalize gate (define-core#core_finalize) so the user chose to resolve, defer, or stop — rather than silently ending?</item>
    <item>Does the /execute handoff contain enough context that a fresh implementer could proceed without re-asking?</item>
  </pre_completion_checklist>
  <final_self_check>
    <question>Is there a simpler scope that satisfies the user's core need?</question>
    <question>Have I challenged any assumption the user presented as fixed that might actually be flexible?</question>
    <question>Am I specifying decision points (the genuinely hard parts), not just documenting the obvious?</question>
    <question>Does any part of this document assume a capability that I haven't verified exists?</question>
  </final_self_check>
  <done_when>
    <criterion>All critical questions answered by user or flagged as outstanding</criterion>
    <criterion>Technical feasibility confirmed with codebase evidence</criterion>
    <criterion>Scope bounded and blast radius identified</criterion>
    <criterion>Acceptance criteria expressed as observable behavior</criterion>
    <criterion>Critique phase completed and findings incorporated or flagged</criterion>
    <criterion>Pre-completion checklist passed</criterion>
    <criterion>Outstanding-issues disposition resolved with the user via the finalize gate (resolve / defer / stop) when any outstanding issues remain; gate correctly skipped when "none"</criterion>
  </done_when>
</completion_conditions>
<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor ambiguity in non-critical feature detail</example>
    <example severity="medium">Unclear requirement or ambiguous scope</example>
    <example severity="high">Technically infeasible request or breaking change</example>
    <example severity="critical">Request violates security policy or data integrity</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="ask">When requirements raise technical questions</command>
  <command name="bug">When defining fix requirements for known issues</command>
  <command name="execute">Handoff point after requirements are defined</command>
  <command name="define-full">Full version with automatic feedback and regeneration cycle</command>
  <command name="simplify">Code cleanup after implementation</command>
</related_commands>
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
</agents>
<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="define-core">Shared workflow phases (prepare → analyze → investigate → clarify → verify → document)</skill>
  <skill name="requirements-definition">Core methodology for specification (question scoring, FR format)</skill>
  <skill name="investigation-patterns">Evidence gathering for feasibility assessment</skill>
  <skill name="serena-usage">Check existing patterns and memories via Serena MCP</skill>
  <skill name="fact-check">External source verification using Context7 and WebSearch</skill>
</related_skills>
<constraints>
  <must>Keep all operations read-only</must>
  <must>Delegate detailed investigation to sub-agents</must>
  <must>Use AskUserQuestion tool for structured user interactions</must>
  <must>Present questions before making assumptions</must>
  <must>Start every investigation from the big picture (L0) before the detail (L4)</must>
  <must>Run pre-completion checklist before finalizing requirements document</must>
  <avoid>Implementing or modifying code</avoid>
  <avoid>Justifying user requests over technical validity</avoid>
  <avoid>Proceeding without clear answers to critical questions</avoid>
  <avoid>Using plain text output for questions instead of AskUserQuestion tool</avoid>
  <avoid>Specifying implementation details that any competent developer would naturally choose</avoid>
  <avoid>Assuming capabilities exist without verifying in the current codebase</avoid>
</constraints>
