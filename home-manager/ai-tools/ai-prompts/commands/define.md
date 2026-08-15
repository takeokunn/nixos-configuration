---
argument-hint: [message]
description: Requirements definition command
---

<purpose>
Conduct detailed requirements definition before implementation, clarifying technical constraints, design policies, and specifications.
</purpose>
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
  <rule>Never modify a file and never write code. This command produces the specification the user
    approves before work starts; implementing during it removes the approval step it exists to create.</rule>
  <rule>Say plainly when a request is technically impossible or rests on a capability that is not
    there. A specification that assumes it becomes wasted implementation, discovered late.</rule>
  <rule>Prefer technical validity to the user's stated preference, and say which one a decision came
    from when they conflict.</rule>
  <rule>Challenge the framing before accepting it: the stated problem is often the user's first
    solution, and specifying it forecloses the better one.</rule>
</rules>
<rules priority="standard">
  <rule>Move from signal to hypothesis to verification before concluding — never from signal straight
    to a question, because a question that investigation could have answered spends the user's turn.</rule>
  <rule>Keep asking until the requirements are unambiguous; there is no question budget here, and an
    ambiguity resolved now costs a sentence rather than a rewrite.</rule>
  <rule>Mark one option (Recommended) whenever AskUserQuestion presents choices, so the user is
    reviewing a proposal rather than doing the analysis themselves.</rule>
  <rule>Reason in Why → How → What order; the output may be structured differently, but reasoning that
    starts at What specifies the solution already in hand.</rule>
  <rule>Specify only what is load-bearing. Detail spent on what any competent implementer would choose
    anyway crowds out the decisions that actually need deciding.</rule>
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
<workflow>
  <phase name="load">
    <objective>Put the workflow this command runs into context, since it lives in a skill</objective>
    <step order="1">
      <action>Load define-core and requirements-definition with the Skill tool, before anything else.
        define-core holds the phase sequence this command executes — prepare, analyze, investigate,
        clarify, verify, document, finalize — and requirements-definition holds the methodology inside
        those phases: question scoring, FR format, acceptance-criteria shape. Neither is in context
        until the Skill tool loads it, so skipping this step leaves the command with no workflow at
        all. Load fact-check as well when the requirements depend on an external library or service
        whose behavior has to be confirmed rather than recalled.</action>
      <tool>Skill</tool>
      <output>The skills loaded, named; and the phase list define-core returned, so the rest of this
        command can be checked against it</output>
    </step>
  </phase>
  <phase name="run_core_workflow">
    <objective>Execute define-core's phases under this command's constraints</objective>
    <step order="1">
      <action>Run the phases define-core defines, in its order. At each phase apply what this file adds
        on top: the Why → How → What ordering and L0-before-L4 depth from thinking_framework, the five
        known biases in bias_correction, the signal table in request_signals, minimum_viable_scope when
        bounding, and the output contract below. Where define-core and this file both speak to the same
        decision, this file governs, because it is the narrower context.</action>
      <output>The phases run, and any phase skipped with its reason</output>
    </step>
  </phase>
</workflow>
<reflection_checkpoint id="group_consistency">
  <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
  <check>Name any workflow phase that was skipped, and why.</check>
  <check>State that no file was created or modified — this command is read-only — or name the file that was.</check>
  <on_unmet>Resolve the structural gap before delivering the requirements document.</on_unmet>
</reflection_checkpoint>
<common_investigation_workflows>
  <playbook id="A" name="New Feature Definition">
    <step order="1">
      <action>Identify which L0 systems are affected (new vs. extending existing)</action>
      <tool>Task tool (explore, design)</tool>
    </step>
    <step order="2">
      <action>Find similar existing implementations as reference patterns</action>
      <tool>Grep, Glob, Serena find_symbol</tool>
    </step>
    <step order="3">
      <action>Map data model changes (L1) and API changes (L2)</action>
      <tool>Task tool (database), Serena find_referencing_symbols</tool>
    </step>
    <step order="4">
      <action>Identify acceptance criteria from the user's goal, not their proposed solution</action>
      <tool>AskUserQuestion when the goal is not derivable from the request</tool>
    </step>
  </playbook>
  <playbook id="B" name="Refactor / Architecture Change">
    <step order="1">
      <action>Map current boundaries, dependencies, and change reasons (change-axis analysis)</action>
      <tool>Task tool (design), Serena get_symbols_overview</tool>
    </step>
    <step order="2">
      <action>Identify what stays the same (stable) vs. what changes (variable)</action>
      <tool>Read, Grep</tool>
    </step>
    <step order="3">
      <action>Detect invariants that must not break across the refactor</action>
      <tool>Read the existing tests; Grep for assertions on the affected behavior</tool>
    </step>
    <step order="4">
      <action>Estimate blast radius: how many modules are affected by each design choice</action>
      <tool>Serena find_referencing_symbols, Grep</tool>
    </step>
  </playbook>
  <playbook id="C" name="Bug Fix Specification">
    <step order="1">
      <action>Reproduce and confirm the failure mode with evidence from code/logs</action>
      <tool>Bash (read-only reproduction), Read</tool>
    </step>
    <step order="2">
      <action>Distinguish root cause from symptoms</action>
      <tool>Task tool (quality-assurance)</tool>
    </step>
    <step order="3">
      <action>Identify all places where the same root cause could recur</action>
      <tool>Grep, Serena find_referencing_symbols</tool>
    </step>
    <step order="4">
      <action>Specify acceptance criteria as observable behavior, not internal mechanism</action>
    </step>
  </playbook>
  <playbook id="D" name="Integration / External System">
    <step order="1">
      <action>Verify external system capabilities via Context7 or documentation (don't assume)</action>
      <tool>Context7 MCP, WebSearch</tool>
    </step>
    <step order="2">
      <action>Map authentication, rate limits, and error contracts</action>
      <tool>Context7 MCP, Read (existing client code)</tool>
    </step>
    <step order="3">
      <action>Identify data translation boundaries (what transforms, what passes through)</action>
      <tool>Read, Serena find_symbol</tool>
    </step>
    <step order="4">
      <action>Define fallback behavior for external failures</action>
      <tool>AskUserQuestion when the fallback is a product decision</tool>
    </step>
  </playbook>
</common_investigation_workflows>
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
  <resolution>Apply in precedence order. The first factor whose `unmet` condition holds decides what
    happens next; later factors are not consulted.</resolution>
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
      <section name="Constraints" required="always">Technical, operational</section>
      <section name="Test Requirements" required="always">Unit, integration, acceptance criteria as observable behavior</section>
      <section name="Verification Performed" required="always">The exact command(s) run during investigation and their exit status, or "none run". A feasibility claim with no command and no file:line behind it is inferred, not verified. State feasibility as the observable condition that supports it — which capability was located where, which one was not found — never as a score</section>
      <section name="Outstanding Issues" required="always">Unresolved questions and anything asked for that this document does not specify, with the reason; "none" must be explicitly stated. This section is also where a disagreement with the user goes: when the investigation reaches a different severity or priority than the user assigned, record both assessments and what each rests on, and hand the decision back. Silently deferring buries the risk; silently escalating overrides a call that was the user's to make</section>
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
    <behavior id="DEF-B002" priority="high">
      <trigger>For design decisions</trigger>
      <action>Use AskUserQuestion tool with structured options</action>
      <verification>User responses recorded</verification>
    </behavior>
    <behavior id="DEF-B003" priority="high">
      <trigger>Before finalizing requirements</trigger>
      <action>Run pre-completion self-check (see completion_conditions)</action>
      <verification>All checklist items answered</verification>
    </behavior>
    <behavior id="DEF-B004" priority="standard">
      <trigger>After completing requirements definition</trigger>
      <action>Evaluate memory_auto_creation_triggers (serena-usage skill); if any trigger matched
        (architectural decisions discovered, conventions identified, novel patterns found),
        call list_memories then use edit_memory (existing) or write_memory (new topic).
        Note: write_memory is Serena memory only — this does not violate the read-only file constraint.</action>
      <verification>Memory operation recorded in output, or "persist: no triggers matched — skip"</verification>
    </behavior>
    <behavior id="DEF-B005" priority="standard">
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
    <behavior id="DEF-P004" priority="critical">
      <trigger>Always</trigger>
      <action>Scoring the document — feasibility, objectivity, confidence, completeness — on a numeric
        scale</action>
      <response>Block. A score has no derivation, so it cannot be checked or disputed, and it reads as
        a measurement. State the observable condition instead: which capability was found at which
        file:line, which one was not found and where it was searched for.</response>
    </behavior>
    <behavior id="DEF-P003" priority="high">
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
<error_escalation>
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
</related_commands>
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
      effort depends on who does the work and what interrupts them, neither of which is observable
      from here, so an hour figure can only be borrowed from a training-data average and will be
      stated confidently beside the admission that the complexity is unknown</done_when>
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
