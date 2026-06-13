---
name: define
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
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<workflow inherits="define-core#workflow" />
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
<output>
  <format>
    <requirements_document>
      <section name="Summary" required="always">One-sentence request, background (Why), expected outcomes</section>
      <section name="Current State" required="always">Existing system, tech stack, affected components</section>
      <section name="Functional Requirements" required="always">FR-001 format (mandatory/optional); behavior-level, not implementation-level</section>
      <section name="Non-Functional Requirements" required="when-applicable">Performance, security, maintainability</section>
      <section name="Technical Specifications" required="always">Design policies, impact scope, key decisions and rationale</section>
      <section name="Metrics" required="always">
        <metric name="feasibility">0-100 with evidence</metric>
        <metric name="objectivity">0-100</metric>
      </section>
      <section name="Constraints" required="always">Technical, operational</section>
      <section name="Test Requirements" required="always">Unit, integration, acceptance criteria as observable behavior</section>
      <section name="Outstanding Issues" required="always">Unresolved questions; "none" must be explicitly stated</section>
    </requirements_document>
    <task_breakdown>
      <dependency_graph>Task dependencies visualization</dependency_graph>
      <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
      <execute_handoff>Decisions made, references, constraints, what /execute must NOT assume</execute_handoff>
    </task_breakdown>
  </format>
</output>
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
