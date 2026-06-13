---
name: define-full
description: Full requirements definition with feedback loop
---

<purpose>
Extended requirements definition that includes a structured feedback loop: define → feedback → regenerate. Produces requirements documents with higher confidence than /define by incorporating a parallel review before finalizing.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="patterns">parallelization-patterns</skill>
  <skill use="workflow">requirements-definition</skill>
  <skill use="workflow">define-core</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<rules priority="critical">
  <rule>Never modify, create, or delete files</rule>
  <rule>Never implement code; requirements definition only</rule>
  <rule>Clearly identify technically impossible requests</rule>
  <rule>Prioritize technical validity over user preferences</rule>
  <rule>Technical evidence over speculation</rule>
  <rule>Maximum one feedback-and-regeneration iteration; if still insufficient, present blockers and ask user</rule>
  <rule>All feedback agents must run in parallel; do not serialize the feedback phase</rule>
</rules>
<rules priority="standard">
  <rule>Use requirements-definition skill for methodology</rule>
  <rule>Use define-core skill for base workflow</rule>
  <rule>Always include a (Recommended) option when presenting choices via AskUserQuestion</rule>
  <rule>Think in Why → How → What order (Golden Circle)</rule>
  <rule>Specify only what is necessary; do not over-specify obvious implementation details</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Iterating on requirements through multiple sequential review rounds — AI can parallelize all review dimensions simultaneously in one pass</practice>
    <practice>Treating feedback as an optional quality step — the feedback loop is mandatory and must complete before finalizing the document</practice>
    <practice>Running feedback agents one at a time — all feedback agents must launch simultaneously with no barrier between them</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Use a single parallel pass for all feedback dimensions (completeness, feasibility, consistency, testability, scope) — sequential review multiplies latency without improving quality</principle>
    <principle>When feedback agents disagree, synthesize the conflict yourself rather than delegating the resolution — you own the final document</principle>
    <principle>Regenerate only the sections that received feedback; do not rewrite sections that passed review</principle>
  </applicable_ai_principles>
</ai_principles>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<workflow>
  <phase name="define">
    <objective>Produce initial requirements document using define-core workflow</objective>
    <step order="1">
      <action>Execute define-core#workflow to produce initial requirements document</action>
      <tool>define-core skill (all phases)</tool>
      <output>Initial requirements document with all required sections</output>
    </step>
  </phase>
  <phase name="feedback">
    <objective>Run all review agents in parallel to evaluate the initial requirements document</objective>
    <step order="1">
      <action>Launch all feedback agents simultaneously (single parallel group, no barrier)</action>
      <tool>Parallel task delegation</tool>
      <output>All feedback agent reports completed</output>
    </step>
  </phase>
  <phase name="regenerate">
    <objective>Revise requirements document based on feedback synthesis</objective>
    <step order="1">
      <action>Synthesize all feedback agent reports; identify sections requiring revision</action>
      <tool>Cross-agent synthesis</tool>
      <output>Prioritized list of revisions needed</output>
    </step>
    <step order="2">
      <action>Revise only the sections that received substantive feedback; leave passing sections unchanged</action>
      <tool>Requirements document editing</tool>
      <output>Revised requirements document</output>
    </step>
    <step order="3">
      <action>If critical issues remain after revision: present blockers and ask user for direction</action>
      <tool>AskUserQuestion (conditional)</tool>
      <output>Resolved blockers or user decision recorded</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="completeness-reviewer" subagent_type="quality-assurance" readonly="true">
    <role>Verify that all required requirement sections are present and sufficiently detailed</role>
    <receives>requirements_document, section_checklist[]</receives>
    <produces>missing_sections[], thin_sections[]{section, gap_description}, completeness_score: 0-100</produces>
    <done_when>All required sections evaluated; completeness score justified with evidence</done_when>
  </agent>
  <agent name="feasibility-reviewer" subagent_type="design" readonly="true">
    <role>Challenge technical feasibility claims; identify unrealistic or unverified assumptions</role>
    <receives>requirements_document, technical_constraints[], architecture_context</receives>
    <produces>feasibility_concerns[]{requirement_id, concern, severity: critical|high|medium}, overall_feasibility: 0-100</produces>
    <done_when>All technical claims assessed; critical feasibility blockers explicitly flagged</done_when>
  </agent>
  <agent name="consistency-reviewer" subagent_type="validator" readonly="true">
    <role>Detect contradictions, ambiguities, and conflicting requirements within the document</role>
    <receives>requirements_document</receives>
    <produces>contradictions[]{req_a, req_b, conflict_description}, ambiguities[], consistency_score: 0-100</produces>
    <done_when>All requirement pairs checked for contradictions; no unresolved conflicts in passing output</done_when>
  </agent>
  <agent name="testability-reviewer" subagent_type="test" readonly="true">
    <role>Verify that all requirements are testable and acceptance criteria are observable</role>
    <receives>requirements_document, acceptance_criteria[]</receives>
    <produces>untestable_requirements[]{req_id, reason}, vague_criteria[]{req_id, suggestion}, testability_score: 0-100</produces>
    <done_when>All requirements evaluated for testability; vague criteria identified with specific improvement suggestions</done_when>
  </agent>
  <agent name="scope-reviewer" subagent_type="general-purpose" readonly="true">
    <role>Identify scope creep, missing scope, and requirements that exceed the minimum viable need</role>
    <receives>requirements_document, user_stated_goal</receives>
    <produces>scope_creep[]{requirement, reason}, missing_scope[]{gap, importance: high|medium|low}, scope_alignment_score: 0-100</produces>
    <done_when>All requirements validated against stated goal; scope creep and gaps explicitly listed</done_when>
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
    <feedback_summary>
      <review>Summary of feedback from each agent with scores</review>
      <revisions>Sections revised and rationale for changes</revisions>
    </feedback_summary>
    <task_breakdown>
      <dependency_graph>Task dependencies visualization</dependency_graph>
      <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
      <execute_handoff>Decisions made, references, constraints, what /execute must NOT assume</execute_handoff>
    </task_breakdown>
  </format>
</output>
<constraints>
  <must>Keep all operations read-only</must>
  <must>Run all feedback agents in parallel in a single message</must>
  <must>Maximum one feedback-and-regeneration cycle</must>
  <must>Use AskUserQuestion tool for structured user interactions</must>
  <avoid>Implementing or modifying code</avoid>
  <avoid>Sequential feedback review (each agent waiting for the previous)</avoid>
  <avoid>Rewriting sections that received no substantive feedback</avoid>
  <avoid>More than one feedback-regenerate iteration without user input</avoid>
</constraints>
