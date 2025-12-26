---
name: Requirements Definition
description: This skill should be used when the user asks to "define requirements", "create specification", "clarify requirements", "write requirements document", or mentions requirement analysis. Provides comprehensive requirements definition methodology.
version: 0.1.0
---

<purpose>
Provide structured methodology for requirements definition, ensuring comprehensive specification before implementation.
</purpose>

<methodology>
<investigation_phases>
<phase id="1" name="structure">
Use Glob/LS for directory structure, Serena `get_symbols_overview` for symbol analysis.
</phase>
<phase id="2" name="keywords">
Use Grep/Serena `find_symbol` for keyword search, `find_referencing_symbols` for dependency mapping.
</phase>
<phase id="3" name="details">
Use Read for specific content, Context7 for latest API documentation and best practices.
</phase>
</investigation_phases>

<question_scoring>
Score each question by these criteria (1-5 each):
<criterion name="design_branching">How much does the answer affect design direction?</criterion>
<criterion name="irreversibility">How difficult to change after implementation?</criterion>
<criterion name="investigation_impossibility">Cannot be determined through code investigation alone?</criterion>
<criterion name="effort_impact">How much does it affect implementation effort?</criterion>

Present high-score questions first. Do not proceed without clear answers to critical questions (score >= 15).
</question_scoring>

<question_classification>
<type name="spec_confirmation">Confirming existing behavior or constraints</type>
<type name="design_choice">Choosing between valid alternatives</type>
<type name="constraint">Technical or business limitations</type>
<type name="scope">Boundaries of implementation</type>
<type name="priority">Order and importance of features</type>
</question_classification>
</methodology>

<document_format>
<section name="summary">
<item>One-sentence request description</item>
<item>Background and context</item>
<item>Expected outcomes</item>
</section>

<section name="current_state">
<item>Existing system description</item>
<item>Technology stack</item>
<item>Relevant patterns and conventions</item>
</section>

<section name="functional_requirements">
Format: FR-XXX
<item>Unique identifier (FR-001, FR-002, ...)</item>
<item>Priority: mandatory or optional</item>
<item>Clear acceptance criteria</item>
</section>

<section name="non_functional_requirements">
<item>Performance: response time, throughput</item>
<item>Security: authentication, authorization, data protection</item>
<item>Maintainability: code quality, documentation</item>
</section>

<section name="technical_specifications">
<item>Design policies and patterns</item>
<item>Impact scope analysis</item>
<item>Key design decisions with rationale</item>
</section>

<section name="metrics">
<item>Feasibility (0-100): Technical achievability</item>
<item>Objectivity (0-100): Evidence-based specification</item>
</section>

<section name="constraints">
<item>Technical constraints: platform, language, framework</item>
<item>Operational constraints: deployment, maintenance</item>
</section>

<section name="test_requirements">
<item>Unit test coverage expectations</item>
<item>Integration test scenarios</item>
<item>Acceptance criteria verification</item>
</section>
</document_format>

<task_breakdown>
<component name="dependency_graph">
Identify task dependencies and execution order.
</component>
<component name="phased_tasks">
<item>Files to modify or create</item>
<item>Overview of changes</item>
<item>Dependencies between tasks</item>
</component>
<component name="execute_handoff">
<item>Key decisions made</item>
<item>Reference implementations</item>
<item>Constraints to observe</item>
</component>
</task_breakdown>
