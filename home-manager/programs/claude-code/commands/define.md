---
argument-hint: [message]
description: Requirements definition command
---

<purpose>
Conduct detailed requirements definition before implementation, clarifying technical constraints, design policies, and specifications.
</purpose>

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
</rules>

<workflow>
<phase name="analysis">
<step>What is the user requesting?</step>
<step>What technical constraints exist?</step>
<step>What design decisions need user input?</step>
<step>Is this technically feasible?</step>
</phase>
<phase name="investigation">
<step>Delegate to explore agent: find relevant files and existing patterns</step>
<step>Delegate to design agent: evaluate architecture consistency and dependencies</step>
<step>Delegate to database agent: analyze database design (if applicable)</step>
<step>Delegate to general-purpose agent: analyze requirements and estimate effort</step>
</phase>
<phase name="user_interview">
<step>Score questions by: design branching, irreversibility, investigation impossibility, effort impact (1-5 each)</step>
<step>Classify: spec confirmation, design choice, constraint, scope, priority</step>
<step>Use AskUserQuestion tool for all user interactions (2-4 structured options per question)</step>
<step>For follow-up clarifications, continue using AskUserQuestion tool rather than plain text</step>
<step>Present high-score questions first; do not proceed without clear answers</step>
</phase>
<phase name="re_investigation">
<step>Verify constraints from answers using agent findings</step>
<step>Check implementations related to chosen approach</step>
</phase>
<phase name="documentation">
<step>Create comprehensive requirements document</step>
<step>Break down tasks for /execute handoff</step>
</phase>
</workflow>

<agents>
<agent name="design" subagent_type="design" readonly="true">Architecture consistency, dependency analysis, API design</agent>
<agent name="database" subagent_type="database" readonly="true">Database design and optimization</agent>
<agent name="general-purpose" subagent_type="general-purpose" readonly="true">Requirements analysis, estimation, dependency analysis</agent>
<agent name="explore" subagent_type="explore" readonly="true">Finding relevant files and existing patterns</agent>
</agents>

<parallel_execution>
<group name="investigation" execution="parallel">
<agent>explore</agent>
<agent>design</agent>
<agent>database</agent>
</group>
<group name="analysis" execution="parallel">
<agent>general-purpose</agent>
</group>
</parallel_execution>

<delegation>
<requirement>Scope overview</requirement>
<requirement>Target file paths</requirement>
<requirement>Explicit edit prohibition</requirement>
<requirement>Sub-agents must use AskUserQuestion tool for any user interactions</requirement>
</delegation>

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
</format>
</output>

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
