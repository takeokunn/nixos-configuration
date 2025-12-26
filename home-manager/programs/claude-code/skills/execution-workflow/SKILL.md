---
name: Execution Workflow
description: This skill should be used when the user asks to "execute task", "implement feature", "delegate work", "run workflow", "review code", "code quality check", or needs task orchestration and code review guidance. Provides execution, delegation, and code review patterns.
version: 0.2.0
---

<purpose>
Provide structured workflow for task execution through delegation to specialized sub-agents, and comprehensive code review standards.
</purpose>

<execution_phases>
<phase id="1" name="analyze">
Understand requirements and identify scope.
<action>Parse task description for key objectives</action>
<action>Identify affected files and components</action>
<action>Check Serena memories for existing patterns</action>
</phase>

<phase id="2" name="break_down">
Split into manageable units.
<action>Identify atomic tasks</action>
<action>Estimate complexity of each task</action>
<action>Assign to appropriate sub-agents</action>
</phase>

<phase id="3" name="organize">
Identify parallel vs sequential execution.
<action>Map task dependencies</action>
<action>Group independent tasks for parallel execution</action>
<action>Order dependent tasks sequentially</action>
</phase>

<phase id="4" name="delegate">
Assign to sub-agents with detailed instructions.
<action>Provide specific scope and expected deliverables</action>
<action>Include target file paths</action>
<action>Specify MCP tool usage instructions</action>
<action>Reference existing implementations</action>
</phase>

<phase id="5" name="integrate">
Verify and combine results.
<action>Review sub-agent outputs</action>
<action>Resolve conflicts between outputs</action>
<action>Ensure consistency across changes</action>
</phase>
</execution_phases>

<agent_delegation>
<phase name="quality_assurance">
<agent name="quality">Syntax, type, format verification</agent>
<agent name="security">Vulnerability detection</agent>
<execution>Parallel - no dependencies</execution>
</phase>

<phase name="implementation">
<agent name="test">Test creation, coverage</agent>
<agent name="refactor">Refactoring, tech debt</agent>
<agent name="docs">Documentation updates</agent>
<execution>Can run in parallel if independent</execution>
</phase>

<phase name="review">
<agent name="review">Post-implementation review</agent>
<execution>Sequential - after implementation</execution>
</phase>
</agent_delegation>

<delegation_instructions>
Each delegation must include:
<item>Specific scope and expected deliverables</item>
<item>Target file paths</item>
<item>Serena MCP usage: `find_symbol`, `get_symbols_overview`, `search_for_pattern`</item>
<item>Context7 MCP usage for library verification</item>
<item>Reference implementations with specific paths</item>
<item>Memory check: `list_memories` for patterns</item>
</delegation_instructions>

<parallel_execution>
<rule>Execute independent tasks in parallel</rule>
<rule>quality + security: Concurrent checks</rule>
<rule>test + docs: Simultaneous creation when independent</rule>
<rule>Never parallelize tasks with data dependencies</rule>
</parallel_execution>

<tool_usage>
<preference order="1">Basic tools (Read/Edit/Write) when sufficient</preference>
<preference order="2">Serena MCP for semantic operations</preference>
<preference order="3">Context7 for library documentation</preference>
<preference order="4">Codex MCP only for code generation/modification</preference>

<codex_prohibited>
<task>Research/analysis - use Explore agent, Serena MCP</task>
<task>Quality verification - use quality agent</task>
<task>Security verification - use security agent</task>
<task>Test creation - use test agent</task>
<task>Documentation - use docs agent</task>
<task>Code review - use review agent</task>
</codex_prohibited>
</tool_usage>

<code_review>
<review_process>
<phase id="1" name="initial_scan">
Quick pass for obvious issues.
<check>Syntax errors and typos</check>
<check>Missing imports or dependencies</check>
<check>Obvious logic errors</check>
<check>Code style violations</check>
</phase>

<phase id="2" name="deep_analysis">
Line-by-line review of changed code.
<check>Algorithm correctness</check>
<check>Edge case handling</check>
<check>Error handling completeness</check>
<check>Resource management</check>
</phase>

<phase id="3" name="context_evaluation">
Impact on related code.
<check>Breaking changes to public APIs</check>
<check>Side effects on existing functionality</check>
<check>Dependency compatibility</check>
</phase>

<phase id="4" name="standards_compliance">
Compare against project and language standards.
<check>Naming conventions</check>
<check>Documentation requirements</check>
<check>Test coverage</check>
</phase>
</review_process>

<quality_criteria>
<criterion name="correctness">
<description>Code does what it's supposed to do</description>
<checks>Logic matches requirements, edge cases handled, error conditions covered</checks>
</criterion>

<criterion name="security">
<description>No security vulnerabilities introduced</description>
<checks>Input validation, authentication/authorization, data sanitization, secrets handling</checks>
</criterion>

<criterion name="performance">
<description>No significant performance degradation</description>
<checks>Algorithm efficiency, resource usage, memory leaks, N+1 queries</checks>
</criterion>

<criterion name="maintainability">
<description>Code is easy to understand and modify</description>
<checks>Clear naming, appropriate comments, single responsibility, DRY principle</checks>
</criterion>

<criterion name="testability">
<description>Code can be effectively tested</description>
<checks>Test coverage adequate, tests meaningful, edge cases tested</checks>
</criterion>
</quality_criteria>

<feedback_format>
<category name="critical">
Must fix before merge.
<examples>Security vulnerabilities, data corruption risks, breaking changes</examples>
</category>

<category name="important">
Should fix before merge.
<examples>Logic errors, missing error handling, performance issues</examples>
</category>

<category name="suggestion">
Nice to have improvements.
<examples>Code style, refactoring opportunities, documentation</examples>
</category>

<category name="positive">
What was done well.
<examples>Good patterns, clever solutions, thorough testing</examples>
</category>
</feedback_format>

<review_output>
<section name="summary">Overall assessment and recommendation</section>
<section name="critical_issues">Must-fix items with file:line references</section>
<section name="important_issues">Should-fix items</section>
<section name="suggestions">Optional improvements</section>
<section name="positive_feedback">Good practices observed</section>
<section name="questions">Clarifications needed</section>
</review_output>

<review_anti_patterns>
<avoid>Nitpicking on style when functionality is broken</avoid>
<avoid>Approving without thorough review</avoid>
<avoid>Focusing only on negatives</avoid>
<avoid>Vague feedback without specific suggestions</avoid>
</review_anti_patterns>
</code_review>

<verification>
<step>Verify sub-agent outputs before integration</step>
<step>Run quality checks after changes</step>
<step>Ensure no regression in existing functionality</step>
<step>Confirm all acceptance criteria met</step>
</verification>
