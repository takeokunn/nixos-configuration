---
name: Execution Workflow
description: This skill should be used when the user asks to "execute task", "implement feature", "delegate work", "run workflow", "review code", "code quality check", or needs task orchestration and code review guidance. Provides execution, delegation, and code review patterns.
version: 0.2.0
---

<purpose>
Provide structured workflow for task execution through delegation to specialized sub-agents, and comprehensive code review standards.
</purpose>

<workflow>
<phase name="analyze">
<description>Understand requirements and identify scope</description>
<step>Parse task description for key objectives</step>
<step>Identify affected files and components</step>
<step>Check Serena memories for existing patterns</step>
</phase>

<phase name="break_down">
<description>Split into manageable units</description>
<step>Identify atomic tasks</step>
<step>Estimate complexity of each task</step>
<step>Assign to appropriate sub-agents</step>
</phase>

<phase name="organize">
<description>Identify parallel vs sequential execution</description>
<step>Map task dependencies</step>
<step>Group independent tasks for parallel execution</step>
<step>Order dependent tasks sequentially</step>
</phase>

<phase name="delegate">
<description>Assign to sub-agents with detailed instructions</description>
<step>Provide specific scope and expected deliverables</step>
<step>Include target file paths</step>
<step>Specify MCP tool usage instructions</step>
<step>Reference existing implementations</step>
</phase>

<phase name="integrate">
<description>Verify and combine results</description>
<step>Review sub-agent outputs</step>
<step>Resolve conflicts between outputs</step>
<step>Ensure consistency across changes</step>
</phase>
</workflow>

<agents>
<group name="quality_assurance" execution="parallel">
<agent name="quality">Syntax, type, format verification</agent>
<agent name="security">Vulnerability detection</agent>
</group>
<group name="implementation" execution="parallel_if_independent">
<agent name="test">Test creation, coverage</agent>
<agent name="refactor">Refactoring, tech debt</agent>
<agent name="docs">Documentation updates</agent>
</group>
<group name="review" execution="sequential_after_implementation">
<agent name="review">Post-implementation review</agent>
</group>
</agents>

<delegation>
<requirement>Specific scope and expected deliverables</requirement>
<requirement>Target file paths</requirement>
<requirement>Serena MCP usage: find_symbol, get_symbols_overview, search_for_pattern</requirement>
<requirement>Context7 MCP usage for library verification</requirement>
<requirement>Reference implementations with specific paths</requirement>
<requirement>Memory check: list_memories for patterns</requirement>
</delegation>

<tools>
<preference order="1">Basic tools (Read/Edit/Write) when sufficient</preference>
<preference order="2">Serena MCP for semantic operations</preference>
<preference order="3">Context7 for library documentation</preference>
<preference order="4">Codex MCP only for code generation/modification</preference>
<prohibited tool="codex">
<task>Research/analysis - use Explore agent, Serena MCP</task>
<task>Quality verification - use quality agent</task>
<task>Security verification - use security agent</task>
<task>Test creation - use test agent</task>
<task>Documentation - use docs agent</task>
<task>Code review - use review agent</task>
</prohibited>
</tools>

<code_review>
<phase name="initial_scan">
<description>Quick pass for obvious issues</description>
<check>Syntax errors and typos</check>
<check>Missing imports or dependencies</check>
<check>Obvious logic errors</check>
<check>Code style violations</check>
</phase>

<phase name="deep_analysis">
<description>Line-by-line review of changed code</description>
<check>Algorithm correctness</check>
<check>Edge case handling</check>
<check>Error handling completeness</check>
<check>Resource management</check>
</phase>

<phase name="context_evaluation">
<description>Impact on related code</description>
<check>Breaking changes to public APIs</check>
<check>Side effects on existing functionality</check>
<check>Dependency compatibility</check>
</phase>

<phase name="standards_compliance">
<description>Compare against project and language standards</description>
<check>Naming conventions</check>
<check>Documentation requirements</check>
<check>Test coverage</check>
</phase>
</code_review>

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

<feedback_categories>
<category name="critical">Must fix before merge (security vulnerabilities, data corruption risks, breaking changes)</category>
<category name="important">Should fix before merge (logic errors, missing error handling, performance issues)</category>
<category name="suggestion">Nice to have improvements (code style, refactoring opportunities, documentation)</category>
<category name="positive">What was done well (good patterns, clever solutions, thorough testing)</category>
</feedback_categories>

<output>
<format>
## Summary
Overall assessment and recommendation

## Critical Issues
Must-fix items with file:line references

## Important Issues
Should-fix items

## Suggestions
Optional improvements

## Positive Feedback
Good practices observed

## Questions
Clarifications needed
</format>
</output>

<rules>
<rule>Execute independent tasks in parallel</rule>
<rule>quality + security: Concurrent checks</rule>
<rule>test + docs: Simultaneous creation when independent</rule>
<rule>Never parallelize tasks with data dependencies</rule>
<rule>Verify sub-agent outputs before integration</rule>
<rule>Run quality checks after changes</rule>
<rule>Ensure no regression in existing functionality</rule>
<rule>Confirm all acceptance criteria met</rule>
</rules>

<anti_patterns>
<avoid>Nitpicking on style when functionality is broken</avoid>
<avoid>Approving without thorough review</avoid>
<avoid>Focusing only on negatives</avoid>
<avoid>Vague feedback without specific suggestions</avoid>
</anti_patterns>

