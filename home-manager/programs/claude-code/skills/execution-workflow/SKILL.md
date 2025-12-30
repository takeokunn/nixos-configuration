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

<tools>
<tool name="agent_groups">
<description>Specialized sub-agents organized by execution model</description>
<usage>
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
</usage>
</tool>

<tool name="delegation">
<description>Essential information to provide when delegating to sub-agents</description>
<usage>
- Specific scope and expected deliverables
- Target file paths
- Serena MCP usage: find_symbol, get_symbols_overview, search_for_pattern
- Context7 MCP usage for library verification
- Reference implementations with specific paths
- Memory check: list_memories for patterns
</usage>
</tool>

<tool name="tool_selection">
<description>Tool selection hierarchy for task execution</description>
<usage>
For coding tasks (generation/modification/review):
Priority 1: Codex MCP (sandbox: workspace-write, approval-policy: on-failure)
Priority 2: Serena MCP for symbol operations and memory
Priority 3: Context7 for library documentation
Priority 4: Basic tools (Read/Edit/Write) as fallback

For non-coding tasks (research/analysis):
Priority 1: Serena MCP for symbol search and memory
Priority 2: Context7 for library documentation
Priority 3: Basic tools (Read/Edit/Write)

Codex MCP configuration:
- sandbox: workspace-write (allows code generation/modification)
- approval-policy: on-failure (auto-approve commands when execution fails)

Prohibited Codex usage:
- Research/analysis - use Explore agent, Serena MCP
- Documentation generation - use docs agent

Allowed Codex usage:
- Code generation (new files/functions)
- Code modification (editing/refactoring)
- Code review and quality analysis
- Test code generation
- Performance optimization suggestions
</usage>
</tool>
  </tools>

<patterns>
<pattern name="code_review_phases">
<description>Systematic code review process</description>
<example>
Phase 1 - Initial Scan:
- Syntax errors and typos
- Missing imports or dependencies
- Obvious logic errors
- Code style violations

Phase 2 - Deep Analysis:

- Algorithm correctness
- Edge case handling
- Error handling completeness
- Resource management

Phase 3 - Context Evaluation:

- Breaking changes to public APIs
- Side effects on existing functionality
- Dependency compatibility

Phase 4 - Standards Compliance:

- Naming conventions
- Documentation requirements
- Test coverage
  </example>
  </pattern>

<pattern name="quality_criteria">
<description>Evaluation criteria for code quality</description>
<example>
Correctness:
- Logic matches requirements
- Edge cases handled
- Error conditions covered

Security:

- Input validation
- Authentication/authorization
- Data sanitization
- Secrets handling

Performance:

- Algorithm efficiency
- Resource usage
- Memory leaks
- N+1 queries

Maintainability:

- Clear naming
- Appropriate comments
- Single responsibility
- DRY principle

Testability:

- Test coverage adequate
- Tests meaningful
- Edge cases tested
  </example>
  </pattern>

<pattern name="feedback_categories">
<description>Categorization of review feedback by priority</description>
<example>
Critical: Must fix before merge
- Security vulnerabilities
- Data corruption risks
- Breaking changes

Important: Should fix before merge

- Logic errors
- Missing error handling
- Performance issues

Suggestion: Nice to have improvements

- Code style
- Refactoring opportunities
- Documentation

Positive: What was done well

- Good patterns
- Clever solutions
- Thorough testing
  </example>
  </pattern>

<pattern name="review_output_format">
<description>Standard format for code review results</description>
<example>
<summary>Overall assessment and recommendation</summary>
<critical_issues>Must-fix items with file:line references</critical_issues>
<important_issues>Should-fix items</important_issues>
<suggestions>Optional improvements</suggestions>
<positive_feedback>Good practices observed</positive_feedback>
<questions>Clarifications needed</questions>
</example>
</pattern>
</patterns>

<rules priority="critical">
<rule>Execute independent tasks in parallel</rule>
<rule>Never parallelize tasks with data dependencies</rule>
<rule>Verify sub-agent outputs before integration</rule>
<rule>Run quality checks after changes</rule>
</rules>

<rules priority="standard">
<rule>quality + security: Concurrent checks</rule>
<rule>test + docs: Simultaneous creation when independent</rule>
<rule>Ensure no regression in existing functionality</rule>
<rule>Confirm all acceptance criteria met</rule>
</rules>

<best_practices>
<practice priority="critical">Analyze task dependencies before execution to determine parallel vs sequential execution model</practice>
<practice priority="critical">Provide comprehensive context to sub-agents including file paths, tool usage, and reference implementations</practice>
<practice priority="critical">Systematically review all phases: initial scan, deep analysis, context evaluation, standards compliance</practice>
<practice priority="high">Balance critical feedback with positive observations of good practices</practice>
<practice priority="high">Provide file:line references and concrete improvement suggestions</practice>
<practice priority="medium">Check Serena memories for existing patterns before delegating implementation tasks</practice>
</best_practices>

<anti_patterns>
<avoid name="nitpicking_style">
<description>Focusing on code style issues when functionality is broken</description>
<instead>Address critical and important issues first, style suggestions last</instead>
</avoid>
<avoid name="rubber_stamping">
<description>Approving changes without thorough review</description>
<instead>Systematically review all phases: scan, deep analysis, context, standards</instead>
</avoid>
<avoid name="only_negatives">
<description>Providing only critical feedback without acknowledging good work</description>
<instead>Balance feedback with positive observations of good practices</instead>
</avoid>
<avoid name="vague_feedback">
<description>Giving feedback without specific, actionable suggestions</description>
<instead>Provide file:line references and concrete improvement suggestions</instead>
</avoid>
<avoid name="sequential_when_parallel">
<description>Executing independent tasks sequentially</description>
<instead>Identify and execute independent tasks in parallel for efficiency</instead>
</avoid>
<avoid name="parallel_when_dependent">
<description>Attempting to parallelize tasks with data dependencies</description>
<instead>Analyze dependencies and execute dependent tasks sequentially</instead>
</avoid>
</anti_patterns>
