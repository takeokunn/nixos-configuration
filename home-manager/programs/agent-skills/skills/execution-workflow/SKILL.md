---
name: execution-workflow
description: "Use when the agent needs to 'execute task', 'implement feature', 'delegate work', 'run workflow', or 'review code' through sub-agent orchestration. Provides structured patterns for parallel/sequential task execution, delegation context, and systematic four-phase code review."
---

Structured workflow for task execution through delegation to specialized sub-agents, and comprehensive code review standards.

## Agent Groups

- **quality_assurance**: Quality and security checks (run in parallel)
- **implementation**: Test, refactor, docs (run in parallel when independent)
- **review**: Sequential after implementation completes

## Delegation Workflow

1. Define scope and file paths for each sub-agent
2. Provide Serena/Context7 tool instructions and reference implementations
3. Check Serena memories for existing patterns
4. Execute independent tasks concurrently; dependent tasks sequentially
5. Verify sub-agent outputs before integration

### Tool Selection

- **Coding tasks**: Serena MCP > Context7 > Basic tools
- **Non-coding tasks**: Serena MCP > Context7 > Basic tools

### Example: Delegating an Implementation Task

```
Sub-agent: implementation
Scope: Refactor auth module
Files: src/auth/handler.ts, src/auth/middleware.ts
Tools: Use Serena find_symbol to locate AuthHandler, then edit
Reference: src/users/handler.ts for pattern
Memory: Check Serena memories for auth patterns
```

## Code Review Phases

The agent should apply these four phases systematically when code has been modified or newly created.

### Phase 1 - Initial Scan
- Syntax errors and typos
- Missing imports or dependencies
- Obvious logic errors, code style violations

### Phase 2 - Deep Analysis
- Algorithm correctness and edge case handling
- Error handling completeness, resource management

### Phase 3 - Context Evaluation
- Breaking changes to public APIs
- Side effects on existing functionality, dependency compatibility

### Phase 4 - Standards Compliance
- Naming conventions, documentation requirements, test coverage

## Quality Criteria

| Dimension | Key Checks |
|-----------|-----------|
| Correctness | Logic matches requirements, edge cases handled, error conditions covered |
| Security | Input validation, auth/authz, data sanitization, secrets handling |
| Performance | Algorithm efficiency, resource usage, memory leaks, N+1 queries |
| Maintainability | Clear naming, appropriate comments, SRP, DRY |
| Testability | Adequate coverage, meaningful tests, edge cases tested |

## Feedback Categories

- **Critical** (must fix): Security vulnerabilities, data corruption risks, breaking changes
- **Important** (should fix): Logic errors, missing error handling, performance issues
- **Suggestion** (nice to have): Code style, refactoring opportunities, documentation
- **Positive**: Good patterns, clever solutions, thorough testing

## Review Output Format

```
Summary: Overall assessment and recommendation
Critical Issues: Must-fix items with file:line references
Important Issues: Should-fix items
Suggestions: Optional improvements
Positive Feedback: Good practices observed
Questions: Clarifications needed
```

## Anti-Patterns

- **Nitpicking style**: Address critical/important issues first, style suggestions last
- **Rubber stamping**: Always review all four phases systematically
- **Only negatives**: Balance feedback with positive observations
- **Vague feedback**: Provide file:line references and concrete suggestions
- **Sequential when parallel**: Execute independent tasks concurrently
- **Parallel when dependent**: Analyze dependencies; run dependent tasks in order

## Error Escalation

| Severity | Example | Action |
|----------|---------|--------|
| Low | Sub-agent returns partial results | Note in report, proceed |
| Medium | Sub-agent task fails | Document issue, ask user for clarification |
| High | Critical task cannot be completed | STOP, present options to user |
| Critical | Sub-agent introduces breaking change | BLOCK operation, require user acknowledgment |

## Critical Rules

- Execute independent tasks in parallel; never parallelize tasks with data dependencies
- Verify sub-agent outputs before integration
- Run quality checks after changes
- Ensure no regression in existing functionality

## Related Skills

- `serena-usage`: Memory checks and symbol operations during delegation
- `investigation-patterns`: When code review reveals unclear implementation details
- `testing-patterns`: Verify test coverage and quality during review
