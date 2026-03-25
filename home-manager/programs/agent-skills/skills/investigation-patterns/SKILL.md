---
name: investigation-patterns
description: "Use when the agent needs to 'investigate code', 'analyze implementation', 'find patterns', 'understand codebase', 'debug issue', 'find bug', or 'troubleshoot' a problem. Provides systematic evidence-based investigation and debugging methodology with confidence scoring."
---

Systematic patterns for codebase investigation and debugging with evidence-based analysis and confidence assessment.

## Investigation Workflow

1. **Classify scope**: Architecture, implementation, debugging, or design question
2. **Identify sources**: Code (Serena symbols), docs (inline/README), history (git log), external (Context7)
3. **Collect evidence**: Use symbol tools, pattern search, and file analysis
4. **Synthesize findings**: Combine evidence, rate confidence (0-100), report coverage (0-100)
5. **Report**: Provide file:line references, conclusions, and information gaps

## Debugging Workflow

1. **Reproduce**: Gather exact steps, identify environment conditions, determine consistency
2. **Isolate**: Narrow scope with `git bisect`, remove unrelated components, create minimal repro
3. **Investigate**: Examine error messages/stack traces, check logs, trace data flow with Serena
4. **Hypothesize**: List possible causes, rank by likelihood, design tests for each
5. **Fix**: Make minimal targeted change, verify resolution, check regressions, add prevention test

### Example: Debugging a Resource Leak

```
Reproduce: Server memory grows over 24h under load
Isolate: git bisect points to commit abc123 (connection pool change)
Investigate:
  - src/db/pool.ts:42 - connections acquired but not released on error path
  - src/db/pool.ts:78 - try block without finally for cleanup
Hypothesize: Exception in query bypasses connection release (confidence: 85)
Fix: Add try-finally around connection usage at pool.ts:42
```

## Tools

- **find_symbol**: Locate symbols by name (`name_path_pattern`, `relative_path`, `depth`)
- **get_symbols_overview**: File structure overview (`relative_path`, `depth`)
- **find_referencing_symbols**: All references to a symbol (`name_path`, `relative_path`)
- **search_for_pattern**: Regex search across codebase (`substring_pattern`, `relative_path`)

## Evidence Standards

### Confidence Levels

| Score | Meaning | Basis |
|-------|---------|-------|
| 90-100 | High | Direct code evidence, explicit documentation |
| 70-89 | Strong | Multiple corroborating sources |
| 50-69 | Moderate | Reasonable inference with gaps |
| 0-49 | Low | Speculation, insufficient evidence |

### Coverage Levels

| Score | Meaning |
|-------|---------|
| 90-100 | All relevant files examined |
| 70-89 | Most relevant files examined |
| 50-69 | Key files examined, some gaps |
| 0-49 | Limited examination |

## Investigation Output Format

```
Question: [Restated question]
Investigation:
  - Source 1: path/to/file.ts:42 - finding description
  - Source 2: path/to/other.ts:15 - finding description
Conclusion: [Direct answer based on evidence]
Metrics: Confidence 85, Coverage 75
Recommendations: [Suggested actions]
Unclear Points: [Information gaps]
```

## Common Bug Patterns

- **Null reference**: Check all paths to null access; add null checks or ensure initialization
- **Race condition**: Look for shared mutable state and async operations; add synchronization
- **Off-by-one**: Check loop boundaries and index calculations; verify inclusive/exclusive
- **Resource leak**: Check acquisition/release paths; ensure cleanup in finally/defer
- **Encoding issue**: Trace encoding at each transformation step; ensure consistency

## Root Cause Analysis: Five Whys

```
Why did the server crash? -> Out of memory
Why out of memory? -> Connection pool exhausted
Why exhausted? -> Connections not being released
Why not released? -> Exception bypasses cleanup
Root cause: Missing try-finally for connection release
```

## Anti-Patterns

- **Speculation**: State confidence levels and gaps; never guess without evidence
- **Confirming assumptions**: Independently verify by examining code
- **Uncited claims**: Always provide `file:line` references
- **Premature implementation**: Complete investigation before proposing fixes

## Critical Rules

- Always provide `path/to/file.ext:line_number` references for all findings
- Rate confidence and coverage metrics for all results
- Complete investigation before proposing solutions
- Use Serena symbol tools before reading entire files
- Independently verify claims rather than confirming assumptions

## Error Escalation

| Severity | Example | Action |
|----------|---------|--------|
| Low | Incomplete evidence trail | Note in report, proceed |
| Medium | Conflicting evidence | Document, ask user for clarification |
| High | Cannot determine root cause | STOP, present options to user |
| Critical | Investigation reveals security issue | BLOCK, require user acknowledgment |

## Related Skills

- `serena-usage`: Memory operations and symbol-level code navigation
- `execution-workflow`: Implement fixes with proper delegation after investigation
- `fact-check`: Verify external documentation and library behavior
- `testing-patterns`: Add regression tests after fixing identified bugs
