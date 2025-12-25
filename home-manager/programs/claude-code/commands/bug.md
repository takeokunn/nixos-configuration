---
argument-hint: [error-message]
description: Root cause investigation command
agents:
  - name: debug
    description: Bug investigation and debug support
    readonly: true
  - name: observability
    description: Logging, monitoring, tracing design
    readonly: true
  - name: error-handling
    description: Error handling pattern verification
    readonly: true
  - name: dependency
    description: Dependency-related error analysis
    readonly: true
  - name: memory
    description: Past troubleshooting record reference
    readonly: true
readonly_tools:
  - name: Read
    description: File content verification
  - name: Grep
    description: Pattern search
  - name: Glob
    description: File exploration
  - name: LS
    description: Directory structure verification
  - name: context7
    description: Latest framework/library documentation
  - name: serena
    description: Semantic search, LSP search, documentation
---

# /bug

## Purpose
Identify root causes from error messages and anomalous behavior, providing fact-based analysis without performing fixes.

## Principles
- **Logs first**: Logs as primary information source
- **Check surrounding code**: Verify similar implementations nearby
- **Systematic investigation**: Track occurrence path chronologically
- **Fact-based**: Judge from facts, not user assumptions

## Workflow

1. **Error Message Analysis**
   - Error type identification (syntax, runtime, logic, etc.)
   - Error location identification (file, line, function)
   - Stack trace analysis
   - Timestamp verification

2. **Log Investigation** (Critical)
   - Application log exploration and verification
   - System log verification (as needed)
   - Log context around error (pre-error flow, error details, post-error impact)

3. **Code Investigation**
   - Tools: Read, Grep, Glob, LS, context7, serena, playwright (all read-only)
   - Items: Error location code details, dependencies/imports, config files, environment variables, recent change history

4. **Environment Investigation**
   - Runtime environment (OS, runtime version, dependency versions, environment variables)
   - Resource status (as needed: disk, memory, network)

5. **Investigation Results Delivery**

## Agent Delegation

| Agent | Role | Readonly |
|-------|------|----------|
| debug | Error tracking, stack trace analysis, log analysis, root cause identification | yes |
| observability | Log analysis support | yes |
| error-handling | Error handling pattern verification and suggestions | yes |
| dependency | Dependency-related error analysis | yes |
| memory | Past troubleshooting record reference | yes |

### Delegation Instructions
1. Full error message/stack trace
2. Reproduction steps (if known)
3. Related file paths
4. Serena MCP usage (`find_symbol`, `find_referencing_symbols`, `search_for_pattern`)
5. Context7 MCP usage (library version compatibility verification)
6. **Explicit edit operation prohibition**

### Expected Output Format
- Root cause identification
- Reproduction steps
- Fix suggestions (no implementation)
- Prevention measures

## Output

**Answer Structure**:
- Overview: Report and investigation summary
- Log Analysis Results: Critical information from logs, error context, related warnings/info
- Code Investigation Results: Relevant code quotes, issue identification
- Cause Identification: Direct cause, root cause (design/config/environment), occurrence conditions
- Investigation Metrics:
  - Confidence: 0-100
  - Log utilization: 0-100
  - Objectivity: 0-100
- Related Information: Impact scope, similar errors
- Response Policy (no implementation): Recommended fixes, alternative approaches, prevention measures
- Additional Investigation Items: Unclear points or items requiring further confirmation

## Constraints
- Absolutely no file editing/creation/deletion
- No error fixes or implementation (investigation and analysis only)
- Prioritize logs as primary information source
- Honestly report unclear points, indicate need for additional investigation
- Don't accept user speculation at face value; judge from facts
- Provide fact-based investigation results instead of apologies
- If problem cannot be identified, don't force contrived causes or unreasonable speculation
- If problem cannot be identified, report this and suggest next steps (sequential logging with numbers, behavior verification, etc.)
