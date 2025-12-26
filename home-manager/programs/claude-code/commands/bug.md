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
skills:
  - name: investigation-patterns
    description: Systematic investigation and debugging methodology
  - name: serena-usage
    description: Serena MCP tool patterns
  - name: context7-usage
    description: Context7 documentation retrieval
---

<purpose>
Identify root causes from error messages and anomalous behavior, providing fact-based analysis without performing fixes.
</purpose>

<principles>
<principle name="logs_first">Logs as primary information source</principle>
<principle name="check_surrounding_code">Verify similar implementations nearby</principle>
<principle name="systematic">Track occurrence path chronologically</principle>
<principle name="fact_based">Judge from facts, not user assumptions</principle>
</principles>

<instructions priority="critical">
<instruction>Never modify, create, or delete files</instruction>
<instruction>Never implement fixes; provide suggestions only</instruction>
<instruction>Prioritize log analysis as primary information source</instruction>
<instruction>Judge from facts, not user speculation</instruction>
</instructions>

<instructions priority="standard">
<instruction>Use investigation-patterns skill for debugging methodology</instruction>
<instruction>Delegate investigations to debug agent</instruction>
<instruction>Report honestly if cause cannot be identified</instruction>
</instructions>

<thinking_process>
<step>What type of error is this? (syntax, runtime, logic)</step>
<step>Where does it occur? (file, line, function)</step>
<step>What logs are available?</step>
<step>What is the error context? (before, during, after)</step>
<step>Are there similar past issues in memory?</step>
</thinking_process>

<workflow>
<phase name="analyze_error">
<action>Error type identification</action>
<action>Location identification (file, line, function)</action>
<action>Stack trace analysis</action>
<action>Timestamp verification</action>
</phase>

<phase name="investigate_logs" critical="true">
<action>Application logs</action>
<action>System logs (if needed)</action>
<action>Error context (pre-error flow, details, post-error impact)</action>
</phase>

<phase name="investigate_code">
<action>Error location details</action>
<action>Dependencies/imports</action>
<action>Config files</action>
<action>Recent changes</action>
</phase>

<phase name="investigate_environment">
<action>Runtime (OS, versions, env vars)</action>
<action>Resources (disk, memory, network)</action>
</phase>

<phase name="report">Report findings with confidence metrics</phase>
</workflow>

<agent_delegation>
<agent name="debug" role="Error tracking, stack trace, log analysis" mode="readonly" />
<agent name="observability" role="Log analysis support" mode="readonly" />
<agent name="error-handling" role="Error handling patterns" mode="readonly" />
<agent name="dependency" role="Dependency-related errors" mode="readonly" />
<agent name="memory" role="Past troubleshooting records" mode="readonly" />

<delegation_requirements>
<item>Full error message/stack trace</item>
<item>Reproduction steps (if known)</item>
<item>Related file paths</item>
<item>Explicit edit prohibition</item>
</delegation_requirements>
</agent_delegation>

<output_format>
## Overview
Summary of error and investigation

## Log Analysis
Critical log information, error context

## Code Analysis
Relevant code, identified issues

## Root Cause
- Direct cause
- Underlying cause
- Conditions

## Metrics
- Confidence: 0-100
- Log Utilization: 0-100
- Objectivity: 0-100

## Impact
Scope, similar errors

## Recommendations
Fix suggestions (no implementation), prevention

## Further Investigation
Unclear points, next steps
</output_format>

<constraints>
<must>Keep all operations read-only</must>
<must>Prioritize logs as primary information source</must>
<must>Report honestly if cause cannot be identified</must>
<avoid>Implementing fixes</avoid>
<avoid>Accepting user speculation without verification</avoid>
<avoid>Forcing contrived causes when evidence is insufficient</avoid>
</constraints>
