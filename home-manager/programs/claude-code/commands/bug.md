---
argument-hint: [error-message]
description: Root cause investigation command
---

<purpose>
Identify root causes from error messages and anomalous behavior, providing fact-based analysis without performing fixes.
</purpose>

<rules priority="critical">
<rule>Never modify, create, or delete files</rule>
<rule>Never implement fixes; provide suggestions only</rule>
<rule>Prioritize log analysis as primary information source</rule>
<rule>Judge from facts, not user speculation</rule>
<rule>Logs as primary information source</rule>
</rules>

<rules priority="standard">
<rule>Use investigation-patterns skill for debugging methodology</rule>
<rule>Delegate investigations to debug agent</rule>
<rule>Report honestly if cause cannot be identified</rule>
<rule>Verify similar implementations nearby</rule>
<rule>Track occurrence path chronologically</rule>
</rules>

<workflow>
<phase name="analyze">
<step>What type of error is this? (syntax, runtime, logic)</step>
<step>Where does it occur? (file, line, function)</step>
<step>What logs are available?</step>
<step>What is the error context? (before, during, after)</step>
<step>Are there similar past issues in memory?</step>
</phase>
<phase name="analyze_error">
<step>Error type identification</step>
<step>Location identification (file, line, function)</step>
<step>Stack trace analysis</step>
<step>Timestamp verification</step>
</phase>
<phase name="investigate_logs" priority="critical">
<step>Application logs</step>
<step>System logs (if needed)</step>
<step>Error context (pre-error flow, details, post-error impact)</step>
</phase>
<phase name="investigate_code">
<step>Error location details</step>
<step>Dependencies/imports</step>
<step>Config files</step>
<step>Recent changes</step>
</phase>
<phase name="investigate_environment">
<step>Runtime (OS, versions, env vars)</step>
<step>Resources (disk, memory, network)</step>
</phase>
<phase name="report">Report findings with confidence metrics</phase>
</workflow>

<agents>
<agent name="debug" readonly="true">Error tracking, stack trace, log analysis</agent>
<agent name="observability" readonly="true">Log analysis support</agent>
<agent name="error-handling" readonly="true">Error handling patterns</agent>
<agent name="dependency" readonly="true">Dependency-related errors</agent>
<agent name="memory" readonly="true">Past troubleshooting records</agent>
</agents>

<delegation>
<requirement>Full error message/stack trace</requirement>
<requirement>Reproduction steps (if known)</requirement>
<requirement>Related file paths</requirement>
<requirement>Explicit edit prohibition</requirement>
</delegation>

<output>
<format>
<overview>Summary of error and investigation</overview>
<log_analysis>Critical log information, error context</log_analysis>
<code_analysis>Relevant code, identified issues</code_analysis>
<root_cause>
- Direct cause
- Underlying cause
- Conditions</root_cause>
<metrics>
- Confidence: 0-100
- Log Utilization: 0-100
- Objectivity: 0-100</metrics>
<impact>Scope, similar errors</impact>
<recommendations>Fix suggestions (no implementation), prevention</recommendations>
<further_investigation>Unclear points, next steps</further_investigation>
</format>
</output>

<constraints>
<must>Keep all operations read-only</must>
<must>Prioritize logs as primary information source</must>
<must>Report honestly if cause cannot be identified</must>
<avoid>Implementing fixes</avoid>
<avoid>Accepting user speculation without verification</avoid>
<avoid>Forcing contrived causes when evidence is insufficient</avoid>
</constraints>
