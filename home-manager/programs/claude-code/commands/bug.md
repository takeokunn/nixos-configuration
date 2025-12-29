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
</phase>
<phase name="investigate">
<step>Delegate to quality-assurance agent: analyze stack trace, error patterns</step>
<step>Delegate to explore agent: find error location and related code paths</step>
<step>Delegate to general-purpose agent: analyze logs and dependencies</step>
<step>Analyze error location details from agent findings</step>
<step>Review dependencies and imports</step>
<step>Check config files and recent changes</step>
</phase>
<phase name="gather">
<step>Collect runtime info (OS, versions, env vars)</step>
<step>Check resources (disk, memory, network)</step>
</phase>
<phase name="report">
<step>Compile agent findings with confidence metrics</step>
<step>Identify root cause with supporting evidence</step>
</phase>
</workflow>

<agents>
<agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">Error tracking, stack trace analysis, debugging</agent>
<agent name="general-purpose" subagent_type="general-purpose" readonly="true">Log analysis, observability, dependency errors</agent>
<agent name="explore" subagent_type="explore" readonly="true">Finding error locations, related code paths</agent>
</agents>

<parallel_execution>
<group name="error_analysis" execution="parallel">
<agent>quality-assurance</agent>
<agent>explore</agent>
</group>
<group name="context_gathering" execution="parallel">
<agent>general-purpose</agent>
</group>
</parallel_execution>

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
