---
name: Investigation Patterns
description: This skill should be used when the user asks to "investigate code", "analyze implementation", "find patterns", "understand codebase", "debug issue", "find bug", "troubleshoot", or needs evidence-based code analysis and debugging. Provides systematic investigation and debugging methodology.
version: 0.2.0
---

<purpose>
Provide systematic patterns for codebase investigation and debugging, ensuring evidence-based analysis with proper confidence assessment.
</purpose>

<workflow>
<phase name="scope_classification">
<description>Classify the question type</description>
<type name="architecture">System design, component relationships</type>
<type name="implementation">Specific code behavior, algorithm details</type>
<type name="debugging">Error causes, unexpected behavior</type>
<type name="design">Pattern usage, code organization</type>
</phase>

<phase name="source_identification">
<description>Identify relevant sources</description>
<source name="code">Use Serena for symbol search and dependency analysis</source>
<source name="documentation">Check inline comments, README, API docs</source>
<source name="history">Git log for context on changes</source>
<source name="external">Context7 for library documentation</source>
</phase>

<phase name="evidence_collection">
<description>Collect evidence systematically</description>
<tool name="find_symbol">Locate specific symbols by name</tool>
<tool name="get_symbols_overview">Understand file structure</tool>
<tool name="find_referencing_symbols">Trace dependencies</tool>
<tool name="search_for_pattern">Find patterns across codebase</tool>
</phase>

<phase name="synthesis">
<description>Synthesize findings with confidence metrics</description>
</phase>
</workflow>

<debugging>
<phase name="reproduce">
<description>Confirm the issue is reproducible</description>
<step>Gather exact steps to reproduce</step>
<step>Identify environment conditions</step>
<step>Determine consistency (always/sometimes fails)</step>
</phase>

<phase name="isolate">
<description>Narrow down the problem scope</description>
<step>Identify when issue started (git bisect if needed)</step>
<step>Remove unrelated components</step>
<step>Create minimal reproduction case</step>
</phase>

<phase name="investigate">
<description>Collect evidence systematically</description>
<step>Examine error messages and stack traces</step>
<step>Check logs at relevant timestamps</step>
<step>Use Serena for code path analysis</step>
<step>Trace data flow through the system</step>
</phase>

<phase name="hypothesize">
<description>Form and test hypotheses</description>
<step>List possible causes</step>
<step>Rank by likelihood</step>
<step>Design tests to confirm/refute each</step>
</phase>

<phase name="fix">
<description>Implement and verify solution</description>
<step>Make minimal targeted change</step>
<step>Verify fix resolves the issue</step>
<step>Check for regressions</step>
<step>Add test to prevent recurrence</step>
</phase>
</debugging>

<evidence_standards>
<standard name="citation">
<description>Always provide file:line references for findings</description>
<format>path/to/file.ext:line_number</format>
</standard>

<standard name="confidence">
<description>Rate confidence based on evidence quality</description>
<level range="90-100">Direct code evidence, explicit documentation</level>
<level range="70-89">Strong inference from multiple sources</level>
<level range="50-69">Reasonable inference with some gaps</level>
<level range="0-49">Speculation, insufficient evidence</level>
</standard>

<standard name="coverage">
<description>Report how much relevant code was examined</description>
<level range="90-100">All relevant files examined</level>
<level range="70-89">Most relevant files examined</level>
<level range="50-69">Key files examined, some gaps</level>
<level range="0-49">Limited examination</level>
</standard>
</evidence_standards>

<bug_patterns>
<pattern name="null_reference">
<symptom>NullPointerException, undefined is not a function</symptom>
<investigation>Check all paths to the null access</investigation>
<fix>Add null checks or ensure initialization</fix>
</pattern>

<pattern name="race_condition">
<symptom>Intermittent failures, works sometimes</symptom>
<investigation>Look for shared mutable state, async operations</investigation>
<fix>Add synchronization or redesign for immutability</fix>
</pattern>

<pattern name="off_by_one">
<symptom>Missing first/last element, index out of bounds</symptom>
<investigation>Check loop boundaries and index calculations</investigation>
<fix>Verify start/end conditions, use inclusive/exclusive correctly</fix>
</pattern>

<pattern name="resource_leak">
<symptom>Memory growth, connection exhaustion</symptom>
<investigation>Check resource acquisition and release paths</investigation>
<fix>Ensure cleanup in finally/defer, use resource management patterns</fix>
</pattern>

<pattern name="encoding_issue">
<symptom>Garbled text, unexpected characters</symptom>
<investigation>Trace encoding at each transformation step</investigation>
<fix>Ensure consistent encoding throughout pipeline</fix>
</pattern>
</bug_patterns>

<root_cause_analysis>
<technique name="five_whys">
<description>Ask "why" repeatedly to drill to root cause</description>
<example>
Why did the server crash? - Out of memory
Why out of memory? - Connection pool exhausted
Why exhausted? - Connections not being released
Why not released? - Exception bypasses cleanup
Root cause: Missing try-finally for connection release
</example>
</technique>

<technique name="timeline_analysis">
<description>Reconstruct sequence of events leading to failure</description>
<step>Collect timestamps from logs</step>
<step>Order events chronologically</step>
<step>Identify divergence from expected behavior</step>
</technique>
</root_cause_analysis>

<output>
<format>
## Question
Restate the question for confirmation

## Investigation

Evidence-based findings with file:line references

- Source 1: `path/to/file.ts:42` - finding description
- Source 2: `path/to/other.ts:15` - finding description

## Conclusion

Direct answer based on evidence

## Metrics

- Confidence: 0-100
- Evidence Coverage: 0-100

## Recommendations

Suggested actions without implementation

## Unclear Points

Information gaps that would improve the answer
</format>

<debugging_format>

## Problem Statement

Clear description of the issue

## Reproduction Steps

How to reproduce

## Investigation

Evidence collected with file:line references

## Root Cause

Identified cause with supporting evidence

## Solution

Proposed fix with rationale

## Verification

How to verify the fix works

## Prevention

How to prevent recurrence
</debugging_format>
</output>

<constraints>
<avoid>Guessing when evidence is insufficient</avoid>
<avoid>Confirming user assumptions without verification</avoid>
<avoid>Making claims without file:line references</avoid>
<avoid>Implementing fixes instead of analyzing</avoid>
</constraints>
