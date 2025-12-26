---
name: Investigation Patterns
description: This skill should be used when the user asks to "investigate code", "analyze implementation", "find patterns", "understand codebase", "debug issue", "find bug", "troubleshoot", or needs evidence-based code analysis and debugging. Provides systematic investigation and debugging methodology.
version: 0.2.0
---

<purpose>
Provide systematic patterns for codebase investigation and debugging, ensuring evidence-based analysis with proper confidence assessment.
</purpose>

<investigation_workflow>
<step id="1" name="scope_classification">
Classify the question type:
<type name="architecture">System design, component relationships</type>
<type name="implementation">Specific code behavior, algorithm details</type>
<type name="debugging">Error causes, unexpected behavior</type>
<type name="design">Pattern usage, code organization</type>
</step>

<step id="2" name="source_identification">
Identify relevant sources:
<source name="code">Use Serena for symbol search and dependency analysis</source>
<source name="documentation">Check inline comments, README, API docs</source>
<source name="history">Git log for context on changes</source>
<source name="external">Context7 for library documentation</source>
</step>

<step id="3" name="evidence_collection">
Collect evidence systematically:
<tool name="find_symbol">Locate specific symbols by name</tool>
<tool name="get_symbols_overview">Understand file structure</tool>
<tool name="find_referencing_symbols">Trace dependencies</tool>
<tool name="search_for_pattern">Find patterns across codebase</tool>
</step>

<step id="4" name="synthesis">
Synthesize findings with confidence metrics.
</step>
</investigation_workflow>

<debugging_workflow>
<phase id="1" name="reproduce">
Confirm the issue is reproducible.
<action>Gather exact steps to reproduce</action>
<action>Identify environment conditions</action>
<action>Determine consistency (always/sometimes fails)</action>
</phase>

<phase id="2" name="isolate">
Narrow down the problem scope.
<action>Identify when issue started (git bisect if needed)</action>
<action>Remove unrelated components</action>
<action>Create minimal reproduction case</action>
</phase>

<phase id="3" name="investigate">
Collect evidence systematically.
<action>Examine error messages and stack traces</action>
<action>Check logs at relevant timestamps</action>
<action>Use Serena for code path analysis</action>
<action>Trace data flow through the system</action>
</phase>

<phase id="4" name="hypothesize">
Form and test hypotheses.
<action>List possible causes</action>
<action>Rank by likelihood</action>
<action>Design tests to confirm/refute each</action>
</phase>

<phase id="5" name="fix">
Implement and verify solution.
<action>Make minimal targeted change</action>
<action>Verify fix resolves the issue</action>
<action>Check for regressions</action>
<action>Add test to prevent recurrence</action>
</phase>
</debugging_workflow>

<evidence_standards>
<standard name="citation">
Always provide file:line references for findings.
Format: `path/to/file.ext:line_number`
</standard>

<standard name="confidence">
Rate confidence based on evidence quality:
<level range="90-100">Direct code evidence, explicit documentation</level>
<level range="70-89">Strong inference from multiple sources</level>
<level range="50-69">Reasonable inference with some gaps</level>
<level range="0-49">Speculation, insufficient evidence</level>
</standard>

<standard name="coverage">
Report how much relevant code was examined:
<level range="90-100">All relevant files examined</level>
<level range="70-89">Most relevant files examined</level>
<level range="50-69">Key files examined, some gaps</level>
<level range="0-49">Limited examination</level>
</standard>
</evidence_standards>

<common_bug_patterns>
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
</common_bug_patterns>

<root_cause_analysis>
<technique name="five_whys">
Ask "why" repeatedly to drill to root cause.
<example>
Why did the server crash? - Out of memory
Why out of memory? - Connection pool exhausted
Why exhausted? - Connections not being released
Why not released? - Exception bypasses cleanup
Root cause: Missing try-finally for connection release
</example>
</technique>

<technique name="timeline_analysis">
Reconstruct sequence of events leading to failure.
<action>Collect timestamps from logs</action>
<action>Order events chronologically</action>
<action>Identify divergence from expected behavior</action>
</technique>
</root_cause_analysis>

<output_structure>
<section name="question">Restate the question for confirmation</section>
<section name="investigation">
Evidence-based findings with file:line references
<item>Source 1: `path/to/file.ts:42` - finding description</item>
<item>Source 2: `path/to/other.ts:15` - finding description</item>
</section>
<section name="conclusion">Direct answer based on evidence</section>
<section name="metrics">
<item>Confidence: 0-100</item>
<item>Evidence Coverage: 0-100</item>
</section>
<section name="recommendations">Suggested actions without implementation</section>
<section name="unclear_points">Information gaps that would improve the answer</section>
</output_structure>

<debugging_output>
<section name="problem_statement">Clear description of the issue</section>
<section name="reproduction_steps">How to reproduce</section>
<section name="investigation">Evidence collected with file:line references</section>
<section name="root_cause">Identified cause with supporting evidence</section>
<section name="solution">Proposed fix with rationale</section>
<section name="verification">How to verify the fix works</section>
<section name="prevention">How to prevent recurrence</section>
</debugging_output>

<anti_patterns>
<avoid>Guessing when evidence is insufficient</avoid>
<avoid>Confirming user assumptions without verification</avoid>
<avoid>Making claims without file:line references</avoid>
<avoid>Implementing fixes instead of analyzing</avoid>
</anti_patterns>
