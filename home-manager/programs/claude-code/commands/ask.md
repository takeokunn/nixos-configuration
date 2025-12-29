---
argument-hint: [question]
description: Question and inquiry command
---

<purpose>
Provide accurate, evidence-based answers to project questions through fact-based investigation. Operates in read-only mode; never modifies files.
</purpose>

<rules priority="critical">
<rule>NEVER modify, create, or delete files</rule>
<rule>NEVER implement fixes; provide analysis and suggestions only</rule>
<rule>ALWAYS base answers on factual investigation from code and documentation</rule>
<rule>ALWAYS report confidence levels and unclear points honestly</rule>
<rule>NEVER justify user assumptions; prioritize technical accuracy</rule>
</rules>

<rules priority="standard">
<rule>Use investigation-patterns skill for systematic analysis</rule>
<rule>Delegate to appropriate agents in parallel</rule>
<rule>Provide file:line references for all findings</rule>
</rules>

<workflow>
<phase name="analyze">
<step>What is the user's core question?</step>
<step>Which code/documentation sources are relevant?</step>
<step>What scope of investigation is appropriate?</step>
<step>Classify question type (architecture, implementation, debugging, design)</step>
</phase>
<phase name="investigate">
<step>Delegate to explore agent: find relevant files and codebase structure</step>
<step>Delegate to design agent: evaluate architecture and component relationships</step>
<step>Delegate to performance agent: identify performance-related aspects (if applicable)</step>
</phase>
<phase name="synthesize">
<step>Delegate to quality-assurance agent: evaluate code quality findings</step>
<step>Delegate to code-quality agent: analyze complexity metrics</step>
<step>Compile agent findings with confidence metrics</step>
</phase>
</workflow>

<agents>
<agent name="explore" subagent_type="explore" readonly="true">Finding files, exploring codebase structure</agent>
<agent name="design" subagent_type="design" readonly="true">System design, architecture, API structure</agent>
<agent name="performance" subagent_type="performance" readonly="true">Performance bottlenecks, optimization questions</agent>
<agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">Code quality evaluation, best practices</agent>
<agent name="code-quality" subagent_type="code-quality" readonly="true">Code complexity analysis</agent>
</agents>

<parallel_execution>
<group name="investigation" execution="parallel">
<agent>explore</agent>
<agent>design</agent>
<agent>performance</agent>
</group>
<group name="synthesis" execution="parallel">
<agent>quality-assurance</agent>
<agent>code-quality</agent>
</group>
</parallel_execution>

<output>
<format>
<question>Restate the user's question for confirmation</question>
<investigation>Evidence-based findings with file:line references
- Source 1: `path/to/file.ts:42` - finding
- Source 2: `path/to/other.ts:15` - finding</investigation>
<conclusion>Direct answer based on evidence</conclusion>
<metrics>
- Confidence: 0-100 (based on evidence quality)
- Evidence Coverage: 0-100 (how much relevant code was examined)</metrics>
<recommendations>Optional: Suggested actions without implementation</recommendations>
<unclear_points>Information gaps that would improve the answer</unclear_points>
</format>
</output>

<constraints>
<must>Keep all operations read-only</must>
<must>Provide file:line references for findings</must>
<must>Report confidence levels honestly</must>
<must>Distinguish between facts and inferences</must>
<avoid>Implementing or modifying any code</avoid>
<avoid>Guessing when evidence is insufficient</avoid>
<avoid>Confirming user assumptions without verification</avoid>
</constraints>
