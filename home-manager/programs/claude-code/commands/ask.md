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
<step>Can I answer with high confidence, or do I need more information?</step>
<step>Which agents should be delegated to?</step>
</phase>
<phase name="scope">Classify question type (architecture, implementation, debugging, design)</phase>
<phase name="investigate">Find relevant files, check library documentation</phase>
<phase name="delegate">Send to appropriate agents in parallel</phase>
<phase name="synthesize">Compile findings with confidence metrics</phase>
</workflow>

<agents>
<agent name="Explore" readonly="true">Finding files, exploring codebase structure</agent>
<agent name="review" readonly="true">Evaluating code quality, identifying best practices</agent>
<agent name="architecture" readonly="true">System design questions, component relationships</agent>
<agent name="dependency" readonly="true">Package dependencies, version compatibility</agent>
<agent name="api-design" readonly="true">API structure, endpoint design questions</agent>
<agent name="performance" readonly="true">Performance bottlenecks, optimization questions</agent>
<agent name="memory" readonly="true">Checking existing patterns and conventions</agent>
</agents>

<output>
<format>
## Question
Restate the user's question for confirmation

## Investigation

Evidence-based findings with file:line references

- Source 1: `path/to/file.ts:42` - finding
- Source 2: `path/to/other.ts:15` - finding

## Conclusion

Direct answer based on evidence

## Metrics

- Confidence: 0-100 (based on evidence quality)
- Evidence Coverage: 0-100 (how much relevant code was examined)

## Recommendations

Optional: Suggested actions without implementation

## Unclear Points

Information gaps that would improve the answer
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
