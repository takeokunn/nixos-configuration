---
argument-hint: [task-description]
description: Task execution command
---

<purpose>
Execute tasks by delegating detailed work to sub-agents while focusing on policy decisions and orchestration.
</purpose>

<rules priority="critical">
<rule>Delegate detailed work to specialized sub-agents</rule>
<rule>Focus on orchestration and policy decisions</rule>
<rule>Execute independent tasks in parallel</rule>
<rule>Verify sub-agent outputs before integration</rule>
</rules>

<rules priority="standard">
<rule>Use execution-workflow skill for delegation patterns</rule>
<rule>Prefer basic tools (Read/Edit/Write) over Codex MCP when sufficient</rule>
<rule>Use Codex MCP only for code generation/modification</rule>
<rule>Check Serena memories before implementation</rule>
</rules>

<workflow>
<phase name="analyze">
<step>What tasks need to be done?</step>
<step>Which sub-agents are best suited?</step>
<step>Which tasks can run in parallel?</step>
<step>What dependencies exist between tasks?</step>
<step>What verification is needed?</step>
</phase>
<phase name="break_down">Split into manageable units</phase>
<phase name="organize">Identify parallel vs sequential tasks</phase>
<phase name="delegate">Assign with detailed instructions</phase>
<phase name="integrate">Verify and combine results</phase>
</workflow>

<agents>
<agent name="quality" readonly="false">Syntax, type, format verification</agent>
<agent name="security" readonly="false">Vulnerability detection</agent>
<agent name="test" readonly="false">Test creation, coverage</agent>
<agent name="refactor" readonly="false">Refactoring, tech debt</agent>
<agent name="docs" readonly="false">Documentation updates</agent>
<agent name="review" readonly="false">Post-implementation review</agent>
<agent name="debug" readonly="false">Debug support</agent>
<agent name="performance" readonly="false">Performance optimization</agent>
<agent name="clean" readonly="false">Dead code elimination</agent>
<agent name="error-handling" readonly="false">Error handling patterns</agent>
<agent name="migration" readonly="false">Migration planning and execution</agent>
<agent name="database" readonly="false">Database design and optimization</agent>
<agent name="infrastructure" readonly="false">Infrastructure design</agent>
<agent name="ci-cd" readonly="false">CI/CD pipeline design</agent>
<agent name="observability" readonly="false">Logging, monitoring, tracing</agent>
<agent name="git" readonly="false">Git workflow design</agent>
<agent name="memory" readonly="false">Knowledge base management</agent>
</agents>

<parallel_execution>
<group name="quality_assurance">quality + security (no dependencies)</group>
<group name="implementation">test + docs (if independent)</group>
<note>review: Sequential - after implementation</note>
</parallel_execution>

<delegation>
<requirement>Specific scope and expected deliverables</requirement>
<requirement>Target file paths</requirement>
<requirement>Reference implementations (specific paths)</requirement>
<requirement>Memory check: `list_memories` for patterns</requirement>
</delegation>

<codex_usage>
<allowed>Code generation (new files/functions), code modification (editing/refactoring)</allowed>
<prohibited>
<task alternative="Explore agent, Serena MCP">Research/analysis</task>
<task alternative="quality agent">Quality verification</task>
<task alternative="security agent">Security verification</task>
<task alternative="test agent">Test creation</task>
<task alternative="docs agent">Documentation</task>
<task alternative="review agent">Code review</task>
</prohibited>
<rule>Prefer basic tools when sufficient</rule>
<rule>One clear, small task per call</rule>
<rule>Separate phases: research → design → implementation</rule>
<rule>No multi-file edits in single call</rule>
</codex_usage>

<constraints>
<must>Delegate detailed work to sub-agents</must>
<must>Execute independent tasks in parallel</must>
<must>Verify outputs before integration</must>
<avoid>Implementing detailed logic directly</avoid>
<avoid>Unnecessary comments about past implementations</avoid>
<avoid>Multi-file edits in single Codex call</avoid>
</constraints>
