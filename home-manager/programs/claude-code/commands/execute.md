---
argument-hint: [task-description]
description: Task execution command
agents:
  - name: quality
    description: Code quality assurance
    readonly: false
  - name: security
    description: Security vulnerability detection
    readonly: false
  - name: test
    description: Test strategy and quality management
    readonly: false
  - name: refactor
    description: Refactoring and technical debt resolution
    readonly: false
  - name: docs
    description: Documentation management
    readonly: false
  - name: review
    description: Code review
    readonly: false
  - name: debug
    description: Debug support
    readonly: false
  - name: performance
    description: Performance optimization
    readonly: false
  - name: clean
    description: Dead code elimination
    readonly: false
  - name: error-handling
    description: Error handling pattern verification
    readonly: false
  - name: migration
    description: Migration planning and execution
    readonly: false
  - name: i18n
    description: Internationalization support
    readonly: false
  - name: accessibility
    description: Web accessibility verification
    readonly: false
  - name: database
    description: Database design and optimization
    readonly: false
  - name: infrastructure
    description: Infrastructure design
    readonly: false
  - name: ci-cd
    description: CI/CD pipeline design
    readonly: false
  - name: observability
    description: Logging, monitoring, tracing design
    readonly: false
  - name: git
    description: Git workflow design
    readonly: false
  - name: merge
    description: Conflict resolution
    readonly: false
  - name: memory
    description: Knowledge base management
    readonly: false
skills:
  - name: execution-workflow
    description: Task execution, delegation, and code review patterns
  - name: testing-patterns
    description: Testing strategy and patterns
  - name: serena-usage
    description: Serena MCP tool patterns
  - name: context7-usage
    description: Context7 documentation retrieval
---

<purpose>
Execute tasks by delegating detailed work to sub-agents while focusing on policy decisions and orchestration.
</purpose>

<instructions priority="critical">
<instruction>Delegate detailed work to specialized sub-agents</instruction>
<instruction>Focus on orchestration and policy decisions</instruction>
<instruction>Execute independent tasks in parallel</instruction>
<instruction>Verify sub-agent outputs before integration</instruction>
</instructions>

<instructions priority="standard">
<instruction>Use execution-workflow skill for delegation patterns</instruction>
<instruction>Prefer basic tools (Read/Edit/Write) over Codex MCP when sufficient</instruction>
<instruction>Use Codex MCP only for code generation/modification</instruction>
<instruction>Check Serena memories before implementation</instruction>
</instructions>

<thinking_process>
<step>What tasks need to be done?</step>
<step>Which sub-agents are best suited?</step>
<step>Which tasks can run in parallel?</step>
<step>What dependencies exist between tasks?</step>
<step>What verification is needed?</step>
</thinking_process>

<workflow>
<phase name="analyze">Understand requirements, identify scope</phase>
<phase name="break_down">Split into manageable units</phase>
<phase name="organize">Identify parallel vs sequential tasks</phase>
<phase name="delegate">Assign with detailed instructions</phase>
<phase name="integrate">Verify and combine results</phase>
</workflow>

<agent_delegation>
<phase name="quality_assurance">
<agent name="quality" role="Syntax, type, format verification" />
<agent name="security" role="Vulnerability detection" />
<execution>Parallel - no dependencies</execution>
</phase>

<phase name="implementation">
<agent name="test" role="Test creation, coverage" />
<agent name="refactor" role="Refactoring, tech debt" />
<agent name="docs" role="Documentation updates" />
<execution>Can run in parallel if independent</execution>
</phase>

<phase name="review">
<agent name="review" role="Post-implementation review" />
<execution>Sequential - after implementation</execution>
</phase>

<parallel_execution>
<rule>quality + security: Concurrent checks</rule>
<rule>test + docs: Simultaneous creation</rule>
</parallel_execution>
</agent_delegation>

<codex_usage>
<allowed>
<action>Code generation (new files/functions)</action>
<action>Code modification (editing/refactoring)</action>
</allowed>

<prohibited>
<task use_instead="Explore agent, Serena MCP">Research/analysis</task>
<task use_instead="quality agent">Quality verification</task>
<task use_instead="security agent">Security verification</task>
<task use_instead="test agent">Test creation</task>
<task use_instead="docs agent">Documentation</task>
<task use_instead="review agent">Code review</task>
</prohibited>

<principles>
<principle>Prefer basic tools when sufficient</principle>
<principle>One clear, small task per call</principle>
<principle>Separate phases: research → design → implementation</principle>
<principle>No multi-file edits in single call</principle>
</principles>
</codex_usage>

<delegation_instructions>
<item>Specific scope and expected deliverables</item>
<item>Target file paths</item>
<item>Reference implementations (specific paths)</item>
<item>Memory check: `list_memories` for patterns</item>
</delegation_instructions>

<constraints>
<must>Delegate detailed work to sub-agents</must>
<must>Execute independent tasks in parallel</must>
<must>Verify outputs before integration</must>
<avoid>Implementing detailed logic directly</avoid>
<avoid>Unnecessary comments about past implementations</avoid>
<avoid>Multi-file edits in single Codex call</avoid>
</constraints>
