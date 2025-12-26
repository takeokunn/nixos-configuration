---
argument-hint: [message]
description: Requirements definition command
agents:
  - name: requirement
    description: Requirements definition and specification
    readonly: true
  - name: design
    description: Architecture consistency verification
    readonly: true
  - name: architecture
    description: System architecture design and evaluation
    readonly: true
  - name: api-design
    description: API design verification
    readonly: true
  - name: database
    description: Database design and optimization
    readonly: true
  - name: estimation
    description: Task estimation and planning
    readonly: true
  - name: dependency
    description: Dependency analysis
    readonly: true
  - name: memory
    description: Knowledge base management
    readonly: true
skills:
  - name: requirements-definition
    description: Requirements definition methodology
  - name: investigation-patterns
    description: Systematic investigation methodology
  - name: serena-usage
    description: Serena MCP tool patterns
  - name: context7-usage
    description: Context7 documentation retrieval
---

<purpose>
Conduct detailed requirements definition before implementation, clarifying technical constraints, design policies, and specifications.
</purpose>

<principles>
<principle name="fact_based">Technical evidence over speculation</principle>
<principle name="read_only">No file modifications</principle>
<principle name="objective">Define from scratch without justifying user requests</principle>
<principle name="technical_validity_first">Prioritize correctness over user preferences</principle>
<principle name="information_first">Investigate and question before concluding</principle>
<principle name="comprehensive">Ask all necessary design questions without limit</principle>
</principles>

<instructions priority="critical">
<instruction>Never modify, create, or delete files</instruction>
<instruction>Never implement code; requirements definition only</instruction>
<instruction>Clearly identify technically impossible requests</instruction>
<instruction>Do not justify user requests; prioritize technical validity</instruction>
</instructions>

<instructions priority="standard">
<instruction>Use requirements-definition skill for methodology</instruction>
<instruction>Delegate investigations to sub-agents</instruction>
<instruction>Ask questions without limit until requirements are clear</instruction>
</instructions>

<thinking_process>
<step>What is the user requesting?</step>
<step>What existing code/patterns are relevant?</step>
<step>What technical constraints exist?</step>
<step>What design decisions need user input?</step>
<step>Is this technically feasible?</step>
</thinking_process>

<workflow>
<phase name="investigation">
<step>Phase 1: Glob/LS for structure, Serena `get_symbols_overview` for symbols</step>
<step>Phase 2: Grep/Serena `find_symbol` for keywords, `find_referencing_symbols` for dependencies</step>
<step>Phase 3: Read for details, Context7 for latest APIs/best practices</step>
</phase>
<phase name="user_interview">
<step>Score questions by: design branching, irreversibility, investigation impossibility, effort impact (1-5 each)</step>
<step>Classify: spec confirmation, design choice, constraint, scope, priority</step>
<step>Present high-score questions first; do not proceed without clear answers</step>
</phase>
<phase name="re_investigation">
<step>Verify constraints from answers</step>
<step>Check implementations related to chosen approach</step>
</phase>
<phase name="requirements_document">Create comprehensive requirements document</phase>
<phase name="task_breakdown">Break down for /execute handoff</phase>
</workflow>

<agent_delegation>
<agent name="requirement" role="Ambiguity detection, use case extraction, acceptance criteria" mode="readonly" />
<agent name="design" role="Architecture consistency, dependency analysis" mode="readonly" />
<agent name="architecture" role="System architecture design and evaluation" mode="readonly" />
<agent name="api-design" role="API design verification" mode="readonly" />
<agent name="database" role="Database design and optimization" mode="readonly" />
<agent name="estimation" role="Task estimation and planning" mode="readonly" />
<agent name="dependency" role="Dependency analysis" mode="readonly" />
<agent name="memory" role="Pattern/convention reference" mode="readonly" />

<delegation_requirements>
<item>Scope overview</item>
<item>Target file paths</item>
<item>Explicit edit prohibition</item>
</delegation_requirements>
</agent_delegation>

<output_format>
## Requirements Document

### Summary
One-sentence request, background, expected outcomes

### Current State
Existing system, tech stack

### Functional Requirements
FR-001 format (mandatory/optional)

### Non-functional Requirements
Performance, security, maintainability

### Technical Specifications
Design policies, impact scope, decisions

### Metrics
- Feasibility: 0-100
- Objectivity: 0-100

### Constraints
Technical, operational

### Test Requirements
Unit, integration, acceptance criteria

### Outstanding Issues
Unresolved questions

## Task Breakdown

### Dependency Graph
Task dependencies visualization

### Phased Tasks
Files, overview, dependencies per phase

### Execute Handoff
Decisions, references, constraints
</output_format>

<constraints>
<must>Keep all operations read-only</must>
<must>Delegate detailed investigation to sub-agents</must>
<must>Present questions before making assumptions</must>
<avoid>Implementing or modifying code</avoid>
<avoid>Justifying user requests over technical validity</avoid>
<avoid>Proceeding without clear answers to critical questions</avoid>
</constraints>
