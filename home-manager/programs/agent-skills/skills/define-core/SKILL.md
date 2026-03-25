---
name: define-core
description: "Use when implementing '/define' or '/define-full' commands to ensure consistent workflow structure, agent delegation, and requirements documentation. Provides shared phases for preparation, analysis, investigation, clarification, verification, and documentation."
---

Shared workflow phases, agent definitions, and patterns common to all requirements definition commands (`/define`, `/define-full`). Eliminates duplication and ensures consistency across requirements definition workflows.

## Workflow Phases

### 1. Prepare

1. Activate Serena project with `activate_project`
2. Check `list_memories` for relevant patterns
3. Load applicable memories with `read_memory`

### 2. Analyze

1. Parse user request to extract core requirements
2. Identify technical constraints from request context
3. Determine design decisions requiring user input
4. Assess technical feasibility at high level

### 3. Investigate

Run agents in parallel for evidence gathering:

1. **Explore agent**: Find relevant files and existing patterns
2. **Design agent**: Evaluate architecture consistency and dependencies
3. **Database agent**: Analyze database design (if applicable)
4. **General-purpose agent**: Analyze requirements and estimate effort
5. **Fact-check**: Verify external documentation via Context7

**Reflection checkpoint** after investigation (threshold 70):
- Have all relevant files and patterns been identified? (weight: 0.4)
- Is the scope clearly understood? (weight: 0.3)
- Are there any technical blockers identified? (weight: 0.3)

### 4. Clarify

1. Score questions by: design branching, irreversibility, investigation impossibility, effort impact (1-5 each)
2. Classify questions: spec confirmation, design choice, constraint, scope, priority
3. Use `AskUserQuestion` tool for all interactions (2-4 structured options per question)
4. Always include a (Recommended) option when presenting choices
5. Present high-score questions first; do not proceed without clear answers

### 5. Verify

1. Verify constraints from answers using agent findings
2. Check implementations related to chosen approach

### 6. Document

1. Create comprehensive requirements document
2. Break down tasks for `/execute` handoff

## Agents

| Agent | Type | Purpose |
|-------|------|---------|
| explore | explore (readonly) | Finding relevant files and existing patterns |
| design | design (readonly) | Architecture consistency, dependency analysis, API design |
| database | database (readonly) | Database design and optimization |
| general-purpose | general-purpose (readonly) | Requirements analysis, estimation |
| validator | validator (readonly) | Cross-validation and consensus verification |

## Execution Graph

```
[explore] ──┐
[design]  ──┼── parallel ──> [general-purpose] ──> sequential
[database] ─┘
```

## Delegation Requirements

Each sub-agent delegation must include: scope overview, target file paths, explicit edit prohibition, and instruction to use `AskUserQuestion` tool for any user interactions.

## Output Format

### Requirements Document

- **Summary**: One-sentence request, background, expected outcomes
- **Current state**: Existing system, tech stack
- **Functional requirements**: FR-001 format (mandatory/optional)
- **Non-functional requirements**: Performance, security, maintainability
- **Technical specifications**: Design policies, impact scope, decisions
- **Metrics**: Feasibility (0-100), Objectivity (0-100)
- **Constraints**: Technical, operational
- **Test requirements**: Unit, integration, acceptance criteria

### Task Breakdown

- Dependency graph, phased tasks (files, overview, dependencies), execute handoff (decisions, references, constraints)

## Critical Rules

- Never modify, create, or delete files -- requirements definition only
- Never implement code
- Clearly identify technically impossible requests
- Prioritize technical validity over user preferences
- Technical evidence over speculation
- Ask questions without limit until requirements are clear
- Always include a (Recommended) option when presenting choices

## Command Usage

- **`/define`**: Inherits all phases, agents, execution graph, delegation, and rules
- **`/define-full`**: Inherits all as subphases within `define_initial`, adds plan agent, extends execution graph with feedback and regenerate phases

## Anti-Patterns to Avoid

- Modifying or creating code files (this is a read-only command)
- Proceeding without answering critical questions
- Justifying user requests over technical validity
- Using plain text for questions instead of `AskUserQuestion` tool
