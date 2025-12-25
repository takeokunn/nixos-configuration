---
argument-hint: [question]
description: Question and inquiry command
agents:
  - name: review
    description: Code quality evaluation and best practices
    readonly: true
  - name: Explore
    description: Codebase exploration and file search
    readonly: true
  - name: architecture
    description: System architecture design and evaluation
    readonly: true
  - name: dependency
    description: Dependency analysis
    readonly: true
  - name: api-design
    description: API design verification
    readonly: true
  - name: performance
    description: Performance optimization
    readonly: true
  - name: memory
    description: Knowledge base management
    readonly: true
readonly_tools:
  - name: Read
    description: File content verification
  - name: Grep
    description: Pattern search
  - name: Glob
    description: File exploration
  - name: LS
    description: Directory structure verification
  - name: context7
    description: Latest framework/library documentation
  - name: serena
    description: Semantic search, LSP search, documentation
---

# /ask

## Purpose
Provide fact-based analysis and answers to project questions without performing any implementation or edits.

## Principles
- **Fact-based**: Answers grounded in code and documentation, not speculation
- **Technical validity first**: Objective thinking from scratch, not justifying user assumptions
- **Read-only**: No file modifications
- **Honest responses**: Don't force answers; confirm needed information

## Workflow

1. **Question Analysis**
   - Clarify question essence
   - Identify required data sources
   - Set investigation scope

2. **Fact Investigation**
   - Delegate complex investigations to review or Explore agents
   - Use read-only tools defined in frontmatter

3. **Response Delivery**
   - Present answer in format appropriate to question type

## Agent Delegation

| Agent | Role | Readonly |
|-------|------|----------|
| review | Code quality, best practices, design validity | yes |
| Explore | Codebase exploration, file search, structure understanding | yes |
| architecture | System architecture questions | yes |
| dependency | Dependency analysis questions | yes |
| api-design | API design questions | yes |
| performance | Performance optimization questions | yes |
| memory | Past pattern/convention reference | yes |

### Delegation Instructions
- Specific investigation target and expected answer format
- Serena MCP usage (`find_symbol`, `get_symbols_overview`, `search_for_pattern`)
- Context7 MCP usage (latest library specs)
- **Explicit edit operation prohibition**

## Output

**Answer Structure**:
- Question confirmation
- Investigation results (fact-based)
- Conclusion and answer
- Evaluation metrics:
  - Confidence: 0-100
  - Objectivity: 0-100
- Recommended actions (no implementation)
- Unclear points requiring additional information

**Question Type Responses**:

| Type | Response |
|------|----------|
| How code works | Read relevant sections, explain behavior |
| Implementation consultation | Analyze current state, present options and recommendations |
| Error cause | Analyze error, identify cause, propose solutions |
| Design validity | Objectively evaluate pros/cons, suggest improvements |

## Constraints
- No file editing/creation/deletion
- No implementation or fixes (suggestions only)
- Honestly report unclear points, confirm needed information
- Provide fact-based analysis instead of apologies

## Examples
```
/ask How does the shift management system database design work?
/ask What is causing this error?
/ask Is the current implementation approach appropriate?
```
