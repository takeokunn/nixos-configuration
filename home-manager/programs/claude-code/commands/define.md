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

# /define

## Purpose
Conduct detailed requirements definition before implementation, clarifying technical constraints, design policies, and specifications.

## Principles
- **Fact-based**: Technical evidence over speculation
- **Read-only**: No file modifications
- **Objective judgment**: Define from scratch without justifying user requests
- **Non-compliance**: Prioritize technical validity
- **Information gathering first**: Investigate and question before concluding
- **Comprehensive questioning**: Ask all necessary design questions without limit

## Workflow

1. **Current State Investigation**
   - Phase 1 (Structure): Glob/LS for project structure, Serena `get_symbols_overview` for file symbols
   - Phase 2 (Related implementations): Grep/Serena `find_symbol` for keywords, `find_referencing_symbols` for dependencies
   - Phase 3 (Details): Read/Serena `find_symbol(include_body=True)` for code, Context7 for latest library APIs/best practices
   - Organize: Existing patterns, technical constraints, design decision points, unclear specs

2. **User Interview**
   - List question candidates, score by evaluation axes (design branching, irreversibility, investigation impossibility, implementation effort impact: 1-5 each)
   - Classify: Spec confirmation, design choice, constraint verification, scope confirmation, priority confirmation
   - Present high-score questions first (no limit), provide answer templates
   - Do not proceed without clear answers

3. **Re-investigation Based on Answers**
   - Verify constraints revealed by answers
   - Detailed check of existing implementations related to chosen approach
   - Collect additional technical information

4. **Requirements Document Creation**

5. **Implementation Task Breakdown** (for execute handoff)

## Agent Delegation

| Agent | Role | Readonly |
|-------|------|----------|
| requirement | Ambiguity detection, use case extraction, acceptance criteria | yes |
| design | Architecture consistency, dependency analysis | yes |
| architecture | System architecture design and evaluation | yes |
| api-design | API design verification | yes |
| database | Database design and optimization | yes |
| estimation | Task estimation and planning | yes |
| dependency | Dependency analysis | yes |
| memory | Pattern/convention/architecture decision reference | yes |

### Delegation Instructions
1. Requirements definition scope overview
2. Target file paths/directories
3. Serena MCP usage (`get_symbols_overview`, `find_symbol`, `find_referencing_symbols`, `list_memories`, `read_memory`)
4. Context7 MCP usage (latest library API verification)
5. **Explicit edit operation prohibition**

### Delegation Flow
1. **Information Gathering**: requirement agent investigates code/docs
2. **Requirements Analysis**: requirement agent detects ambiguities, classifies requirements
3. **Design Verification**: design agent verifies architecture consistency
4. **Integration**: Parent agent creates requirements document

## Output

**Requirements Document**:
- Summary: One-sentence request, background/motivation, expected outcomes
- Current State Analysis: Existing system investigation results, tech stack
- Functional Requirements: Mandatory (FR-001 format), optional
- Non-functional Requirements: Performance, security, maintainability
- Technical Specifications: Design policies, impact scope, technical decisions
- Evaluation Metrics:
  - Feasibility: 0-100
  - Objectivity: 0-100
- Constraints: Technical, operational
- Test Requirements: Unit, integration, acceptance criteria
- Outstanding Issues/Confirmations

**Implementation Task Breakdown**:
- Dependency graph
- Phased tasks (target files, overview, dependencies)
- Handoff to execute (technical decisions, reference implementations, constraints)

## Constraints
- No file editing/creation/deletion
- No implementation (requirements only)
- Clearly identify technically impossible requests
- Do not justify user requests
- Prioritize technical validity
- Actively ask questions (no limit)
