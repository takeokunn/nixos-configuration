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
---

# /execute

## Purpose
Execute tasks by delegating detailed work to sub-agents while focusing on policy decisions and requirement definition.

## Workflow
1. **Task Analysis**: Understand requirements and identify scope
2. **Task Breakdown**: Split large tasks into manageable units
3. **Dependency Organization**: Identify parallel vs sequential tasks
4. **Agent Delegation**: Assign tasks with detailed instructions, execute independent tasks in parallel
5. **Result Integration**: Verify and integrate sub-agent outputs, provide additional instructions as needed

## Sub-agent Instructions

Each delegation must include:
- Specific work scope and expected deliverables
- Target file paths
- Serena MCP usage instructions (`find_symbol`, `get_symbols_overview`, `search_for_pattern`)
- Context7 MCP usage instructions (verify latest library specs)
- Reference to existing implementations (with specific paths)
- Memory check instructions (`list_memories` for patterns/conventions)

## Codex MCP vs Custom Agents

### Codex MCP (Code Generation Only)

**Allowed**:
- Code generation (new files/functions)
- Code modification (editing/refactoring)

**Prohibited** (use custom agents or basic tools):
- Research/analysis → Explore agent, Serena MCP
- Quality verification → quality agent
- Security verification → security agent
- Test creation → test agent
- Documentation → docs agent
- Code review → review agent

**Execution Principles**:
- Prefer basic tools (Read/Edit/Write/Grep/Glob)
- Minimal granularity (one clear, small task per call)
- Staged execution (separate research, design, implementation phases)
- No multi-file edits (split into separate calls)
- Avoid on timeout risk

### Custom Agents

| Task Type | Agent |
|-----------|-------|
| Code quality verification | quality |
| Security verification | security |
| Test creation/coverage | test |
| Refactoring | refactor |
| Documentation | docs |
| Code review | review |
| Bug investigation | debug |
| Codebase exploration | Explore |

## Agent Delegation

### Quality Assurance Phase
- **quality**: Syntax, type, format verification
- **security**: Vulnerability detection

### Implementation Phase
- **test**: Test creation, coverage improvement
- **refactor**: Refactoring, technical debt resolution
- **docs**: Documentation updates

### Review Phase
- **review**: Post-implementation code review

### Parallel Execution
Execute independent tasks simultaneously:
- quality + security: Concurrent quality and security checks
- test + docs: Simultaneous test creation and documentation

## Output
Task execution results with verification status and any identified issues.

## Constraints
- Focus on assigned tasks only
- Avoid unnecessary comments suggesting past implementations
- Maximize sub-agent utilization for detailed work
- Parent agent focuses on orchestration and policy decisions
