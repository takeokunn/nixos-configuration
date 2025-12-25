---
description: Review command for Claude Code's recent work
modes:
  - name: define
    description: Comprehensive review of /define execution plan
    agents:
      - name: plan
        description: Execution plan review
      - name: estimation
        description: Estimation validity review
  - name: execute
    description: Comprehensive review of /execute work
    agents:
      - name: quality
        description: Code quality review
      - name: security
        description: Security review
      - name: design
        description: Architecture consistency review
      - name: docs
        description: Documentation quality review
      - name: performance
        description: Performance review
      - name: test
        description: Test coverage review
      - name: accessibility
        description: Accessibility review
      - name: error-handling
        description: Error handling review
  - name: general
    description: Review of Claude Code's recent work
    agents:
      - name: review
        description: Comprehensive work review
      - name: complexity
        description: Code complexity review
      - name: memory
        description: Consistency check with existing patterns
readonly_tools:
  - name: serena
    description: Symbol analysis, memory verification, dependency tracking
  - name: context7
    description: Library best practices verification
---

# /feedback

## Purpose
Multi-faceted review of Claude Code's work within the same session, automatically selecting appropriate review mode based on previous command and executing efficiently in parallel.

## Mode Selection

| Condition | Mode | Description |
|-----------|------|-------------|
| `/define` executed | define | Comprehensive execution plan feedback |
| `/execute` executed | execute | Comprehensive work content feedback |
| Other | general | Feedback on recent Claude Code work |

## Execution

### Parallel Execution (Critical for Timeout Avoidance)
**Important**: Launch all Task tools **simultaneously in one message**. Never execute sequentially.

### define Mode (1 agent)

**Target**: Extract execution plan (step breakdown) from conversation history

**Review Aspects**:
- Step granularity: Appropriately decomposed
- Dependencies: Proper sequencing/parallelization
- Risk identification: No overlooked technical challenges
- Completeness: All requirements covered
- Feasibility: Each step executable
- Comprehensiveness: No missing necessary work
- Clarity: Each step specific

**Tools**:
- `serena`: `find_symbol`, `get_symbols_overview` for code structure
- `serena`: `find_referencing_symbols` for dependency analysis
- `serena`: `list_memories`, `read_memory` for existing patterns/conventions

### execute Mode (4 agents in parallel)

**Target Identification**:
1. Extract Edit/Write tool target files from conversation history
2. Fallback: Detect uncommitted changes via git status
3. If unclear: Confirm with user

**Important**: Review only changed code, not existing code quality issues

#### a) Code Quality (quality)
**Aspects**: Naming conventions, separation of concerns, DRY principle, readability, unnecessary comments/dead code

**Tools**:
- `serena`: `find_symbol`, `get_symbols_overview` for symbol analysis
- `context7`: Framework best practices

#### b) Security (security)
**Aspects**: OWASP Top 10 (SQLi, XSS, CSRF), input validation, authentication/authorization, sensitive data handling

**Tools**:
- `serena`: `find_referencing_symbols` for auth/authz flow tracking
- `grep`: Hardcoded secret search

#### c) Architecture (design)
**Aspects**: Consistency with existing design, dependency direction, adherence to Serena memory patterns

**Tools**:
- `serena`: `list_memories`, `read_memory` for existing patterns
- `serena`: `find_referencing_symbols` for dependency analysis

#### d) Documentation (docs)
**Aspects**:
- Accuracy/consistency: Code divergence, reference link validity
- Readability/structure: Heading hierarchy, Markdown syntax, format consistency
- Comprehensiveness: Required sections, parameter description completeness

**Tools**:
- `Read`: .md file content verification
- `Grep`: Code consistency verification
- `context7`: Documentation best practices

### general Mode (1 agent)

**Target**: Identify recent Claude Code work from conversation history

**Review Aspects by Work Type**:

| Work Type | Check Aspects |
|-----------|---------------|
| Research/analysis | Investigation comprehensiveness, analysis accuracy, information organization |
| Documentation | Accuracy, readability, comprehensiveness |
| Code generation/modification | Quality, security, consistency |
| Other | Goal achievement, quality, improvements |

**Tools**:
- `serena`: `list_memories`, `read_memory` for existing patterns/conventions
- Appropriate tools based on work type

## Output

```markdown
# 📊 {Mode Name} Feedback Results

## 📈 Evaluation Scores

- {Metric1}: XX/100
- {Metric2}: XX/100
  ...

**Overall Score: XX/100**

## 🔴 Critical (Immediate Fix Required)

- [Category] Issue details: Location
  - **Problem**: Issue description
  - **Fix**: Specific fix proposal

## 🟡 Warning (Fix Recommended)

- [Category] Improvement details: Location
  - **Problem**: Issue description
  - **Recommendation**: Improvement proposal

## 🟢 Good Practice (Good Implementation/Work)

- [Category] Commendable aspects

## 📝 Recommended Actions (Priority Order)

1. [Priority: High] Top improvement
2. [Priority: Medium] Next improvement
3. [Priority: Low] If time permits
```

### Mode-Specific Metrics

**define Mode**:
- Execution plan quality: XX/100

**execute Mode**:
- Code quality: XX/100
- Security: XX/100
- Architecture: XX/100
- Documentation: XX/100

**general Mode**:
- Work quality: XX/100

## Constraints
- **Timeout avoidance**: Launch all Tasks **simultaneously in one message** (no sequential execution)
- **Context detection**: Auto-select mode based on previous command (/define, /execute, other)
- **Specific findings**: Concrete fix proposals, not abstract theories
- **No compliance**: Quality first
- **Serena usage**: Explicitly in prompts
- **Session-based**: Target Claude Code operations within session, not git diff
- **Changed code only**: In execute mode, evaluate only changed code, not existing code quality issues
