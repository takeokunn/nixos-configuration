---
name: requirements-definition
description: "Use when asked to 'define requirements', 'create specification', 'clarify requirements', 'write requirements document', or perform requirement analysis. Provides structured methodology for requirements definition with investigation, scoring, and specification patterns."
---

Structured methodology for requirements definition, ensuring comprehensive specification before implementation. The agent should investigate the current system state, score clarification questions, and produce verifiable requirements.

## Investigation Workflow

When the current system state is unclear, the agent should gather context before defining requirements:

1. **Directory structure**: Analyze project layout using Glob
2. **Symbol analysis**: Examine code structure using `get_symbols_overview`
3. **Keyword search**: Find relevant patterns using Grep or `find_symbol`
4. **Dependency mapping**: Trace relationships using `find_referencing_symbols`
5. **Content details**: Read specific files for deep understanding
6. **API documentation**: Fetch latest docs using Context7

## Question Scoring

Score each unclear requirement question on four criteria (1-5 each) to prioritize clarifications:

| Criterion | Description |
|-----------|-------------|
| Design branching | How much does the answer affect design direction? |
| Irreversibility | How difficult to change after implementation? |
| Investigation impossibility | Cannot be determined through code investigation alone? |
| Effort impact | How much does it affect implementation effort? |

**Example:**

```
Question: "Should we use TypeScript's strict mode?"
- Design Branching: 5 (affects all type decisions)
- Irreversibility: 4 (hard to change later)
- Investigation Impossibility: 3 (requires policy decision)
- Effort Impact: 4 (affects development effort)
Total: 16 (critical - must answer before proceeding)
```

The agent should present high-score questions first. Questions scoring >= 15 are critical and must be answered before proceeding.

## Question Classification

| Type | Description | Example |
|------|-------------|---------|
| Spec confirmation | Confirming existing behavior | "Does the API return null or empty array for no results?" |
| Design choice | Choosing between alternatives | "Should we use REST or GraphQL?" |
| Constraint | Technical/business limitations | "Must support IE11 browsers?" |
| Scope | Implementation boundaries | "Should admin features be in v1?" |
| Priority | Feature ordering | "Which feature should be implemented first?" |

## Requirements Specification

### Functional Requirements Format

```
FR-001: User Authentication
Priority: mandatory
- Users must be able to log in with email and password
- Session must expire after 24 hours of inactivity
- Failed login attempts must be rate-limited (max 5 per hour)
```

### Non-Functional Requirements

```
Performance:
- API response time < 200ms for 95th percentile
- Support 1000 concurrent users

Security:
- All data encrypted at rest using AES-256
- JWT tokens for authentication

Maintainability:
- Test coverage >= 80%
- Documentation for all public APIs
```

### Technical Specifications

```
Design Decision: Use React Query for data fetching
Rationale:
- Built-in caching reduces API calls
- Automatic background refetching
- TypeScript support
Impact Scope:
- All components making API calls
- Testing utilities need to mock React Query
```

## Quality Metrics

| Metric | Score Range | Meaning |
|--------|------------|---------|
| Feasibility | 90-100 | Straightforward with existing tools |
| | 70-89 | Requires some research or new libraries |
| | 50-69 | Significant technical challenges |
| | <50 | May need architecture changes |
| Objectivity | 90-100 | All requirements verified through investigation |
| | 70-89 | Most verified, some assumptions documented |
| | <50 | Mostly assumptions, needs more investigation |

## Output Structure

1. **Summary**: One-sentence description, background, expected outcomes
2. **Current state**: Existing system, tech stack, relevant patterns
3. **Functional requirements**: FR-XXX format with priority and acceptance criteria
4. **Non-functional requirements**: Performance, security, maintainability targets
5. **Technical specifications**: Design decisions with rationale and impact
6. **Metrics**: Feasibility and objectivity scores
7. **Constraints**: Technical and operational limitations
8. **Test requirements**: Unit, integration, and acceptance criteria
9. **Task breakdown**: Dependency graph, phased tasks, handoff notes

## Critical Rules

- The agent should never proceed without clear answers to critical questions (score >= 15)
- All requirements must be verifiable and measurable
- The agent should distinguish mandatory from optional requirements with explicit rationale
- The agent should document assumptions explicitly when requirements are unclear
- The agent should map each requirement to specific test scenarios

## Anti-Patterns

- **Vague requirements**: Write specific, testable requirements with clear acceptance criteria
- **Implementation details in requirements**: Describe what needs to be achieved, not how
- **Skipping investigation**: Always investigate current state before defining requirements
- **Missing constraints**: Explicitly document all technical and operational constraints
- **Undefined priorities**: Clearly mark requirements as mandatory or optional with rationale

## Error Escalation

| Severity | Example | Action |
|----------|---------|--------|
| Low | Minor ambiguity in non-critical detail | Note in report, proceed |
| Medium | Unclear requirement affecting scope | Document, ask user for clarification |
| High | Technically infeasible requirement | Stop, present options to user |
| Critical | Requirement violates security or ethics | Block operation, require user acknowledgment |
