---
name: Requirements Definition
description: This skill should be used when the user asks to "define requirements", "create specification", "clarify requirements", "write requirements document", or mentions requirement analysis. Provides comprehensive requirements definition methodology.
version: 0.1.0
---

<purpose>
Provide structured methodology for requirements definition, ensuring comprehensive specification before implementation.
</purpose>

<workflow>
<phase name="investigate">
<step tool="Glob/LS">Directory structure analysis</step>
<step tool="get_symbols_overview">Symbol analysis via Serena</step>
<step tool="Grep/find_symbol">Keyword search</step>
<step tool="find_referencing_symbols">Dependency mapping</step>
<step tool="Read">Specific content details</step>
<step tool="Context7">Latest API documentation and best practices</step>
</phase>

<phase name="question_scoring">
<description>Score each question by these criteria (1-5 each)</description>
<criterion name="design_branching">How much does the answer affect design direction?</criterion>
<criterion name="irreversibility">How difficult to change after implementation?</criterion>
<criterion name="investigation_impossibility">Cannot be determined through code investigation alone?</criterion>
<criterion name="effort_impact">How much does it affect implementation effort?</criterion>
<note>Present high-score questions first. Do not proceed without clear answers to critical questions (score >= 15)</note>
</phase>

<phase name="question_classification">
<type name="spec_confirmation">Confirming existing behavior or constraints</type>
<type name="design_choice">Choosing between valid alternatives</type>
<type name="constraint">Technical or business limitations</type>
<type name="scope">Boundaries of implementation</type>
<type name="priority">Order and importance of features</type>
</phase>
</workflow>

<output>
<format>
## Summary
- One-sentence request description
- Background and context
- Expected outcomes

## Current State
- Existing system description
- Technology stack
- Relevant patterns and conventions

## Functional Requirements
Format: FR-XXX (FR-001, FR-002, ...)
- Priority: mandatory or optional
- Clear acceptance criteria

## Non-Functional Requirements
- Performance: response time, throughput
- Security: authentication, authorization, data protection
- Maintainability: code quality, documentation

## Technical Specifications
- Design policies and patterns
- Impact scope analysis
- Key design decisions with rationale

## Metrics
- Feasibility (0-100): Technical achievability
- Objectivity (0-100): Evidence-based specification

## Constraints
- Technical constraints: platform, language, framework
- Operational constraints: deployment, maintenance

## Test Requirements
- Unit test coverage expectations
- Integration test scenarios
- Acceptance criteria verification

## Task Breakdown
- Dependency graph: task dependencies and execution order
- Phased tasks: files to modify/create, overview of changes, dependencies
- Execute handoff: key decisions, reference implementations, constraints
</format>
</output>

