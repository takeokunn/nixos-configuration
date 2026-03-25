---
name: core-patterns
description: "Use when creating agents, commands, or shared workflows that need standardized error escalation, decision criteria with confidence scoring, or enforcement behavior patterns. Provides base templates referenced across all agents and commands to avoid duplication."
---

Standardized base templates for error handling (4-level severity), decision criteria (weighted confidence scoring with boundary tests), and enforcement behaviors (mandatory/prohibited) shared across all agents and commands.

## Severity Levels

Standard 4-level severity classification for error escalation:

| Level | Description | Action |
|-------|-------------|--------|
| low | Minor issues | Note in report, proceed |
| medium | Unclear situations | Document issue, ask user for clarification |
| high | Breaking changes or blockers | STOP, present options to user |
| critical | Security or data loss risks | BLOCK operation, require explicit acknowledgment |

## Error Escalation Template

The agent should include this 4-level structure in every agent or command, customizing only the examples:

```xml
<error_escalation>
  <level severity="low">
    <example>Minor issue description</example>
    <action>Note in report, proceed</action>
  </level>
  <level severity="medium">
    <example>Unclear or ambiguous situation</example>
    <action>Document issue, use AskUserQuestion for clarification</action>
  </level>
  <level severity="high">
    <example>Breaking change or blocker</example>
    <action>STOP, present options to user</action>
  </level>
  <level severity="critical">
    <example>Security risk or data loss</example>
    <action>BLOCK operation, require explicit user acknowledgment</action>
  </level>
</error_escalation>
```

## Decision Criteria Template

### Confidence Thresholds

- **success**: confidence >= 80
- **warning**: confidence 60-79
- **error**: confidence < 60

### Weight Distributions

Weights must always sum to 1.0:

- 3-factor equal: 0.33, 0.34, 0.33
- 3-factor weighted: 0.4, 0.3, 0.3
- 2-factor equal: 0.5, 0.5
- 2-factor weighted: 0.6, 0.4

### Required Validation Tests

All 5 tests must be included when defining decision criteria:

| Test | Input Example (0.4/0.3/0.3) | Result | Status |
|------|-----|--------|--------|
| success_case | 95, 90, 95 | 93.5 | success |
| boundary_success_80 | 85, 75, 80 | 80.5 | success |
| boundary_warning_79 | 80, 75, 80 | 78.5 | warning |
| boundary_error_59 | 60, 55, 60 | 58.5 | error |
| error_case | 50, 55, 45 | 50.0 | error |

## Enforcement Template

### Behavior ID Format

Format: `PREFIX-TYPE-NUMBER` where PREFIX is the agent/command abbreviation (e.g., EXEC, DEF), TYPE is B (mandatory) or P (prohibited), and NUMBER is sequential (001, 002, ...).

```xml
<enforcement>
  <mandatory_behaviors>
    <behavior id="PREFIX-B001" priority="critical">
      <trigger>When condition occurs</trigger>
      <action>Required action</action>
      <verification>How to verify compliance</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="PREFIX-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Prohibited action description</action>
      <response>What to do instead</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
```

## Skill Reference Syntax

Use the `refs` tag to reference skills from agents and commands:

```xml
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="domain">nix-ecosystem</skill>
</refs>
```

Use attribute values: `patterns` (shared templates), `tools` (tool-specific usage), `workflow` (how-to guides), `domain` (domain knowledge).

## Anti-Patterns to Avoid

- **Inline error escalation** -- reference core-patterns, customize only examples
- **Inconsistent thresholds** -- always use 60/80 boundaries
- **Missing boundary tests** -- always include all 5 validation tests (boundary_success_80, boundary_warning_79, boundary_error_59)
- **Weight sum mismatch** -- verify weights sum to exactly 1.0
- **Inconsistent behavior IDs** -- use PREFIX-TYPE-NUMBER format consistently

## Critical Rules

- Always include all 5 validation tests for decision criteria
- Boundary tests must use exact threshold values (80, 79.x, 59.x)
- Error escalation must have exactly 4 severity levels
- Weights in decision criteria must sum to 1.0
- Use refs tag to reference this skill from all agents and commands
