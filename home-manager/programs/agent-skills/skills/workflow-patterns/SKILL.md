---
name: workflow-patterns
description: "Use when defining agent output formats, reflection checkpoints, self-evaluation phases, or Serena initialization patterns. Provides standardized workflow templates shared across agents and commands."
---

Standardized patterns for output formatting, workflow checkpoints, agent references, and self-evaluation shared across agents and commands. The agent should use these templates to ensure consistent structure in all agent workflows.

## Critical Rules

- Output status must use standard criteria: success >= 80 confidence, warning 60-79, error < 60
- Reflection checkpoints must include a confidence threshold
- Commands must include prepare_phase for Serena initialization
- Call `think_about_collected_information` after any search sequence of 3+ operations
- Call `think_about_task_adherence` before any symbol editing or file modification
- Call `think_about_whether_you_are_done` before returning final result to user

## Output Format

The agent should use this standard output format for all agents returning structured results:

```json
{
  "status": "success|warning|error",
  "status_criteria": {
    "success": "All checks passed, confidence >= 80",
    "warning": "Minor issues OR confidence 60-79",
    "error": "Critical issues OR confidence < 60"
  },
  "confidence": 0,
  "summary": "Brief summary of results",
  "metrics": {},
  "findings": [],
  "next_actions": []
}
```

## Reflection Checkpoint

Include at key workflow decision points to verify analysis quality:

```xml
<reflection_checkpoint id="analysis_quality">
  <question>Have I gathered sufficient evidence to proceed?</question>
  <question>Are there gaps in my understanding?</question>
  <threshold>If confidence < 70, seek more evidence or ask user</threshold>
</reflection_checkpoint>
```

## Prepare Phase (Serena Initialization)

Every command workflow should begin with:

1. Activate Serena project with `activate_project`
2. Check `list_memories` for relevant patterns
3. Load applicable memories with `read_memory`

## Serena Validation Triggers

| Trigger | Tool | When |
|---------|------|------|
| After investigation (3+ search ops) | `think_about_collected_information` | After find_symbol, search_for_pattern, Grep, Glob sequences |
| Before code modification | `think_about_task_adherence` | Before replace_symbol_body, insert_before/after_symbol, rename_symbol, file edits |
| Before final response | `think_about_whether_you_are_done` | Before returning result to user |

### On Failure

- **Incomplete search:** Expand search scope, use alternative strategies, or ask user for clarification
- **Task deviation:** Review requirements, document deviation rationale, or ask user before proceeding
- **Incomplete work:** Identify remaining items, iterate on missing work, or report partial completion

## Reflection Workflow Steps

The agent should include these as mandatory steps in workflows:

```xml
<!-- After search/investigation sequence (3+ operations) -->
<step order="N">
  <action>Validate search completeness</action>
  <tool>Serena think_about_collected_information</tool>
  <mandatory>true</mandatory>
</step>

<!-- Before any code modification -->
<step order="N">
  <action>Validate task adherence</action>
  <tool>Serena think_about_task_adherence</tool>
  <mandatory>true</mandatory>
</step>

<!-- Before final response -->
<step order="N">
  <action>Validate task completion</action>
  <tool>Serena think_about_whether_you_are_done</tool>
  <mandatory>true</mandatory>
</step>
```

## Failure Handling Phase

Include in all complex workflows:

1. If tool call fails — log error, attempt alternative approach
2. If data unavailable — document gap, proceed with partial analysis
3. If contradictory evidence — flag uncertainty, request user clarification

## Agent References

Use `ref` attribute to reference agents defined in the `agents/` directory:

```xml
<agents>
  <agent ref="explore" readonly="true" />
  <agent ref="design" readonly="true" />
</agents>
```

The `readonly` attribute indicates whether the agent can modify files.

## Self-Evaluation Phase

For agents producing reports or recommendations:

1. Calculate confidence using `decision_criteria` factors
2. Identify top 1-2 critical issues if confidence below 80
3. Append self-feedback section to output

```xml
<self_feedback>
  <confidence>XX/100 (based on decision_criteria calculation)</confidence>
  <issues>
    - [Critical] Issue description (if any, max 2 total)
    - [Warning] Issue description (if any)
  </issues>
</self_feedback>
```

## Best Practices

- Use `output_format` for all agents that return structured results
- Include `reflection_checkpoint` at key workflow decision points
- Add `self_evaluate_phase` for agents producing reports
- Use `failure_handling` phase in all complex workflows
- Use `agent_ref` syntax for consistent agent references

## Constraints

- **Must:** Use standard status criteria thresholds, include confidence scores, define thresholds in checkpoints
- **Must:** Include prepare_phase and serena_validation in command workflows
- **Avoid:** Custom status thresholds, omitting failure_handling, skipping mandatory reflection tool calls
