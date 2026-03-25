---
name: parallelization-patterns
description: "Use when configuring 'parallel execution', 'agent timeouts', 'retry policies', 'consensus mechanisms', or 'multi-agent orchestration'. Provides standardized patterns for parallel agent execution strategies, timeout tiers, retry policies, and consensus thresholds."
---

Standardized patterns for parallel agent execution, timeout configuration, retry policies, and consensus mechanisms shared across orchestrators and agents.

## Parallel Safety Classification

Agents are classified by their parallel execution safety:

| Classification | Modifies State | Examples | Safe With |
|---------------|---------------|----------|-----------|
| Read-only | None | explore, validator | All agents |
| Analysis | None | design, database, performance | Other analysis agents |
| Execution | Local | security, test, devops | Requires coordination |
| Orchestration | Orchestration | commands, CLAUDE.md | Manages sub-agents |

## Timeout Tiers

| Agent Type | Timeout | Examples |
|-----------|---------|----------|
| Fast read-only | 180,000ms (3 min) | explore |
| Medium analysis | 240,000ms (4 min) | design, database, quality-assurance |
| Deep analysis | 300,000ms (5 min) | security, performance, test |
| Complex operations | 300,000ms (5 min) | code-quality, devops, git, docs |
| Orchestration | 300,000ms (5 min) | commands, CLAUDE.md |

## Parallelization Patterns

### Read-Only Pattern

For agents that never modify state (explore, validator):

```xml
<parallelization>
  <capability>
    <parallel_safe>true</parallel_safe>
    <read_only>true</read_only>
    <modifies_state>none</modifies_state>
  </capability>
  <execution_strategy>
    <max_parallel_agents>16</max_parallel_agents>
    <timeout_per_agent>180000</timeout_per_agent>
  </execution_strategy>
</parallelization>
```

### Analysis Pattern

For analysis agents (design, database, code-quality, performance):

```xml
<parallelization>
  <capability>
    <parallel_safe>true</parallel_safe>
    <read_only>true</read_only>
    <modifies_state>none</modifies_state>
  </capability>
  <execution_strategy>
    <max_parallel_agents>16</max_parallel_agents>
    <timeout_per_agent>240000</timeout_per_agent>
  </execution_strategy>
</parallelization>
```

### Execution Pattern

For agents that modify local state (security, test, devops):

```xml
<parallelization>
  <capability>
    <parallel_safe>true</parallel_safe>
    <read_only>false</read_only>
    <modifies_state>local</modifies_state>
  </capability>
  <execution_strategy>
    <max_parallel_agents>16</max_parallel_agents>
    <timeout_per_agent>300000</timeout_per_agent>
  </execution_strategy>
</parallelization>
```

### Orchestration Pattern

For orchestrators managing sub-agents, with dependency groups:

```xml
<parallelization>
  <capability>
    <parallel_safe>true</parallel_safe>
    <read_only>false</read_only>
    <modifies_state>orchestration</modifies_state>
  </capability>
  <execution_strategy>
    <max_parallel_agents>16</max_parallel_agents>
    <timeout_per_agent>300000</timeout_per_agent>
    <parallel_groups>
      <group id="investigation" agents="explore,design,database,performance" independent="true"/>
      <group id="quality" agents="code-quality,security,test" independent="true"/>
      <group id="review" agents="quality-assurance,docs" independent="true"/>
      <group id="validation" agents="validator" independent="false" depends_on="investigation,quality,review"/>
    </parallel_groups>
  </execution_strategy>
</parallelization>
```

## Retry Policy

```xml
<retry_policy>
  <max_retries>2</max_retries>
  <retry_conditions>
    <condition>Agent timeout</condition>
    <condition>Partial results returned</condition>
    <condition>Confidence score below 60</condition>
  </retry_conditions>
  <fallback_strategy>
    <action>Use alternative agent from same parallel group</action>
  </fallback_strategy>
</retry_policy>
```

## Consensus Mechanism

### Thresholds

| Level | Value | Action |
|-------|-------|--------|
| High | >= 0.9 | Auto-accept without review |
| Medium | >= 0.7 | Accept with note |
| Low | >= 0.5 | Flag for user review |
| Conflict | < 0.5 | Block, require user decision |

### Agent Weights

| Agent | Weight | Agent | Weight |
|-------|--------|-------|--------|
| explore | 1.0 | code-quality | 1.1 |
| design | 1.2 | security | 1.5 |
| database | 1.2 | test | 1.1 |
| performance | 1.2 | quality-assurance | 1.3 |
| docs | 1.0 | devops | 1.1 |
| validator | 2.0 | | |

## Decision Workflow

1. **Classify the agent**: Determine if it is read-only, analysis, execution, or orchestration
2. **Select timeout**: Match agent type to the appropriate timeout tier
3. **Choose parallelization pattern**: Based on state modification behavior
4. **Configure groups**: For orchestrators, define independent and dependent parallel groups
5. **Set retry policy**: Apply retry conditions for critical operations
6. **Define consensus**: Set thresholds and weights for multi-agent validation

## Critical Rules

- The agent should match parallelization pattern to the agent's state modification behavior
- Read-only agents must use `parallelization_readonly` or `parallelization_analysis`
- Agents that modify state must use `parallelization_execution`
- Orchestrators must use `parallelization_orchestration` with defined dependency chains
- The agent should never run write agents in parallel without coordination
- The agent should always use standard timeout tiers from the decision workflow
