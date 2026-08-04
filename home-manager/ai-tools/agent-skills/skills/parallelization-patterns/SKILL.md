---
name: Parallelization Patterns
description: Patterns for parallel execution strategies, timeout configuration, retry policies, consensus mechanisms, and data-parallel work scheduling.
version: 2.2.0
---

<purpose>
  Provide standardized patterns for parallel agent execution, timeout configuration, retry policies, and consensus mechanisms shared across orchestrators and agents.
</purpose>

<tools>
  <tool name="parallelization_template">
    <description>Standard parallel execution capability and strategy patterns</description>
    <use_case>Include in agents and commands for consistent parallel execution</use_case>
  </tool>
</tools>

<concepts>
  <concept name="parallel_safety">
    <description>Classification of agents by their parallel execution safety</description>
    <example>
      read_only: Safe to run with any other agent (explore, validator)
      analysis: Safe with other analysis agents (design, database, performance)
      execution: May modify local state, requires coordination (security, test, devops)
      orchestration: Manages sub-agents (commands, CLAUDE.md)
    </example>
  </concept>

  <concept name="timeout_tiers">
    <description>Standard timeout values by agent type</description>
    <example>
      Fast read-only (explore): 180000ms (3 min)
      Medium analysis (design, database, quality-assurance): 240000ms (4 min)
      Deep analysis (security, performance, test): 300000ms (5 min)
      Complex operations (code-quality, devops, git, docs): 300000ms (5 min)
      Orchestration (commands, CLAUDE.md): 300000ms (5 min)
    </example>
  </concept>
</concepts>

<patterns>
  <pattern name="parallelization_readonly">
    <description>Parallelization pattern for read-only agents (explore, validator)</description>
    <example>
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
  <safe_with>
    <agent>all read-only agents</agent>
  </safe_with>
  <conflicts_with />
</parallelization>
    </example>
  </pattern>

  <pattern name="parallelization_analysis">
    <description>Parallelization pattern for analysis agents (design, database, code-quality, performance)</description>
    <example>
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
  <safe_with>
    <agent>other analysis agents</agent>
  </safe_with>
  <conflicts_with />
</parallelization>
    </example>
  </pattern>

  <pattern name="parallelization_execution">
    <description>Parallelization pattern for execution agents (security, test, devops)</description>
    <example>
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
    </example>
  </pattern>

  <pattern name="parallelization_orchestration">
    <description>Parallelization pattern for orchestrators (CLAUDE.md, commands)</description>
    <example>
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
    </example>
  </pattern>

  <pattern name="retry_policy">
    <description>Standard retry policy for agent execution failures</description>
    <example>
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
    </example>
  </pattern>

  <pattern name="consensus_thresholds">
    <description>Standard consensus thresholds for multi-agent validation</description>
    <example>
<consensus_thresholds>
  <threshold level="high" value="0.9" action="Auto-accept without review"/>
  <threshold level="medium" value="0.7" action="Accept with note"/>
  <threshold level="low" value="0.5" action="Flag for user review"/>
  <threshold level="conflict" value="0.5" action="Block, require user decision"/>
</consensus_thresholds>
    </example>
  </pattern>

  <pattern name="agent_weights">
    <description>Standard agent weights for consensus mechanism</description>
    <example>
<weights>
  <agent name="explore" weight="1.0"/>
  <agent name="design" weight="1.2"/>
  <agent name="database" weight="1.2"/>
  <agent name="performance" weight="1.2"/>
  <agent name="code-quality" weight="1.1"/>
  <agent name="security" weight="1.5"/>
  <agent name="test" weight="1.1"/>
  <agent name="docs" weight="1.0"/>
  <agent name="quality-assurance" weight="1.3"/>
  <agent name="devops" weight="1.1"/>
  <agent name="validator" weight="2.0"/>
</weights>
    </example>
  </pattern>

  <pattern name="work_scheduling_on_skewed_inputs">
    <description>Scheduling a data-parallel workload across workers, as opposed to the agent-level patterns above. Static contiguous chunking assumes the work per item is roughly uniform; when it is skewed, one oversized item strands most workers idle while a single worker finishes it.</description>
    <strategy>
      <step order="1">Sort the work units by descending estimated size, so the longest job starts first and the short ones backfill around it (longest-processing-time-first).</step>
      <step order="2">Hand out units through a shared atomic cursor rather than pre-assigning ranges. Each worker claims the next index when it becomes free, so a slow unit delays only its own worker.</step>
      <step order="3">Have each worker write its result into the slot for the unit's original index, claimed at the same time as the work.</step>
      <step order="4">Read results back by index after all workers join — never by claim order or completion order.</step>
    </strategy>
    <ordering_caveat>Step 3 and step 4 are the part implementations get wrong. Deterministic output ordering was a free, accidental property of contiguous chunking, where a chunk's slot range equalled its input position. Size-descending claiming destroys that correspondence, so output order has to be re-established deliberately through pre-claimed index slots. Skipping this produces output whose order varies run to run — a change that looks unrelated to scheduling and is easy to misdiagnose.</ordering_caveat>
    <expected_profile>On a size-skewed workload the change is a large win, and on an even workload it is neutral rather than a regression, because claiming overhead is small relative to per-unit work. That makes it a strict improvement rather than a tradeoff — but verify the neutral case rather than assuming it, and confirm the output is byte-identical either way. See performance-benchmarking for the measured numbers and for how to measure both arms defensibly.</expected_profile>
    <when_not_to_use>Work units of genuinely uniform cost, or units so small that the atomic claim dominates the work itself.</when_not_to_use>
  </pattern>

  <decision_tree name="timeout_selection">
    <question>What type of agent is this?</question>
    <branch condition="Fast read-only (explore)">180000ms</branch>
    <branch condition="Medium analysis (design, database, quality-assurance)">240000ms</branch>
    <branch condition="Deep analysis (security, performance, test)">300000ms</branch>
    <branch condition="Complex operations (code-quality, devops, git, docs)">300000ms</branch>
    <branch condition="Orchestration (commands, CLAUDE.md)">300000ms</branch>
  </decision_tree>

  <decision_tree name="parallelization_selection">
    <question>What does this agent modify?</question>
    <branch condition="Nothing (read-only)">Use parallelization_readonly</branch>
    <branch condition="Analysis only">Use parallelization_analysis</branch>
    <branch condition="Local state">Use parallelization_execution</branch>
    <branch condition="Orchestrates sub-agents">Use parallelization_orchestration</branch>
  </decision_tree>
</patterns>

<best_practices>
  <practice priority="critical">Select appropriate parallelization pattern based on agent capabilities</practice>
  <practice priority="critical">Select timeout based on agent type following decision_tree</practice>
  <practice priority="high">Group independent agents in parallel_groups for concurrent execution</practice>
  <practice priority="high">Define depends_on for validation agents that require prior results</practice>
  <practice priority="medium">Use retry_policy for critical operations</practice>
  <practice priority="medium">For skewed data-parallel work, claim units size-descending through a shared cursor and re-establish output order explicitly through pre-claimed index slots (work_scheduling_on_skewed_inputs)</practice>
</best_practices>

<rules priority="critical">
  <rule>Read-only agents must use parallelization_readonly or parallelization_analysis</rule>
  <rule>Agents that modify state must use parallelization_execution</rule>
  <rule>Orchestrators must use parallelization_orchestration</rule>
</rules>

<rules priority="standard">
  <rule>Use timeout_selection decision tree for consistent timeout values</rule>
  <rule>Apply retry_policy for operations that may fail transiently</rule>
  <rule>Use consensus_thresholds for multi-agent validation scenarios</rule>
</rules>

<constraints>
  <must>Match parallelization pattern to agent's state modification behavior</must>
  <must>Use standard timeout tiers from timeout_selection</must>
  <must>Define parallel_groups with correct dependency chains</must>
  <avoid>Running write agents in parallel without coordination</avoid>
  <avoid>Using non-standard timeout values</avoid>
</constraints>

<related_skills>
  <skill name="core-patterns">Base templates for error escalation, decision criteria, enforcement</skill>
  <skill name="workflow-patterns">Output formats, reflection checkpoints, self-evaluation</skill>
  <skill name="performance-benchmarking">Source of the measured speedup pair behind work_scheduling_on_skewed_inputs (4.80x median on a size-skewed workload, 1.004x on an even one) and the protocol for measuring both arms before calling a scheduling change a strict improvement</skill>
</related_skills>

<related_agents>
  <agent name="validator">Validate that parallel task results are consistent and non-conflicting</agent>
</related_agents>
