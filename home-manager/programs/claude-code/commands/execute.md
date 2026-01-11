---
argument-hint: [task-description]
description: Task execution command
---

<purpose>
Execute tasks by delegating detailed work to sub-agents while focusing on policy decisions and orchestration.
</purpose>

<rules priority="critical">
  <rule>Delegate detailed work to specialized sub-agents</rule>
  <rule>Focus on orchestration and policy decisions</rule>
  <rule>Execute independent tasks in parallel</rule>
  <rule>Verify sub-agent outputs before integration</rule>
</rules>

<rules priority="standard">
  <rule>Use execution-workflow skill for delegation patterns</rule>
  <rule>Prefer basic tools (Read/Edit/Write) over Codex MCP when sufficient</rule>
  <rule>Use Codex MCP only for code generation/modification</rule>
  <rule>Check Serena memories before implementation</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Understand the task scope and identify required resources</objective>
    <step order="1">What tasks need to be done?</step>
    <step order="2">Which sub-agents are best suited?</step>
    <step order="3">Which tasks can run in parallel?</step>
    <step order="4">What dependencies exist between tasks?</step>
    <step order="5">What verification is needed?</step>
  </phase>
  <phase name="decompose">
    <objective>Break down complex tasks into manageable units</objective>
    <step order="1">Split into manageable units</step>
    <step order="2">Identify task boundaries</step>
  </phase>
  <phase name="structure">
    <objective>Organize tasks for optimal execution</objective>
    <step order="1">Identify parallel vs sequential tasks</step>
    <step order="2">Define task dependencies</step>
  </phase>
  <reflection_checkpoint id="analysis_quality">
    <question>Have I gathered sufficient evidence to proceed?</question>
    <question>Are there gaps in my understanding?</question>
    <threshold>If confidence less than 70, seek more evidence or ask user</threshold>
  </reflection_checkpoint>
  <phase name="assign">
    <objective>Delegate tasks to appropriate sub-agents with clear instructions</objective>
    <step order="1">Delegate tasks with detailed instructions</step>
    <step order="2">Provide context and constraints</step>
  </phase>
  <reflection_checkpoint id="assignment_complete" after="assign">
    <questions>
      <question weight="0.4">Have all tasks been properly delegated?</question>
      <question weight="0.3">Are the sub-agent instructions clear?</question>
      <question weight="0.3">Are dependencies between tasks handled?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Refine task assignments or ask user</below_threshold>
    </threshold>
  </reflection_checkpoint>
  <phase name="failure_handling">
    <objective>Handle errors and edge cases gracefully</objective>
    <step order="1">If tool call fails: Log error, attempt alternative approach</step>
    <step order="2">If data unavailable: Document gap, proceed with partial analysis</step>
    <step order="3">If contradictory evidence: Flag uncertainty, request user clarification</step>
  </phase>
  <phase name="consolidate">
    <objective>Integrate sub-agent outputs into cohesive result</objective>
    <step order="1">Verify sub-agent outputs</step>
    <step order="2">Combine results</step>
  </phase>
</workflow>

<agents>
  <agent name="quality" subagent_type="quality-assurance" readonly="false">Syntax, type, format verification</agent>
  <agent name="security" subagent_type="security" readonly="false">Vulnerability detection</agent>
  <agent name="test" subagent_type="test" readonly="false">Test creation, coverage</agent>
  <agent name="refactor" subagent_type="general-purpose" readonly="false">Refactoring, tech debt</agent>
  <agent name="docs" subagent_type="docs" readonly="false">Documentation updates</agent>
  <agent name="review" subagent_type="quality-assurance" readonly="false">Post-implementation review</agent>
  <agent name="debug" subagent_type="general-purpose" readonly="false">Debug support</agent>
  <agent name="performance" subagent_type="performance" readonly="false">Performance optimization</agent>
  <agent name="clean" subagent_type="code-quality" readonly="false">Dead code elimination</agent>
  <agent name="error-handling" subagent_type="general-purpose" readonly="false">Error handling patterns</agent>
  <agent name="migration" subagent_type="general-purpose" readonly="false">Migration planning and execution</agent>
  <agent name="database" subagent_type="database" readonly="false">Database design and optimization</agent>
  <agent name="infrastructure" subagent_type="devops" readonly="false">Infrastructure design</agent>
  <agent name="ci-cd" subagent_type="devops" readonly="false">CI/CD pipeline design</agent>
  <agent name="observability" subagent_type="devops" readonly="false">Logging, monitoring, tracing</agent>
  <agent name="git" subagent_type="git" readonly="false">Git workflow design</agent>
  <agent name="memory" subagent_type="general-purpose" readonly="false">Knowledge base management</agent>
  <agent name="validator" subagent_type="validator" readonly="true">Cross-validation and consensus verification</agent>
</agents>

<execution_graph>
  <parallel_group id="quality_assurance" depends_on="none">
    <agent>quality</agent>
    <agent>security</agent>
  </parallel_group>
  <parallel_group id="implementation" depends_on="none">
    <agent>test</agent>
    <agent>docs</agent>
  </parallel_group>
  <sequential_phase id="review_phase" depends_on="quality_assurance,implementation">
    <agent>review</agent>
    <reason>Requires completion of quality checks and implementation</reason>
  </sequential_phase>
</execution_graph>

<delegation>
  <requirement>Specific scope and expected deliverables</requirement>
  <requirement>Target file paths</requirement>
  <requirement>Reference implementations (specific paths)</requirement>
  <requirement>Memory check: `list_memories` for patterns</requirement>
</delegation>

<codex_usage>
  <allowed>Code generation (new files/functions), code modification (editing/refactoring)</allowed>
  <prohibited>
    <task alternative="Explore agent, Serena MCP">Research/analysis</task>
    <task alternative="quality agent">Quality verification</task>
    <task alternative="security agent">Security verification</task>
    <task alternative="test agent">Test creation</task>
    <task alternative="docs agent">Documentation</task>
    <task alternative="review agent">Code review</task>
  </prohibited>
  <rule>Prefer basic tools when sufficient</rule>
  <rule>One clear, small task per call</rule>
  <rule>Separate phases: research → design → implementation</rule>
  <rule>No multi-file edits in single call</rule>
</codex_usage>

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

<decision_criteria>
  <criterion name="confidence_calculation">
    <factor name="task_clarity" weight="0.3">
      <score range="90-100">Clear requirements with acceptance criteria</score>
      <score range="70-89">Clear requirements</score>
      <score range="50-69">Some ambiguity</score>
      <score range="0-49">Unclear requirements</score>
    </factor>
    <factor name="implementation_quality" weight="0.4">
      <score range="90-100">All tests pass, code reviewed</score>
      <score range="70-89">Tests pass</score>
      <score range="50-69">Some issues remain</score>
      <score range="0-49">Major issues</score>
    </factor>
    <factor name="verification_completeness" weight="0.3">
      <score range="90-100">Full verification by sub-agents</score>
      <score range="70-89">Core verification done</score>
      <score range="50-69">Partial verification</score>
      <score range="0-49">Minimal verification</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="fully_verified">
      <input>task_clarity=95, implementation_quality=90, verification_completeness=95</input>
      <calculation>(95*0.3)+(90*0.4)+(95*0.3) = 28.5+36+28.5 = 93</calculation>
      <expected_status>success</expected_status>
      <reasoning>Clear requirements with tested code and full verification yields high confidence</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>task_clarity=80, implementation_quality=75, verification_completeness=85</input>
      <calculation>(80*0.3)+(75*0.4)+(85*0.3) = 24+30+25.5 = 79.5</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Tests pass but some issues remain results in 79.5, triggers warning</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>task_clarity=85, implementation_quality=75, verification_completeness=85</input>
      <calculation>(85*0.3)+(75*0.4)+(85*0.3) = 25.5+30+25.5 = 81</calculation>
      <expected_status>success</expected_status>
      <reasoning>Weighted average 81 meets success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>task_clarity=65, implementation_quality=55, verification_completeness=60</input>
      <calculation>(65*0.3)+(55*0.4)+(60*0.3) = 19.5+22+18 = 59.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Weighted average 59.5 is below 60, triggers error</reasoning>
    </test>
    <test name="major_issues">
      <input>task_clarity=50, implementation_quality=45, verification_completeness=50</input>
      <calculation>(50*0.3)+(45*0.4)+(50\*0.3) = 15+18+15 = 48</calculation>
      <expected_status>error</expected_status>
      <reasoning>Unclear requirements with major issues results in 48, triggers error</reasoning>
    </test>
  </validation_tests>
</decision_criteria>

<enforcement>
  <mandatory_behaviors>
    <behavior id="EXEC-B001" priority="critical">
      <trigger>Before implementation</trigger>
      <action>Check Serena memories for existing patterns</action>
      <verification>Pattern check in output</verification>
    </behavior>
    <behavior id="EXEC-B002" priority="critical">
      <trigger>After implementation</trigger>
      <action>Delegate verification to quality and security agents</action>
      <verification>Agent reports in output</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="EXEC-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Implementing without sub-agent delegation</action>
      <response>Block operation, delegate to specialized agents</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<error_escalation>
  <level severity="low">
    <example>Minor code style inconsistency</example>
    <action>Note in report, proceed</action>
  </level>
  <level severity="medium">
    <example>Test failure or unclear implementation approach</example>
    <action>Document issue, use AskUserQuestion for clarification</action>
  </level>
  <level severity="high">
    <example>Breaking change or major implementation blocker</example>
    <action>STOP, present options to user</action>
  </level>
  <level severity="critical">
    <example>Security vulnerability or data loss risk</example>
    <action>BLOCK operation, require explicit user acknowledgment</action>
  </level>
</error_escalation>

<related_commands>
  <command name="define">When implementation reveals unclear requirements</command>
  <command name="ask">When implementation requires investigation</command>
  <command name="bug">When implementation encounters unexpected errors</command>
  <command name="feedback">Review work after execution completion</command>
  <command name="upstream">When preparing changes for upstream OSS contribution</command>
</related_commands>

<related_skills>
  <skill name="execution-workflow">Core delegation and orchestration patterns</skill>
  <skill name="serena-usage">Check memories for existing patterns before implementation</skill>
  <skill name="testing-patterns">Ensure proper test coverage</skill>
</related_skills>

<constraints>
  <must>Delegate detailed work to sub-agents</must>
  <must>Execute independent tasks in parallel</must>
  <must>Verify outputs before integration</must>
  <avoid>Implementing detailed logic directly</avoid>
  <avoid>Unnecessary comments about past implementations</avoid>
  <avoid>Multi-file edits in single Codex call</avoid>
</constraints>
