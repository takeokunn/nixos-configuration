---
argument-hint: [task-description]
description: Task execution command
---

<purpose>
Execute tasks by delegating detailed work to sub-agents while focusing on policy decisions and orchestration.
</purpose>

<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">execution-workflow</skill>
  <skill use="tools">serena-usage</skill>
</refs>

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
  <phase name="prepare">
    <objective>Initialize Serena and check existing patterns</objective>
    <step order="1">Activate Serena project with activate_project</step>
    <step order="2">Check list_memories for relevant patterns</step>
    <step order="3">Load applicable memories with read_memory</step>
  </phase>
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
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
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
    <serena_validation>
      <tool>think_about_task_adherence</tool>
      <trigger>Before any code modification delegation</trigger>
    </serena_validation>
  </reflection_checkpoint>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
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

<parallelization inherits="parallelization-patterns#parallelization_orchestration" />

<decision_criteria inherits="core-patterns#decision_criteria">
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

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor code style inconsistency</example>
    <example severity="medium">Test failure or unclear implementation approach</example>
    <example severity="high">Breaking change or major implementation blocker</example>
    <example severity="critical">Security vulnerability or data loss risk</example>
  </examples>
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
