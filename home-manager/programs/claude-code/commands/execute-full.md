---
argument-hint: [task-description]
description: Full task execution with feedback loop
---

<purpose>
Execute tasks with automatic feedback collection and conditional fix phase. Runs execute -> feedback -> fix issues (only if issues found) in a single automated flow.
</purpose>

<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">execution-workflow</skill>
  <skill use="tools">serena-usage</skill>
</refs>

<rules priority="critical">
  <rule>Delegate detailed work to specialized sub-agents</rule>
  <rule>Complete full cycle: execute -> feedback -> fix (conditional)</rule>
  <rule>Maximum one fix iteration (no infinite loops)</rule>
  <rule>Automatic flow between phases (no user confirmation)</rule>
  <rule>Skip fix phase if no issues found in feedback</rule>
</rules>

<rules priority="standard">
  <rule>Use execution-workflow skill for delegation patterns</rule>
  <rule>Check Serena memories before implementation</rule>
  <rule>Fix only issues identified in feedback, not full re-implementation</rule>
</rules>

<workflow>
  <phase name="execute_initial">
    <objective>Execute core workflow phases to complete initial implementation</objective>
    <subphase name="prepare">
      <objective>Initialize Serena and check existing patterns</objective>
      <step number="1">
        <action>Activate Serena project with activate_project</action>
        <tool>Serena activate_project</tool>
        <output>Project activated</output>
      </step>
      <step number="2">
        <action>Check list_memories for relevant patterns</action>
        <tool>Serena list_memories</tool>
        <output>Available memory list</output>
      </step>
      <step number="3">
        <action>Load applicable memories with read_memory</action>
        <tool>Serena read_memory</tool>
        <output>Relevant patterns loaded</output>
      </step>
    </subphase>
    <subphase name="analyze">
      <objective>Understand the task scope and identify required resources</objective>
      <step number="1">
        <action>What tasks need to be done?</action>
        <tool>Task analysis</tool>
        <output>Task list</output>
      </step>
      <step number="2">
        <action>Which sub-agents are best suited?</action>
        <tool>Agent selection</tool>
        <output>Agent assignments</output>
      </step>
      <step number="3">
        <action>Which tasks can run in parallel?</action>
        <tool>Dependency analysis</tool>
        <output>Parallel task groups</output>
      </step>
      <step number="4">
        <action>What dependencies exist between tasks?</action>
        <tool>Dependency mapping</tool>
        <output>Dependency graph</output>
      </step>
      <step number="5">
        <action>What verification is needed?</action>
        <tool>Verification planning</tool>
        <output>Verification checklist</output>
      </step>
    </subphase>
    <subphase name="decompose">
      <objective>Break down complex tasks into manageable units</objective>
      <step number="1">
        <action>Split into manageable units</action>
        <tool>Task decomposition</tool>
        <output>Atomic task list</output>
      </step>
      <step number="2">
        <action>Identify task boundaries</action>
        <tool>Boundary analysis</tool>
        <output>Clear task scopes</output>
      </step>
    </subphase>
    <subphase name="structure">
      <objective>Organize tasks for optimal execution</objective>
      <step number="1">
        <action>Identify parallel vs sequential tasks</action>
        <tool>Execution planning</tool>
        <output>Execution order</output>
      </step>
      <step number="2">
        <action>Define task dependencies</action>
        <tool>Dependency definition</tool>
        <output>Task dependency map</output>
      </step>
    </subphase>
    <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
    <subphase name="assign">
      <objective>Delegate tasks to appropriate sub-agents with clear instructions</objective>
      <step number="1">
        <action>Delegate tasks with detailed instructions</action>
        <tool>Sub-agent delegation</tool>
        <output>Delegated tasks</output>
      </step>
      <step number="2">
        <action>Provide context and constraints</action>
        <tool>Context provision</tool>
        <output>Contextual guidance</output>
      </step>
    </subphase>
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
    <subphase name="consolidate">
      <objective>Integrate sub-agent outputs into cohesive result</objective>
      <step number="1">
        <action>Verify sub-agent outputs</action>
        <tool>Output verification</tool>
        <output>Verification results</output>
      </step>
      <step number="2">
        <action>Combine results</action>
        <tool>Result integration</tool>
        <output>Consolidated implementation</output>
      </step>
    </subphase>
  </phase>

  <phase name="collect_feedback">
    <objective>Launch feedback agents in execute mode and collect evaluation results</objective>
    <step number="1">
      <action>Launch quality agent: syntax, type, format verification</action>
      <tool>Sub-agent delegation (quality-assurance)</tool>
      <aspects>Syntax correctness, type safety, code formatting, style compliance</aspects>
      <output>Quality evaluation report</output>
    </step>
    <step number="2">
      <action>Launch security agent: vulnerability detection</action>
      <tool>Sub-agent delegation (security)</tool>
      <aspects>Security vulnerabilities, input validation, authentication, authorization</aspects>
      <output>Security evaluation report</output>
    </step>
    <step number="3">
      <action>Launch design agent: architecture consistency</action>
      <tool>Sub-agent delegation (design)</tool>
      <aspects>Architecture patterns, dependency management, API design, coupling</aspects>
      <output>Design evaluation report</output>
    </step>
    <step number="4">
      <action>Launch docs agent: documentation completeness</action>
      <tool>Sub-agent delegation (docs)</tool>
      <aspects>Code comments, API documentation, README updates</aspects>
      <output>Documentation evaluation report</output>
    </step>
    <step number="5">
      <action>Launch performance agent: performance implications</action>
      <tool>Sub-agent delegation (performance)</tool>
      <aspects>Algorithm complexity, resource usage, potential bottlenecks</aspects>
      <output>Performance evaluation report</output>
    </step>
    <step number="6">
      <action>Launch test agent: test coverage analysis</action>
      <tool>Sub-agent delegation (test)</tool>
      <aspects>Test coverage, edge cases, test quality</aspects>
      <output>Test evaluation report</output>
    </step>
    <execution_mode>All agents in parallel</execution_mode>
    <reflection_checkpoint id="feedback_quality" after="collect_feedback">
      <questions>
        <question weight="0.4">Did all feedback agents complete successfully?</question>
        <question weight="0.3">Is the feedback specific and actionable?</question>
        <question weight="0.3">Are there critical issues requiring fixes?</question>
      </questions>
      <threshold min="70" action="proceed">
        <below_threshold>Re-run failed agents or gather additional context</below_threshold>
      </threshold>
      <serena_validation>
        <tool>think_about_collected_information</tool>
        <trigger>After feedback collection completes</trigger>
      </serena_validation>
    </reflection_checkpoint>
  </phase>

  <phase name="fix_issues">
    <condition>Execute only if feedback found critical or warning issues</condition>
    <skip_condition>Skip if no issues found in feedback</skip_condition>
    <objective>Fix identified issues from feedback phase</objective>
    <step number="1">
      <action>Synthesize feedback from all agents</action>
      <tool>Feedback synthesis</tool>
      <output>Consolidated issue list</output>
    </step>
    <step number="2">
      <action>Prioritize issues by severity (critical > warning > info)</action>
      <tool>Issue prioritization</tool>
      <output>Prioritized issue list</output>
    </step>
    <step number="3">
      <action>Delegate fixes to appropriate sub-agents</action>
      <tool>Sub-agent delegation</tool>
      <scope>Only issues identified in feedback, not full re-implementation</scope>
      <output>Fix assignments</output>
    </step>
    <step number="4">
      <action>Verify fixes address the identified issues</action>
      <tool>Fix verification</tool>
      <output>Verification results</output>
    </step>
    <step number="5">
      <action>Consolidate fix results</action>
      <tool>Result consolidation</tool>
      <output>Fixed implementation</output>
    </step>
    <iteration_limit>1</iteration_limit>
    <reflection_checkpoint id="fix_complete" after="fix_issues">
      <questions>
        <question weight="0.5">Have all critical issues been addressed?</question>
        <question weight="0.3">Have warning issues been addressed where feasible?</question>
        <question weight="0.2">Are any issues deferred with clear justification?</question>
      </questions>
      <threshold min="70" action="complete">
        <below_threshold>Flag remaining issues for user review</below_threshold>
      </threshold>
      <serena_validation>
        <tool>think_about_whether_you_are_done</tool>
        <trigger>Before final output</trigger>
      </serena_validation>
    </reflection_checkpoint>
  </phase>

  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
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
  <agent name="design" subagent_type="design" readonly="true">Architecture evaluation for feedback</agent>
</agents>

<execution_graph>
  <sequential_phase id="execute" depends_on="none">
    <parallel_group id="quality_assurance">
      <agent>quality</agent>
      <agent>security</agent>
    </parallel_group>
    <parallel_group id="implementation">
      <agent>test</agent>
      <agent>docs</agent>
    </parallel_group>
    <sequential_step id="consolidation">
      <action>Consolidate initial execution results</action>
    </sequential_step>
  </sequential_phase>
  <sequential_phase id="feedback" depends_on="execute">
    <parallel_group id="feedback_agents">
      <agent>quality</agent>
      <agent>security</agent>
      <agent>design</agent>
      <agent>docs</agent>
      <agent>performance</agent>
      <agent>test</agent>
    </parallel_group>
  </sequential_phase>
  <conditional_phase id="fix" depends_on="feedback">
    <condition>issues_found == true</condition>
    <skip_action>Output skip confirmation with no issues message</skip_action>
    <parallel_group id="fix_agents">
      <agent>Agents matching issue categories</agent>
    </parallel_group>
  </conditional_phase>
</execution_graph>

<delegation>
  <requirement>Specific scope and expected deliverables</requirement>
  <requirement>Target file paths</requirement>
  <requirement>Reference implementations (specific paths)</requirement>
  <requirement>Memory check: list_memories for patterns</requirement>
  <requirement>For fix phase: specific issue references from feedback</requirement>
</delegation>

<parallelization inherits="parallelization-patterns#parallelization_orchestration" />

<decision_criteria inherits="core-patterns#decision_criteria">
  <criterion name="confidence_calculation">
    <factor name="task_clarity" weight="0.2">
      <score range="90-100">Clear requirements with acceptance criteria</score>
      <score range="70-89">Clear requirements</score>
      <score range="50-69">Some ambiguity</score>
      <score range="0-49">Unclear requirements</score>
    </factor>
    <factor name="implementation_quality" weight="0.3">
      <score range="90-100">All tests pass, code reviewed</score>
      <score range="70-89">Tests pass</score>
      <score range="50-69">Some issues remain</score>
      <score range="0-49">Major issues</score>
    </factor>
    <factor name="feedback_severity" weight="0.25">
      <score range="90-100">No issues found</score>
      <score range="70-89">Info-level issues only</score>
      <score range="50-69">Warning-level issues</score>
      <score range="0-49">Critical issues</score>
    </factor>
    <factor name="fix_completeness" weight="0.25">
      <score range="90-100">All issues fixed and verified</score>
      <score range="70-89">Critical issues fixed</score>
      <score range="50-69">Some issues remain</score>
      <score range="0-49">Major issues unfixed</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="no_issues_found">
      <input>task_clarity=90, implementation_quality=95, feedback_severity=95, fix_completeness=100</input>
      <calculation>(90*0.2)+(95*0.3)+(95*0.25)+(100*0.25) = 95.25</calculation>
      <expected_status>success</expected_status>
      <expected_fix_phase>skipped</expected_fix_phase>
      <reasoning>No issues found, fix phase skipped, high confidence</reasoning>
    </test>
    <test name="issues_fixed_success">
      <input>task_clarity=85, implementation_quality=80, feedback_severity=60, fix_completeness=90</input>
      <calculation>(85*0.2)+(80*0.3)+(60*0.25)+(90*0.25) = 78.5</calculation>
      <expected_status>success</expected_status>
      <expected_fix_phase>executed</expected_fix_phase>
      <reasoning>Issues found and fixed, acceptable confidence</reasoning>
    </test>
    <test name="boundary_success_70">
      <input>task_clarity=70, implementation_quality=70, feedback_severity=70, fix_completeness=70</input>
      <calculation>(70*0.2)+(70*0.3)+(70*0.25)+(70*0.25) = 70</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly at success threshold</reasoning>
    </test>
    <test name="boundary_warning_69">
      <input>task_clarity=69, implementation_quality=69, feedback_severity=69, fix_completeness=69</input>
      <calculation>(69*0.2)+(69*0.3)+(69*0.25)+(69*0.25) = 69</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Below success threshold, flag for review</reasoning>
    </test>
    <test name="critical_issues_unfixed">
      <input>task_clarity=80, implementation_quality=70, feedback_severity=30, fix_completeness=40</input>
      <calculation>(80*0.2)+(70*0.3)+(30*0.25)+(40*0.25) = 54.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Critical issues remain unfixed</reasoning>
    </test>
  </validation_tests>
</decision_criteria>

<enforcement>
  <mandatory_behaviors>
    <behavior id="EXECF-B001" priority="critical">
      <trigger>Before implementation</trigger>
      <action>Check Serena memories for existing patterns</action>
      <verification>Pattern check in output</verification>
    </behavior>
    <behavior id="EXECF-B002" priority="critical">
      <trigger>After initial execution</trigger>
      <action>Execute feedback collection phase automatically</action>
      <verification>Feedback results in output</verification>
    </behavior>
    <behavior id="EXECF-B003" priority="critical">
      <trigger>After feedback collection</trigger>
      <action>Evaluate if issues require fix phase</action>
      <verification>Issue evaluation in output</verification>
    </behavior>
    <behavior id="EXECF-B004" priority="critical">
      <trigger>When issues found</trigger>
      <action>Execute fix phase for identified issues only</action>
      <verification>Fix results in output</verification>
    </behavior>
    <behavior id="EXECF-B005" priority="critical">
      <trigger>When no issues found</trigger>
      <action>Skip fix phase with confirmation message</action>
      <verification>Skip confirmation in output</verification>
    </behavior>
    <behavior id="EXECF-B006" priority="critical">
      <trigger>During feedback phase</trigger>
      <action>Launch all feedback agents in parallel</action>
      <verification>Parallel execution confirmed</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="EXECF-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Implementing without sub-agent delegation</action>
      <response>Block operation, delegate to specialized agents</response>
    </behavior>
    <behavior id="EXECF-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Multiple fix iterations</action>
      <response>Block operation, maximum one fix iteration allowed</response>
    </behavior>
    <behavior id="EXECF-P003" priority="critical">
      <trigger>Between phases</trigger>
      <action>Requesting user confirmation to proceed</action>
      <response>Proceed automatically between phases</response>
    </behavior>
    <behavior id="EXECF-P004" priority="critical">
      <trigger>In fix phase</trigger>
      <action>Full re-implementation instead of targeted fixes</action>
      <response>Fix only identified issues from feedback</response>
    </behavior>
    <behavior id="EXECF-P005" priority="critical">
      <trigger>When no issues found</trigger>
      <action>Executing fix phase unnecessarily</action>
      <response>Skip fix phase when feedback shows no issues</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<output>
  <format>
    <execution_results>
      <summary>Brief description of implemented changes</summary>
      <files_modified>
        <file>
          <path>Absolute file path</path>
          <changes>Summary of changes</changes>
        </file>
      </files_modified>
      <tests_status>Pass/Fail status</tests_status>
    </execution_results>
    <feedback_summary>
      <evaluation_scores>
        <metric name="quality">XX/100</metric>
        <metric name="security">XX/100</metric>
        <metric name="design">XX/100</metric>
        <metric name="documentation">XX/100</metric>
        <metric name="performance">XX/100</metric>
        <metric name="test_coverage">XX/100</metric>
        <metric name="overall">XX/100</metric>
      </evaluation_scores>
      <issues_found>
        <critical>
          <issue>
            <category>Category</category>
            <description>Issue description</description>
            <location>File and line reference</location>
          </issue>
        </critical>
        <warnings>
          <issue>
            <category>Category</category>
            <description>Issue description</description>
            <recommendation>Suggested fix</recommendation>
          </issue>
        </warnings>
        <info>
          <issue>
            <category>Category</category>
            <description>Minor observation</description>
          </issue>
        </info>
      </issues_found>
      <good_practices>
        <practice>
          <category>Category</category>
          <description>Commendable aspects</description>
        </practice>
      </good_practices>
    </feedback_summary>
    <fix_results condition="if issues found">
      <issues_addressed>
        <issue>
          <original>Original issue from feedback</original>
          <fix>How it was fixed</fix>
          <status>Fixed/Deferred</status>
        </issue>
      </issues_addressed>
      <deferred_issues>
        <issue>
          <description>Issue not fixed</description>
          <reason>Justification for deferral</reason>
        </issue>
      </deferred_issues>
    </fix_results>
    <skip_confirmation condition="if no issues found">
      <message>No issues requiring fixes were identified in feedback phase</message>
      <status>Fix phase skipped</status>
    </skip_confirmation>
    <final_status>
      <confidence>XX/100</confidence>
      <summary>Final status summary</summary>
      <next_steps>Recommended follow-up actions if any</next_steps>
    </final_status>
  </format>
</output>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor code style inconsistency</example>
    <example severity="medium">Test failure or unclear implementation approach</example>
    <example severity="high">Breaking change or major implementation blocker</example>
    <example severity="critical">Security vulnerability or data loss risk</example>
  </examples>
</error_escalation>

<related_commands>
  <command name="execute">Basic execution without feedback loop</command>
  <command name="feedback">Standalone feedback command for reviewing work</command>
  <command name="define">When implementation reveals unclear requirements</command>
  <command name="define-full">When detailed requirements definition is needed</command>
  <command name="ask">When implementation requires investigation</command>
  <command name="bug">When implementation encounters unexpected errors</command>
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
  <must>Complete all phases: execute, feedback, fix (conditional)</must>
  <must>Automatically proceed between phases without user confirmation</must>
  <must>Skip fix phase when no issues found</must>
  <must>Limit to maximum one fix iteration</must>
  <avoid>Implementing detailed logic directly</avoid>
  <avoid>Multiple fix iterations (exactly one allowed when needed)</avoid>
  <avoid>Sequential execution of independent feedback agents</avoid>
  <avoid>Full re-implementation in fix phase</avoid>
  <avoid>Requesting user confirmation between phases</avoid>
</constraints>
