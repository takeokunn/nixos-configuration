---
argument-hint: "[skill-name]"
description: Capture session as reusable skill
---

<purpose>
Analyze the current session to extract a repeatable process, interview the user to refine it, and produce a SKILL.md file that can be invoked as a reusable skill.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="tools">serena-usage</skill>
</refs>
<rules priority="critical">
  <rule>Use AskUserQuestion tool for ALL interview rounds; never use plain text for questions</rule>
  <rule>Do not over-ask for simple processes; collapse trivial steps</rule>
  <rule>Present the complete SKILL.md for user review before saving</rule>
  <rule>Every step in the generated SKILL.md must have explicit success criteria</rule>
</rules>
<rules priority="standard">
  <rule>Analyze the session thoroughly before asking any questions</rule>
  <rule>Suggest sensible defaults for all options; let user override</rule>
  <rule>Include allowed-tools in the SKILL.md frontmatter based on observed tool usage</rule>
  <rule>Respect the user's naming preferences over auto-generated names</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Asking clarifying questions immediately without first analyzing the session — AI must complete full session analysis before starting any interview round to avoid asking questions the session already answers</practice>
    <practice>Writing the SKILL.md in one pass without structured user validation — the four-round interview produces the user input needed to make the skill reusable rather than session-specific</practice>
    <practice>Using generic step descriptions without success criteria — every step in the generated SKILL.md must have an explicit success condition so future invocations can self-verify</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Extract the repeatable process from session history holistically: identify inputs, steps, corrections, tools, and success signals before framing the first interview question</principle>
    <principle>Use AskUserQuestion for all four interview rounds to enable structured option selection; collapse rounds only for trivially simple single-step processes</principle>
    <principle>Present the complete SKILL.md as a code block before writing; the user's explicit confirmation is a hard gate that prevents premature file creation</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <objective>Initialize Serena and check existing patterns</objective>
    <step order="1">
      <action>Activate Serena project with activate_project</action>
      <tool>Serena activate_project</tool>
      <output>Project activated</output>
    </step>
    <step order="2">
      <action>Check list_memories for relevant skill patterns</action>
      <tool>Serena list_memories</tool>
      <output>Available memory list</output>
    </step>
    <step order="3">
      <action>Load applicable memories with read_memory</action>
      <tool>Serena read_memory</tool>
      <output>Relevant patterns loaded</output>
    </step>
  </phase>
  <phase name="analyze_session">
    <objective>Identify the repeatable process from the current session</objective>
    <step order="1">
      <action>Scan the session history for the process that was performed</action>
      <tool>Session analysis</tool>
      <output>Process description</output>
    </step>
    <step order="2">
      <action>Identify inputs, parameters, and variable elements</action>
      <tool>Pattern extraction</tool>
      <output>Input/parameter list</output>
    </step>
    <step order="3">
      <action>Extract the distinct steps in execution order</action>
      <tool>Step decomposition</tool>
      <output>Ordered step list</output>
    </step>
    <step order="4">
      <action>Note where the user corrected or steered the agent</action>
      <tool>Correction analysis</tool>
      <output>Correction points and lessons</output>
    </step>
    <step order="5">
      <action>Catalog tools, permissions, and agents used</action>
      <tool>Tool inventory</tool>
      <output>Tool and agent list</output>
    </step>
    <step order="6">
      <action>Identify success criteria for each step</action>
      <tool>Criteria extraction</tool>
      <output>Per-step success criteria</output>
    </step>
  </phase>
  <phase name="interview">
    <objective>Refine the skill definition through 4 structured interview rounds</objective>
    <step order="1">
      <action>Round 1 - High-level confirmation: Suggest a name and description for the skill, propose high-level goals and specific success criteria, ask user to confirm or rename</action>
      <tool>AskUserQuestion</tool>
      <output>Confirmed skill name, description, goals, success criteria</output>
    </step>
    <step order="2">
      <action>Round 2 - Details: Present high-level steps as numbered list, suggest arguments based on observed parameters, ask whether skill should run inline (user steers mid-process) or forked (self-contained sub-agent), ask save location (repo .claude/skills/ or personal ~/.claude/skills/)</action>
      <tool>AskUserQuestion</tool>
      <output>Confirmed steps, arguments, execution context, save location</output>
    </step>
    <step order="3">
      <action>Round 3 - Step breakdown: For each major step ask what it produces, what proves success, whether user should confirm before proceeding, whether steps are independent (parallelizable), and any hard constraints or preferences</action>
      <tool>AskUserQuestion</tool>
      <output>Detailed step specifications with dependencies and checkpoints</output>
    </step>
    <step order="4">
      <action>Round 4 - Final questions: Confirm when the skill should be invoked, suggest trigger phrases, ask about gotchas or edge cases to watch out for</action>
      <tool>AskUserQuestion</tool>
      <output>Trigger conditions, edge cases, final adjustments</output>
    </step>
  </phase>
  <reflection_checkpoint id="interview_complete" after="interview">
    <questions>
      <question weight="0.4">Are all steps clearly defined with success criteria?</question>
      <question weight="0.3">Are the arguments and inputs well specified?</question>
      <question weight="0.3">Is the execution context (inline vs forked) appropriate?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Ask additional clarifying questions</below_threshold>
    </threshold>
  </reflection_checkpoint>
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
  <phase name="write_skill">
    <objective>Produce the SKILL.md content</objective>
    <step order="1">
      <action>Compose YAML frontmatter with name, description, allowed-tools, when_to_use, argument-hint, arguments, and context</action>
      <tool>Template composition</tool>
      <output>YAML frontmatter block</output>
    </step>
    <step order="2">
      <action>Write the skill body with Inputs, Goal, and Steps sections; annotate each step with success criteria, execution mode, artifacts, human checkpoints, and rules as applicable</action>
      <tool>Markdown composition</tool>
      <output>Complete SKILL.md content</output>
    </step>
    <step order="3">
      <action>Present the complete SKILL.md as a code block for user review</action>
      <tool>Output presentation</tool>
      <output>SKILL.md displayed for review</output>
    </step>
  </phase>
  <phase name="confirm_and_save">
    <objective>Get user confirmation and write the file</objective>
    <step order="1">
      <action>Ask user to confirm the SKILL.md content or request changes</action>
      <tool>AskUserQuestion</tool>
      <output>User confirmation or change requests</output>
    </step>
    <step order="2">
      <action>If changes requested, revise and re-present; repeat until confirmed</action>
      <tool>Iterative revision</tool>
      <output>Final confirmed SKILL.md</output>
    </step>
    <step order="3">
      <action>Write the SKILL.md file to the confirmed location</action>
      <tool>Write tool</tool>
      <output>File written</output>
    </step>
    <step order="4">
      <action>Report to user: where the file was saved, how to invoke it (e.g. /skill-name), and that it can be edited directly</action>
      <tool>Summary output</tool>
      <output>Completion message with usage instructions</output>
    </step>
  </phase>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Handle execution errors and apply fallback strategy</action>
      <tool>Error analysis and retry policy</tool>
      <output>Recovered execution path or documented blocker</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="explore" subagent_type="explore" readonly="true">Finding existing skills and patterns in the codebase</agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Session analysis and skill structure validation</agent>
</agents>
<execution_graph>
  <parallel_group id="analysis" depends_on="none">
    <agent>explore</agent>
    <agent>general-purpose</agent>
  </parallel_group>
</execution_graph>
<delegation>
  <requirement>Session context and user messages</requirement>
  <requirement>Existing skill patterns for reference</requirement>
  <requirement>Read-only constraint for analysis agents</requirement>
</delegation>
<decision_criteria inherits="core-patterns#decision_criteria">
  <criterion name="confidence_calculation">
    <factor name="session_analysis" weight="0.3">
      <score range="90-100">Clear repeatable process with all steps identified</score>
      <score range="70-89">Process identified with some ambiguity</score>
      <score range="50-69">Partial process identification</score>
      <score range="0-49">No clear repeatable process found</score>
    </factor>
    <factor name="interview_completeness" weight="0.4">
      <score range="90-100">All rounds completed with clear user responses</score>
      <score range="70-89">Most rounds completed</score>
      <score range="50-69">Some rounds skipped or unclear</score>
      <score range="0-49">Minimal user input gathered</score>
    </factor>
    <factor name="skill_quality" weight="0.3">
      <score range="90-100">All steps have success criteria, clear triggers, proper frontmatter</score>
      <score range="70-89">Most steps complete</score>
      <score range="50-69">Some steps missing criteria</score>
      <score range="0-49">Incomplete skill definition</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="success_case">
      <input>session_analysis=92, interview_completeness=93, skill_quality=92</input>
      <calculation>(92*0.3)+(93*0.4)+(92*0.3) = 92.4</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>session_analysis=80, interview_completeness=80, skill_quality=80</input>
      <calculation>(80*0.3)+(80*0.4)+(80*0.3) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>session_analysis=79, interview_completeness=79, skill_quality=79</input>
      <calculation>(79*0.3)+(79*0.4)+(79*0.3) = 79</calculation>
      <expected_status>warning</expected_status>
      <reasoning>79 is below success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>session_analysis=59, interview_completeness=59, skill_quality=59</input>
      <calculation>(59*0.3)+(59*0.4)+(59*0.3) = 59</calculation>
      <expected_status>error</expected_status>
      <reasoning>59 is at error threshold</reasoning>
    </test>
    <test name="error_case">
      <input>session_analysis=40, interview_completeness=35, skill_quality=45</input>
      <calculation>(40*0.3)+(35*0.4)+(45*0.3) = 39.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores yield error status</reasoning>
    </test>
  </validation_tests>
</decision_criteria>
<output>
  <format>
    <skill_md_template>
      <frontmatter>
        <field name="name">skill-name (kebab-case)</field>
        <field name="description">One-line description</field>
        <field name="allowed-tools">List of tool permission patterns observed in session</field>
        <field name="when_to_use">Detailed trigger description with example phrases</field>
        <field name="argument-hint">Hint showing argument placeholders</field>
        <field name="arguments">List of argument names</field>
        <field name="context">inline or fork</field>
      </frontmatter>
      <body>
        <section name="Inputs">Describe each argument and its expected format</section>
        <section name="Goal">Clear statement of what the skill accomplishes</section>
        <section name="Steps">
          <step_annotations>
            <annotation name="Success criteria" required="true">What proves this step succeeded</annotation>
            <annotation name="Execution mode" required="false">parallel or sequential</annotation>
            <annotation name="Artifacts" required="false">What this step produces for later steps</annotation>
            <annotation name="Human checkpoint" required="false">Whether to pause for user confirmation</annotation>
            <annotation name="Rules" required="false">Hard constraints for this step</annotation>
          </step_annotations>
        </section>
      </body>
    </skill_md_template>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="SKL-B001" priority="critical">
      <trigger>Before interview phase</trigger>
      <action>Complete session analysis first</action>
      <verification>Analysis summary present in output</verification>
    </behavior>
    <behavior id="SKL-B002" priority="critical">
      <trigger>For all user interactions</trigger>
      <action>Use AskUserQuestion tool with structured options</action>
      <verification>All questions use AskUserQuestion</verification>
    </behavior>
    <behavior id="SKL-B003" priority="critical">
      <trigger>Before saving SKILL.md</trigger>
      <action>Present complete content for user review and get explicit confirmation</action>
      <verification>User confirmation recorded before write</verification>
    </behavior>
    <behavior id="SKL-B004" priority="critical">
      <trigger>When writing Steps section</trigger>
      <action>Include success criteria annotation on every step</action>
      <verification>Every step has a Success criteria line</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="SKL-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Saving the SKILL.md without user confirmation</action>
      <response>Present for review first</response>
    </behavior>
    <behavior id="SKL-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Skipping interview rounds for non-trivial processes</action>
      <response>Complete all 4 rounds; collapse only for very simple processes</response>
    </behavior>
    <behavior id="SKL-P003" priority="critical">
      <trigger>Always</trigger>
      <action>Using plain text output for interview questions instead of AskUserQuestion</action>
      <response>Use AskUserQuestion tool</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor ambiguity in step ordering</example>
    <example severity="medium">Session has no clear repeatable process</example>
    <example severity="high">Conflicting user answers across interview rounds</example>
    <example severity="critical">Skill would automate destructive operations without safeguards</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="execute">For running tasks that a skill might automate</command>
  <command name="define">For defining requirements before creating a skill</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="core-patterns">Shared enforcement and decision patterns</skill>
  <skill name="serena-usage">Memory operations for storing skill metadata</skill>
</related_skills>
<constraints>
  <must>Analyze session before asking questions</must>
  <must>Use AskUserQuestion for all interview interactions</must>
  <must>Include success criteria on every generated step</must>
  <must>Get explicit user confirmation before writing SKILL.md</must>
  <must>Tell user where the file was saved and how to invoke it after completion</must>
  <avoid>Over-asking for simple processes; collapse trivial rounds</avoid>
  <avoid>Generating skills without observed session patterns to base them on</avoid>
  <avoid>Writing SKILL.md without presenting it for review first</avoid>
</constraints>
