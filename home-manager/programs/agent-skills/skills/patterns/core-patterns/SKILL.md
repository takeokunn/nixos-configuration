---
name: Core Patterns
description: Base templates for error escalation, decision criteria, and enforcement. Referenced by agents and commands to avoid duplication.
version: 2.0.0
---

<purpose>
  Provide standardized base templates for error handling, decision criteria, and enforcement behaviors shared across all agents and commands.
</purpose>

<tools>
  <tool name="error_escalation_template">
    <description>Standard 4-level severity escalation pattern</description>
    <use_case>Include in agents and commands for consistent error handling</use_case>
  </tool>

  <tool name="decision_criteria_template">
    <description>Standard confidence calculation with validation tests</description>
    <use_case>Include in agents and commands for consistent quality assessment</use_case>
  </tool>

  <tool name="enforcement_template">
    <description>Standard mandatory and prohibited behaviors pattern</description>
    <use_case>Include in agents and commands for consistent behavior enforcement</use_case>
  </tool>
</tools>

<concepts>
  <concept name="severity_levels">
    <description>Standard 4-level severity classification for error escalation</description>
    <example>
      low: Minor issues, note and proceed
      medium: Unclear situations, document and ask user
      high: Breaking changes, STOP and present options
      critical: Security/data risks, BLOCK and require acknowledgment
    </example>
  </concept>

  <concept name="confidence_thresholds">
    <description>Standard confidence score boundaries for status determination</description>
    <example>
      success: confidence >= 80
      warning: confidence 60-79
      error: confidence less than 60

      Boundary tests required:
      boundary_success_80: Exactly 80, yields success
      boundary_warning_79: 78.5-79.9, yields warning
      boundary_error_59: 58.5-59.9, yields error
    </example>
  </concept>

  <concept name="behavior_ids">
    <description>Naming convention for enforcement behavior IDs</description>
    <example>
      Format: PREFIX-TYPE-NUMBER
      PREFIX: Agent/command abbreviation (e.g., EXEC, DEF, EXP)
      TYPE: B for mandatory behavior, P for prohibited
      NUMBER: Sequential (001, 002, ...)

      Examples:
      EXEC-B001: Execute command mandatory behavior 1
      DEF-P002: Define command prohibited behavior 2
    </example>
  </concept>

  <concept name="weight_distribution">
    <description>Decision criteria weights must sum to 1.0</description>
    <example>
      Standard distributions:
      3-factor equal: 0.33, 0.34, 0.33
      3-factor weighted: 0.4, 0.3, 0.3
      2-factor equal: 0.5, 0.5
      2-factor weighted: 0.6, 0.4
    </example>
  </concept>
</concepts>

<patterns>
  <pattern name="error_escalation">
    <description>Standard 4-level error escalation template</description>
    <example>
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
    </example>
  </pattern>

  <pattern name="decision_criteria">
    <description>Standard decision criteria with validation tests including boundary cases</description>
    <example>
<decision_criteria>
  <criterion name="confidence_calculation">
    <factor name="factor1" weight="0.4">
      <score range="90-100">Excellent condition</score>
      <score range="70-89">Good condition</score>
      <score range="50-69">Fair condition</score>
      <score range="0-49">Poor condition</score>
    </factor>
    <factor name="factor2" weight="0.3">
      <score range="90-100">Excellent</score>
      <score range="70-89">Good</score>
      <score range="50-69">Fair</score>
      <score range="0-49">Poor</score>
    </factor>
    <factor name="factor3" weight="0.3">
      <score range="90-100">Excellent</score>
      <score range="70-89">Good</score>
      <score range="50-69">Fair</score>
      <score range="0-49">Poor</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="success_case">
      <input>factor1=95, factor2=90, factor3=95</input>
      <calculation>(95*0.4)+(90*0.3)+(95*0.3) = 38+27+28.5 = 93.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>factor1=85, factor2=75, factor3=80</input>
      <calculation>(85*0.4)+(75*0.3)+(80*0.3) = 34+22.5+24 = 80.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>Weighted average 80.5 meets success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>factor1=80, factor2=75, factor3=80</input>
      <calculation>(80*0.4)+(75*0.3)+(80*0.3) = 32+22.5+24 = 78.5</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Weighted average 78.5 triggers warning</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>factor1=60, factor2=55, factor3=60</input>
      <calculation>(60*0.4)+(55*0.3)+(60*0.3) = 24+16.5+18 = 58.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Weighted average 58.5 is below 60, triggers error</reasoning>
    </test>
    <test name="error_case">
      <input>factor1=50, factor2=55, factor3=45</input>
      <calculation>(50*0.4)+(55*0.3)+(45*0.3) = 20+16.5+13.5 = 50</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores yield error</reasoning>
    </test>
  </validation_tests>
</decision_criteria>
    </example>
  </pattern>

  <pattern name="enforcement">
    <description>Standard enforcement template with mandatory and prohibited behaviors</description>
    <example>
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
    </example>
  </pattern>

  <pattern name="refs_syntax">
    <description>Standard syntax for referencing skills from agents and commands</description>
    <example>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="patterns">parallelization-patterns</skill>
  <skill use="patterns">workflow-patterns</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="domain">nix-ecosystem</skill>
</refs>

Use attribute values:
  patterns: Shared templates (core-patterns, parallelization-patterns, workflow-patterns)
  tools: Tool-specific usage patterns (serena-usage, context7-usage)
  workflow: How-to guides and methodologies (investigation-patterns, execution-workflow)
  domain: Domain knowledge and best practices (nix-ecosystem, typescript-ecosystem)
    </example>
  </pattern>
</patterns>

<anti_patterns>
  <avoid name="inline_error_escalation">
    <description>Duplicating full error_escalation in each file</description>
    <instead>Reference core-patterns and customize only examples</instead>
  </avoid>

  <avoid name="inconsistent_thresholds">
    <description>Using different confidence thresholds (75, 80, 85) across files</description>
    <instead>Always use 60/80 boundaries as defined in core-patterns</instead>
  </avoid>

  <avoid name="missing_boundary_tests">
    <description>Omitting boundary validation tests (59/60, 79/80)</description>
    <instead>Always include boundary_success_80, boundary_warning_79, boundary_error_59 tests</instead>
  </avoid>

  <avoid name="weight_sum_mismatch">
    <description>Decision criteria weights not summing to 1.0</description>
    <instead>Verify weights sum to exactly 1.0 (e.g., 0.4+0.3+0.3)</instead>
  </avoid>

  <avoid name="inconsistent_behavior_ids">
    <description>Using different ID formats across files</description>
    <instead>Use PREFIX-TYPE-NUMBER format consistently</instead>
  </avoid>
</anti_patterns>

<best_practices>
  <practice priority="critical">Reference core-patterns for error_escalation, decision_criteria, enforcement templates</practice>
  <practice priority="critical">Always include all 5 validation tests: success, boundary_success_80, boundary_warning_79, boundary_error_59, error</practice>
  <practice priority="critical">Ensure decision criteria weights sum to 1.0</practice>
  <practice priority="high">Customize error_escalation examples to be domain-specific while keeping structure</practice>
  <practice priority="high">Use consistent behavior ID prefixes within each agent/command</practice>
</best_practices>

<rules priority="critical">
  <rule>Always include all 5 validation tests for decision_criteria</rule>
  <rule>Boundary tests must use exact threshold values (80, 79.x, 59.x)</rule>
  <rule>Error escalation must have exactly 4 severity levels</rule>
  <rule>Weights in decision_criteria must sum to 1.0</rule>
</rules>

<rules priority="standard">
  <rule>Use refs tag to reference this skill from agents and commands</rule>
  <rule>Customize examples in error_escalation while keeping structure</rule>
  <rule>Use consistent behavior ID naming convention</rule>
</rules>

<enforcement>
  <mandatory_behaviors>
    <behavior id="CORE-B001" priority="critical">
      <trigger>When creating new agent or command</trigger>
      <action>Reference core-patterns skill in refs section</action>
      <verification>refs tag contains core-patterns</verification>
    </behavior>
    <behavior id="CORE-B002" priority="critical">
      <trigger>When defining decision_criteria</trigger>
      <action>Include all 5 validation tests with boundary cases</action>
      <verification>Tests include boundary_success_80, boundary_warning_79, boundary_error_59</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="CORE-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Using non-standard confidence thresholds</action>
      <response>Use 60/80 thresholds as defined in this skill</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<error_escalation>
  <level severity="low">
    <example>Minor inconsistency in behavior ID format</example>
    <action>Note in report, proceed</action>
  </level>
  <level severity="medium">
    <example>Missing one boundary test</example>
    <action>Document issue, add missing test</action>
  </level>
  <level severity="high">
    <example>Decision criteria weights do not sum to 1.0</example>
    <action>STOP, fix weight distribution before proceeding</action>
  </level>
  <level severity="critical">
    <example>Error escalation missing critical level</example>
    <action>BLOCK operation, require complete 4-level structure</action>
  </level>
</error_escalation>

<related_skills>
  <skill name="parallelization-patterns">Parallel execution strategies and timeout configuration</skill>
  <skill name="workflow-patterns">Output formats, reflection checkpoints, agent weights</skill>
  <skill name="serena-usage">For memory operations to store pattern decisions</skill>
</related_skills>

<constraints>
  <must>Define exactly 4 severity levels for error_escalation</must>
  <must>Include all 5 validation tests for decision_criteria</must>
  <must>Ensure weights sum to 1.0</must>
  <must>Use standard confidence thresholds (60/80)</must>
  <avoid>Inventing new severity levels</avoid>
  <avoid>Omitting boundary tests</avoid>
  <avoid>Using non-standard thresholds</avoid>
</constraints>
