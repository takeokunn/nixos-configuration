---
name: Core Patterns
description: Base templates for error escalation, decision criteria, and enforcement, referenced by agents and commands to avoid duplication. Also holds cross-cutting patterns worth loading directly — modelling absence structurally instead of with an in-range sentinel such as 0, -1, or the empty string, deriving a cost estimate from the emitter that actually produces the artifact rather than re-modelling it in a second place, resolving an apparent contradiction between two rulesets by finding the distinguishing condition instead of weakening either, and safe alternatives to destructive Git commands — including mirroring a worktree's state back into the shared checkout with a file sync rather than a branch switch, and the preconditions for removing a worktree.
version: 2.2.0
---

<purpose>
  Provide standardized base templates for error handling, decision criteria, and enforcement behaviors shared across all agents and commands.
</purpose>

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

  <pattern name="parallel_project_isolation">
    <description>Constraints for safe operation when multiple Claude Code sessions
      run concurrently in the same working directory</description>
    <assumption>Assume other Claude Code sessions may be active in the same repository
      at any time. Never treat the working directory as exclusively owned.</assumption>
    <prohibited_operations>
      <operation>git stash / git stash pop — may absorb or destroy another session's uncommitted changes</operation>
      <operation>git checkout [branch] / git switch [branch] — switches working tree, destroying other sessions' work</operation>
      <operation>git reset --hard — discards all uncommitted changes across all sessions</operation>
      <operation>git clean -f / git clean -fd — deletes untracked files that may belong to other sessions</operation>
    </prohibited_operations>
    <safe_alternatives>
      <alternative>Branch isolation needed → git worktree add [path] [branch]</alternative>
      <alternative>Work-in-progress save → WIP commit instead of stash</alternative>
      <alternative>Use Claude Code's isolation: worktree mode for truly isolated work</alternative>
      <alternative>Reflect a worktree's state back into the main checkout → mirror the files with a sync tool
        (rsync in archive mode with delete, excluding the git metadata directory and any nested worktree
        directory) instead of switching branches in the shared tree. This propagates unstaged, staged, and
        untracked changes without touching Git metadata, which is exactly the moment someone otherwise
        reaches for a prohibited command: the isolation guidance above says how to create a worktree but
        nothing about how to get its state back.</alternative>
    </safe_alternatives>
    <worktree_removal_preconditions>
      <description>Removing a linked worktree destroys anything not reflected elsewhere, so it needs preconditions rather than a judgement call.</description>
      <precondition>The main worktree has no unmerged paths.</precondition>
      <precondition>The main worktree's complete working-tree diff against the selected target branch is empty — meaning the mirrored state is fully present, not merely believed to be.</precondition>
      <precondition>Branch refs are retained until the reflected state is committed, so the work is recoverable if the mirror was incomplete.</precondition>
    </worktree_removal_preconditions>
  </pattern>

  <pattern name="presence_vs_value">
    <description>Absence and value are different facts, and an in-range value must never be read as "unset"</description>
    <problem>Choosing a sentinel that lies inside the valid domain — 0, -1, the empty string — collapses two distinct cases. A guard like "apply the update only if the value is non-zero" silently drops every legitimate zero observation and leaves dependent state stale, and it fails as a dropped fact rather than as an error, so nothing surfaces it.</problem>
    <rule>Model absence structurally: a nullable type, an option or maybe type, or an explicit supplied-p flag alongside the value.</rule>
    <rule>Test optional numerics with a null or presence check, never with truthiness or a comparison against a domain value.</rule>
    <rule>If an in-range sentinel is nonetheless chosen deliberately, record that every consumer now inherits the ambiguity and must branch on it. That downstream tax is the real cost of the choice, and it is paid at every call site rather than at the definition.</rule>
  </pattern>

  <pattern name="estimator_derived_from_emitter">
    <description>A cost estimate that drives a strategy decision must come from the code that produces the artifact</description>
    <problem>An independently-modeled cost function drifts from the emitter it models, because the emitter optimizes (batching, grouping, shared setup) in ways the model does not track. A per-unit accounting model can overestimate by an order of magnitude against what is actually emitted, and the strategy switch it feeds then picks the wrong branch with full confidence.</problem>
    <rule>Derive the estimate from the emitter — call it, or have it report the size it produced — rather than re-modeling its behavior in a second place.</rule>
    <rule>Have the threshold fixtures consume the same function the production decision consumes. If they diverge, the tests validate a number nobody uses.</rule>
    <applies_to>Any size-based, cost-based, or budget-based strategy switch: choosing between a full and an incremental path, batching versus streaming, or a fast path selected by predicted output size.</applies_to>
  </pattern>

  <pattern name="apparent_rule_conflict_resolution">
    <description>How to handle two related rulesets that appear to contradict each other</description>
    <context>In a corpus of cross-referencing skills and agents, apparent conflicts arise as the corpus grows. The reflex is to pick a winner and weaken the loser, which loses real guidance.</context>
    <procedure>
      <step order="1">Assume both rules are correct and look for the distinguishing condition that separates their domains. Most apparent conflicts are two correct rules stated without their preconditions.</step>
      <step order="2">Add a reconciling note to the affected category naming that condition. This restores consistency without changing the substance of either rule, which is the smallest edit that fixes the problem.</step>
      <step order="3">Only if no distinguishing condition exists is one of the rules actually wrong. Weakening or removing a rule is the last resort, not the first move.</step>
    </procedure>
    <note>Prefer a condition that already exists in the material over one invented to settle the dispute; an invented axis tends to be unmemorable and will not be applied consistently later.</note>
  </pattern>
</patterns>

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
  <practice priority="high">Model absence structurally instead of with an in-range sentinel, and never infer "unset" from a value inside the valid domain (presence_vs_value)</practice>
  <practice priority="medium">Mirror a worktree into the shared checkout with a file sync rather than a branch switch, and remove a worktree only once its diff against the target is empty (parallel_project_isolation)</practice>
  <practice priority="medium">Derive a strategy-driving cost estimate from the emitter itself, and have tests consume the same function (estimator_derived_from_emitter)</practice>
  <practice priority="medium">When two related rulesets appear to contradict, find the distinguishing condition and add a reconciling note before weakening either (apparent_rule_conflict_resolution)</practice>
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

<tools>
  <tool name="Read">Read relevant source files and docs</tool>
  <tool name="Grep">Search for patterns and references</tool>
</tools>

<decision_tree name="skill_activation">
  <question>Does the task clearly match this skill domain?</question>
  <branch condition="Yes">Use this skill workflow and constraints</branch>
  <branch condition="No">Use a more appropriate domain skill</branch>
</decision_tree>

<related_agents>
  <agent name="explore">Locate code patterns and references for this domain</agent>
  <agent name="quality-assurance">Review implementation quality against this skill guidance</agent>
</related_agents>

<constraints>
  <must>Define exactly 4 severity levels for error_escalation</must>
  <must>Include all 5 validation tests for decision_criteria</must>
  <must>Ensure weights sum to 1.0</must>
  <must>Use standard confidence thresholds (60/80)</must>
  <avoid>Inventing new severity levels</avoid>
  <avoid>Omitting boundary tests</avoid>
  <avoid>Using non-standard thresholds</avoid>
</constraints>

<related_skills>
  <skill name="parallelization-patterns">Parallel execution strategies and timeout configuration</skill>
  <skill name="workflow-patterns">Output formats, reflection checkpoints, agent weights</skill>
  <skill name="serena-usage">For memory operations to store pattern decisions</skill>
</related_skills>
