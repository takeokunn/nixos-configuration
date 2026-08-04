---
name: Execution Workflow
description: This skill should be used when the user asks to "execute task", "implement feature", "delegate work", "run workflow", "review code", "code quality check", "definition of done", or needs task orchestration and code review guidance. Also covers treating done as an enumerated set of verification commands exiting zero rather than a judgement call, reporting the verification tier actually reached and the exact command a later session must re-run, choosing between a convention-conformance review and a behavior review (the first approves what the second rejects), checking for surprise dirty state before committing in a checkout other sessions may share, and never skipping a failing hook or gate to get a commit through. Provides execution, delegation, and code review patterns.
version: 2.2.0
---

<purpose>
  Provide structured workflow for task execution through delegation to specialized sub-agents, and comprehensive code review standards.
</purpose>

<tools>
  <tool name="agent_groups">Specialized sub-agents: quality_assurance (quality, security - parallel), implementation (test, refactor, docs - parallel if independent), review (sequential after implementation)</tool>
  <tool name="delegation">Provide scope, file paths, Serena/Context7 tool instructions, reference implementations, memory checks</tool>
  <tool name="tool_selection">Coding: Serena MCP → Context7 → Basic tools; Non-coding: Serena MCP → Context7 → Basic tools</tool>
</tools>

<concepts>
  <concept name="parallel_execution">Execute independent tasks concurrently; quality+security can run in parallel, test+docs can run in parallel when independent</concept>
  <concept name="sequential_dependencies">Tasks with data dependencies must run in order; verify outputs before dependent tasks start</concept>
  <concept name="delegation_context">Sub-agents need: specific scope, file paths, tool usage instructions, reference implementations, memory patterns</concept>
  <concept name="review_phases">Four phases: Initial scan (syntax), Deep analysis (logic), Context evaluation (impact), Standards compliance (naming/docs)</concept>
</concepts>

<patterns>
  <pattern name="code_review_phases">
    <description>Systematic code review process</description>
    <decision_tree name="when_to_use">
      <question>Has code been modified or newly created?</question>
      <if_yes>Apply code review phases systematically to ensure quality</if_yes>
      <if_no>Skip review and proceed to next task</if_no>
    </decision_tree>
    <example>
      Phase 1 - Initial Scan:
      - Syntax errors and typos
      - Missing imports or dependencies
      - Obvious logic errors
      - Code style violations

      Phase 2 - Deep Analysis:

      - Algorithm correctness
      - Edge case handling
      - Error handling completeness
      - Resource management

      Phase 3 - Context Evaluation:

      - Breaking changes to public APIs
      - Side effects on existing functionality
      - Dependency compatibility

      Phase 4 - Standards Compliance:

      - Naming conventions
      - Documentation requirements
      - Test coverage
    </example>
  </pattern>

  <pattern name="quality_criteria">
    <description>Evaluation criteria for code quality</description>
    <decision_tree name="when_to_use">
      <question>Is this a code review or quality assessment task?</question>
      <if_yes>Apply quality criteria across all dimensions</if_yes>
      <if_no>Focus on implementation patterns instead</if_no>
    </decision_tree>
    <example>
      Correctness:
      - Logic matches requirements
      - Edge cases handled
      - Error conditions covered

      Security:

      - Input validation
      - Authentication/authorization
      - Data sanitization
      - Secrets handling

      Performance:

      - Algorithm efficiency
      - Resource usage
      - Memory leaks
      - N+1 queries

      Maintainability:

      - Clear naming
      - Appropriate comments
      - Single responsibility
      - DRY principle

      Testability:

      - Test coverage adequate
      - Tests meaningful
      - Edge cases tested
    </example>
  </pattern>

  <pattern name="feedback_categories">
    <description>Categorization of review feedback by priority</description>
    <decision_tree name="when_to_use">
      <question>Have you identified issues during code review?</question>
      <if_yes>Apply feedback categories to prioritize by severity</if_yes>
      <if_no>Continue code review phases</if_no>
    </decision_tree>
    <example>
      Critical: Must fix before merge
      - Security vulnerabilities
      - Data corruption risks
      - Breaking changes

      Important: Should fix before merge

      - Logic errors
      - Missing error handling
      - Performance issues

      Suggestion: Nice to have improvements

      - Code style
      - Refactoring opportunities
      - Documentation

      Positive: What was done well

      - Good patterns
      - Clever solutions
      - Thorough testing
    </example>
  </pattern>

  <pattern name="review_output_format">
    <description>Standard format for code review results</description>
    <decision_tree name="when_to_use">
      <question>Is it time to communicate code review findings?</question>
      <if_yes>Apply review output format for structured communication</if_yes>
      <if_no>Continue analyzing code through review phases</if_no>
    </decision_tree>
    <example>
      <summary>Overall assessment and recommendation</summary>
      <critical_issues>Must-fix items with file:line references</critical_issues>
      <important_issues>Should-fix items</important_issues>
      <suggestions>Optional improvements</suggestions>
      <positive_feedback>Good practices observed</positive_feedback>
      <questions>Clarifications needed</questions>
    </example>
  </pattern>

  <pattern name="definition_of_done">
    <description>Task completion is an enumerated set of verification commands exiting zero, not a subjective judgement</description>
    <decision_tree name="when_to_use">
      <question>Are you about to report a task as complete?</question>
      <if_yes>Run the project's enumerated verification commands and report each one's result</if_yes>
      <if_no>Continue implementation</if_no>
    </decision_tree>
    <rules>
      <rule>Enumerate the project's verification commands — formatter, linter, type or compile check, test suite, and any project-specific gate — and treat "all of these exit zero" as the definition of done. Naming the list makes completion checkable without asking the user what counts.</rule>
      <rule>A failing pre-push or pre-commit hook is evidence about the work, not an obstacle in front of it. The correct response is to fix the work. Never bypass the hook with a skip-verification flag, and apply the same reading to a red CI job.</rule>
      <rule>A verification gate that selects and runs zero tests is a false green: assert a nonzero selected-test count before reading a pass as a pass. See test-integrity for the full treatment of selector, double, and teardown traps.</rule>
      <rule>Name exactly one canonical gate for the project so a narrower subset run is never reported as if it were the whole gate.</rule>
    </rules>
  </pattern>

  <pattern name="verification_ceiling_reporting">
    <description>State the verification tier actually reached and the exact command a future session must re-run</description>
    <tiers>
      <tier order="1">Static read or parse check — the source was inspected, nothing was executed</tier>
      <tier order="2">Interpreted or partial load — the code loaded but was not compiled or exercised</tier>
      <tier order="3">Real compile, load, and run of the relevant tests locally</tier>
      <tier order="4">The project's canonical gate green in CI, on a clean environment</tier>
    </tiers>
    <rules>
      <rule>Name the tier actually achieved, and say plainly that a lower tier is not equivalent to a higher one even when it found real bugs. Hand-tracing catches genuine defects and is worth doing; it is still not a compile-and-run confirmation.</rule>
      <rule>Record the exact command the next session should run first to close the gap, so resuming is a lookup rather than a reconstruction.</rule>
      <rule>Report which checks ran and which could not run, with the reason. A silently omitted check reads as a passed check.</rule>
    </rules>
    <note>The tiers genuinely differ: bugs that survive extensive local smoke testing are routinely caught only by a full clean-environment run. Treat the gap between tiers as real risk, not as bookkeeping.</note>
  </pattern>

  <pattern name="review_lens_selection">
    <description>Convention-conformance review and behavior review are different reviews, and the first will approve what the second rejects</description>
    <decision_tree name="when_to_use">
      <question>Is the change's stated purpose behavioral — performance, correctness, or concurrency?</question>
      <if_yes>A conformance pass is not sufficient evidence; run a behavior review against what the code does at runtime</if_yes>
      <if_no>A conformance review may be adequate on its own, but say which lens was applied</if_no>
    </decision_tree>
    <rules>
      <rule>A reviewer working from a conformance checklist systematically cannot catch a correctness defect that is spelled like the convention. Every box ticks, and the change ships with the bug the convention was meant to prevent.</rule>
      <rule>Do not report a high conformance score as approval. State which lens produced it, so a later reader does not treat a style pass as a behavioral clearance.</rule>
      <rule>When two reviews of the same change disagree sharply, the disagreement is usually a lens difference rather than a judgement difference. Identify which lens each applied before trying to reconcile the verdicts.</rule>
    </rules>
  </pattern>

  <pattern name="shared_repo_dirty_state_check">
    <description>Check for surprise dirty state before committing into a repository other sessions may be using</description>
    <decision_tree name="when_to_use">
      <question>Are you about to stage or commit in a checkout that other sessions or people may share?</question>
      <if_yes>Inspect working-tree state and attribute every hunk before staging</if_yes>
      <if_no>Proceed, but still stage deliberately rather than staging everything</if_no>
    </decision_tree>
    <procedure>
      <step order="1">Inspect status and the full diff before staging anything. Use the plain, non-decorated diff form so the output is actually parseable (see quality-tools on external diff drivers).</step>
      <step order="2">If every hunk is cleanly attributable to your own work, stage only those.</step>
      <step order="3">If a shared file — an export list, a build manifest, a lockfile — carries changes interleaved with someone else's, stop and ask. Do not bundle them and do not split them speculatively; whose-work-is-it is not inferable from the diff.</step>
    </procedure>
    <note>The counterpart rule is that destructive shared-tree operations (stash, hard reset, clean, switching a branch in place) are off the table entirely; core-patterns holds that list and the safe alternatives.</note>
  </pattern>
</patterns>

<best_practices>
  <practice priority="critical">Analyze task dependencies before execution to determine parallel vs sequential execution model</practice>
  <practice priority="critical">Provide comprehensive context to sub-agents including file paths, tool usage, and reference implementations</practice>
  <practice priority="critical">Systematically review all phases: initial scan, deep analysis, context evaluation, standards compliance</practice>
  <practice priority="high">Balance critical feedback with positive observations of good practices</practice>
  <practice priority="high">Provide file:line references and concrete improvement suggestions</practice>
  <practice priority="medium">Check Serena memories for existing patterns before delegating implementation tasks</practice>
  <practice priority="critical">Define done as an enumerated set of verification commands exiting zero, and treat a failing hook or gate as unfinished work rather than an obstacle (definition_of_done)</practice>
  <practice priority="critical">Report the verification tier actually reached and name the exact command needed to close the gap (verification_ceiling_reporting)</practice>
  <practice priority="high">Choose the review lens deliberately: a behavioral change needs a behavior review, and a conformance score is not approval (review_lens_selection)</practice>
  <practice priority="high">Inspect working-tree state and attribute every hunk before staging in a shared checkout (shared_repo_dirty_state_check)</practice>
</best_practices>

<anti_patterns>
  <avoid name="nitpicking_style">
    <description>Focusing on code style issues when functionality is broken</description>
    <instead>Address critical and important issues first, style suggestions last</instead>
  </avoid>
  <avoid name="rubber_stamping">
    <description>Approving changes without thorough review</description>
    <instead>Systematically review all phases: scan, deep analysis, context, standards</instead>
  </avoid>
  <avoid name="only_negatives">
    <description>Providing only critical feedback without acknowledging good work</description>
    <instead>Balance feedback with positive observations of good practices</instead>
  </avoid>
  <avoid name="vague_feedback">
    <description>Giving feedback without specific, actionable suggestions</description>
    <instead>Provide file:line references and concrete improvement suggestions</instead>
  </avoid>
  <avoid name="sequential_when_parallel">
    <description>Executing independent tasks sequentially</description>
    <instead>Identify and execute independent tasks in parallel for efficiency</instead>
  </avoid>
  <avoid name="parallel_when_dependent">
    <description>Attempting to parallelize tasks with data dependencies</description>
    <instead>Analyze dependencies and execute dependent tasks sequentially</instead>
  </avoid>
  <avoid name="bypassing_a_failing_gate">
    <description>Skipping a red hook or gate to get the commit through</description>
    <instead>Read the failure as evidence that the work is unfinished and fix the work</instead>
  </avoid>
  <avoid name="implied_verification">
    <description>Reporting completion without saying which checks ran and which did not</description>
    <instead>Name the verification tier reached, list what could not run and why, and give the command to re-run</instead>
  </avoid>
  <avoid name="staging_everything_in_a_shared_tree">
    <description>Staging all changes without attributing them, in a checkout others may be using</description>
    <instead>Attribute each hunk, stage only your own, and ask when a shared file has interleaved changes</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Execute independent tasks in parallel</rule>
  <rule>Never parallelize tasks with data dependencies</rule>
  <rule>Verify sub-agent outputs before integration</rule>
  <rule>Run quality checks after changes</rule>
</rules>

<rules priority="standard">
  <rule>quality + security: Concurrent checks</rule>
  <rule>test + docs: Simultaneous creation when independent</rule>
  <rule>Ensure no regression in existing functionality</rule>
  <rule>Confirm all acceptance criteria met</rule>
</rules>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Sub-agent returns partial results</example>
    <example severity="medium">Sub-agent task fails</example>
    <example severity="high">Critical task cannot be completed</example>
    <example severity="critical">Sub-agent introduces breaking change</example>
  </examples>
</error_escalation>

<constraints>
  <must>Delegate detailed work to sub-agents</must>
  <must>Execute independent tasks in parallel</must>
  <must>Verify outputs before integration</must>
  <avoid>Implementing detailed logic directly</avoid>
  <avoid>Sequential execution of independent tasks</avoid>
  <avoid>Skipping verification of sub-agent outputs</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Use for memory checks and symbol operations during delegation</skill>
  <skill name="investigation-patterns">Use when code review reveals unclear implementation details</skill>
  <skill name="testing-patterns">Use to verify test coverage and quality during review</skill>
  <skill name="test-integrity">Use when a gate reports green: selector, double, and teardown traps that make a suite pass without testing anything</skill>
  <skill name="quality-tools">Use for the verification command catalog and for running checks so their output is trustworthy</skill>
  <skill name="core-patterns">Use for shared working-tree constraints and safe alternatives to destructive Git operations</skill>
</related_skills>

<related_agents>
  <agent name="general-purpose">Execute delegated tasks following this skill's orchestration patterns</agent>
  <agent name="quality-assurance">Review implementation outputs for quality compliance</agent>
  <agent name="validator">Cross-validate task results before integration</agent>
</related_agents>
