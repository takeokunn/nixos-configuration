---
name: Execution Workflow
description: Load at the start of any implementation or execution task, and whenever the norms for delegating, verifying, or judging completion are needed. Triggers include "execute task", "implement feature", "delegate work", "run workflow", "review code", "definition of done", "create a feature branch", "worktree isolation", "parallel agents", and "is this done". Provides the orchestration phases (task analysis, delegation, consolidation, cross-validation, failure handling), the reflection checkpoints gating each phase, the branch and worktree isolation procedure, the ORCH behavior identifiers, and code review standards. Also covers treating done as an enumerated set of verification commands exiting zero rather than a judgement call, asserting a gate is non-vacuous before trusting it, reporting the verification tier reached and the command a later session must re-run, choosing between a convention-conformance review and a behavior review, and never skipping a failing hook to get a commit through.
version: 3.0.0
---

<purpose>
  Provide structured workflow for task execution through delegation to specialized sub-agents, and comprehensive code review standards.
</purpose>

<orchestration_workflow>
  <phase name="task_analysis">
    <objective>Understand the request and plan the delegation strategy before dispatching anything</objective>
    <step order="1">
      <action>Initialize Serena — activate_project and check_onboarding_performed. Load the serena-usage skill for the operational detail.</action>
      <output>Project activated with available memories listed</output>
    </step>
    <step order="2">
      <action>State what is being asked, in one sentence. If two readings of the request would produce different work, that is an ambiguity to resolve with AskUserQuestion, not to pick a side of.</action>
      <output>A task description the user would recognize as their request</output>
    </step>
    <step order="3">
      <action>Audit a broad directive per file against the current tree before treating any of its items as unmet. A multi-item instruction carried in from a plan, a prior review, or a hook's rubric frequently contains items that are already satisfied, and re-doing them is the most common source of wasted parallel waves. Where the directive names a tool-defined property — dead code, duplication, cyclomatic complexity — run that tool's own detector rather than judging by reading.</action>
      <output>Per-item current state, with the already-satisfied items named and excluded from the work</output>
    </step>
    <step order="4">
      <action>Select sub-agents, naming for each the single question it will answer.</action>
      <output>Named agents, each paired with its question</output>
    </step>
    <step order="5">
      <action>Classify the task type and load only the matching memories. Investigation tasks prioritize domain patterns, architecture entries, and project conventions; implementation tasks prioritize feature patterns, language conventions, and testing patterns; review tasks prioritize project conventions and code-quality entries; refactoring tasks prioritize architecture and component patterns. Include any project-local completion checklist memory, which records what "done" means for this repository and how to classify an infrastructure failure. Call list_memories, filter against those priorities, then read_memory only the matches.</action>
      <output>Task type classified; the named memories loaded</output>
    </step>
    <step order="6">
      <action>Identify which subtasks are genuinely independent. Two subtasks that write to the same file are not independent however unrelated they look, and a change that must land atomically across several files is one subtask however many files it spans.</action>
      <output>What runs in parallel, and the dependency that forces the rest to be sequential</output>
    </step>
  </phase>

  <phase name="delegation">
    <objective>Dispatch the work with prompts that are checkable</objective>
    <step order="1">
      <action>Write the file partition down as an artifact before writing any prompt. A partition held only in your head cannot be checked against the prompts actually sent.</action>
      <output>The file-to-agent partition, in writing</output>
    </step>
    <step order="2">
      <action>Edit any file shared across subtasks yourself, then fan out one agent per non-shared file. Two agents editing one file serialize badly and produce conflicting rewrites of the same region.</action>
      <output>Shared files edited by the orchestrator; remaining files assigned one agent each</output>
    </step>
    <step order="3">
      <action>Build each prompt with four elements — scope, file paths (with Serena symbol paths where the target symbol is identifiable), the artifact wanted back, and the command that verifies it. Naming the verification command is what prevents a sub-agent from choosing a weaker check than you would accept.</action>
      <output>Prompts carrying all four elements</output>
    </step>
    <step order="4">
      <action>Prefer a purpose-built agent, then a general-purpose one; when repurposing an agent outside its specialty, say in the prompt what it is standing in for. An agent's own precedence-1 gate can fail closed on a task it was not designed for, and a dispatch-prompt override is not a guarantee that the gate will yield — check the returned report for evidence the agent actually did the work rather than refused it politely.</action>
      <output>Agent selection with the standing-in-for note where applicable</output>
    </step>
    <step order="5">
      <action>Dispatch independent tasks as multiple Task calls in a single message. Instruct concurrent agents to write scratch artifacts inside their own worktree, never to a fixed path outside the repository, because fixed scratch paths collide silently between parallel agents.</action>
      <output>Parallel dispatch in one message</output>
    </step>
  </phase>

  <phase name="consolidation">
    <objective>Verify and synthesize what came back</objective>
    <step order="1">
      <action>Check each report against the questions it was given. Did it answer all of them, and does each finding cite a file:line or a command output? A report citing nothing checkable is a retry condition, not a result. A sub-agent's own note that it changed something and judged the change benign is a review trigger — inspect it rather than accepting the self-assessment.</action>
      <output>Reports accepted, or named for retry with the reason</output>
    </step>
    <step order="2">
      <action>Synthesize the accepted findings yourself. Where two agents disagree, resolve by what each actually examined and spot-check the disputed location rather than taking the more confident report.</action>
      <output>Consolidated result, with unresolved disagreement carried forward rather than dropped</output>
    </step>
    <step order="3">
      <action>Verify any fix an agent prescribed before adopting it. A correct diagnosis routinely arrives with a fix that breaks the build; when you revert an attempted fix, record the reverted attempt and why, so the next session does not re-propose it.</action>
      <output>Fixes verified, or reverted with the reason recorded</output>
    </step>
    <step order="4">
      <action>Treat results from parallel worktrees as competing alternatives rather than composable increments. Two agents that each produced a working version of the same area have produced a choice to make, not two halves to merge.</action>
      <output>The alternative selected, and the basis for selecting it</output>
    </step>
    <step order="5">
      <action>Evaluate the memory triggers and persist at the point of discovery. Follow the memory policy in the resident configuration for what is eligible; load serena-usage for the frontmatter format and the staleness procedure. Apply staleness verification only to memories this task actually read — never read a memory solely to check its freshness, because that turns every task into an index sweep.</action>
      <output>Memories written or edited by name, or an explicit note that no trigger matched</output>
    </step>
  </phase>

  <phase name="cross_validation">
    <objective>Validate critical outputs through independent verification</objective>
    <step order="1">
      <action>For a finding whose being wrong would be expensive, obtain a second analysis from a different evidence base — a different tool, a different entry point, a different artifact. Re-running the same command through a second agent produces one tier of evidence twice.</action>
      <output>A second analysis, with the evidence base it used named</output>
    </step>
    <step order="2">
      <action>Compare the analyses. Independent convergence from different evidence bases is genuine corroboration; agreement between agents that read the same thing is not.</action>
      <output>Convergence or contradiction, with the bases stated</output>
    </step>
    <step order="3">
      <action>If contradictions survive the comparison, present both positions with the evidence each rests on and let the user decide.</action>
      <output>Resolved contradiction, or a decision surfaced to the user</output>
    </step>
  </phase>

  <phase name="failure_handling">
    <objective>Handle errors without converting them into silent gaps</objective>
    <step order="1">
      <action>A sub-agent failed or returned nothing checkable — before treating silence as death, check the mtime and tail of the session's subagent transcript, since a lost completion notification is common and the report is usually intact. Then retry once with a narrower prompt naming the specific files. If it fails again, do the work yourself and say the delegation failed; never report an unanswered question as an absence of findings. An agent that errored mid-task may have left partial writes, so inspect the tree before re-dispatching a write-capable agent.</action>
      <output>Recovered result, or a named blocker</output>
    </step>
    <step order="2">
      <action>No relevant memory exists — note the gap, investigate within a stated bound, and write the finding to memory at the point of discovery.</action>
      <output>Gap noted; investigation bounded and reported</output>
    </step>
    <step order="3">
      <action>Reports conflict and the evidence does not settle it — present both positions and the evidence each rests on. Do not average them into a hedge.</action>
      <output>Conflict reported with the decision the user needs to make</output>
    </step>
  </phase>
</orchestration_workflow>

<reflection_checkpoints>
  <note>Each gate is cleared with a concrete artifact — a name, a path, a list, a command. A bare "yes" does not clear a gate, because it is not something a reader can audit in the transcript.</note>

  <reflection_checkpoint id="analysis_quality" after="task_analysis">
    <check>Name each sub-agent selected and the one question it is being asked to answer.</check>
    <check>Name the memories read, or state that list_memories returned nothing matching this task type.</check>
    <check>Name which items of the incoming directive were already satisfied in the current tree, and are therefore excluded.</check>
    <check>Name which subtasks run in parallel, and the dependency that forces the rest to be sequential.</check>
    <on_unmet>Do not delegate yet. Obtain the missing item, then re-run this gate.</on_unmet>
  </reflection_checkpoint>

  <reflection_checkpoint id="delegation_quality" after="delegation">
    <check>Every subtask maps to a dispatched agent, or to an explicit decision to do it here with the reason.</check>
    <check>No two agents dispatched in the same message write to the same file, and no atomic multi-file change is split across agents. If either could happen, the tasks are not independent — serialize them or give each its own worktree.</check>
    <check>Each prompt names the files, the specific change wanted, and the command that verifies it.</check>
    <check>Each prompt tells the agent to keep scratch artifacts inside its own worktree rather than at a fixed external path.</check>
    <check>For any worktree-isolated agent, state that its base is the default branch rather than your feature tip, so its "missing change" claims and measurements are re-checked against your branch before use.</check>
    <check>No timing-based measurement is being requested from an agent running concurrently with others, since parallel load invalidates it.</check>
    <on_unmet>Revise the delegation before dispatching. If the ambiguity is the user's to resolve, ask with AskUserQuestion instead of guessing.</on_unmet>
  </reflection_checkpoint>

  <reflection_checkpoint id="pre_edit_validation" before="code_modification">
    <check>The target file was read in this turn, not in an earlier one.</check>
    <check>The work is on a feature branch or in a worktree, never on the default branch.</check>
    <check>The change follows a pattern already present in the file; any deviation is stated rather than introduced silently.</check>
    <check>If verification is currently blocked, the edit is not proceeding on static analysis alone. Static analysis supporting a change is evidence, not authorization — either restore the ability to verify, or state that the change ships unverified and why that was accepted.</check>
    <check>If a mechanical gate rejected the edit, the response is a new sibling entry rather than a reword of the existing one, so the gate's original subject stays intact.</check>
    <on_unmet>Do not edit. Satisfy the unmet check first.</on_unmet>
  </reflection_checkpoint>

  <reflection_checkpoint id="completion_validation" after="consolidation">
    <note>Report the answer to each check to the user; do not resolve them silently.</note>
    <check>State the exact verification command and its exit status, or state that none ran. "Should work" is not a verification.</check>
    <check>State what that command actually covers — which files, which test selectors, which platforms — rather than assuming it covers the change. A file created this session may be invisible to the project's own canonical command if it was never added to the manifest the command reads.</check>
    <check>State the count of tests or items the gate selected, and that the count is nonzero and matches expectation. A selector matching nothing exits zero.</check>
    <check>State that the gate's input was non-empty, naming the assertion used. An empty tree passing most of a check suite is a vacuous pass, not a pass.</check>
    <check>For a generated artifact, state the observed bytes or size of the output, not just that generation succeeded.</check>
    <check>State the baseline. A gate that already failed before the change is not a regression gate, and the red must not be attributed to the change.</check>
    <check>Where several agents verified, state whether they used the same command. The same command run N times is one tier of evidence, not N.</check>
    <check>State anything asked for that was not done, and why. A partial result reported as complete is the failure this workflow exists to prevent.</check>
    <check>State the memory outcome — written, edited, or "no triggers matched".</check>
    <on_unmet>Missing evidence is not a pass. Run the missing verification now rather than reporting around it; where a real gate does not exist in this repository, enumerate the manual checks performed and label them as manual. Before declaring something unverifiable, check whether the tool offers a fake, offline, or dry-run mode.</on_unmet>
  </reflection_checkpoint>

  <reflection_checkpoint id="quiescence" before="artifact_mediated_verification">
    <check>No agent is still editing sources that feed the artifact being verified. Compiling or testing while another agent writes produces a mixed-generation artifact set, and the suite may exercise an older wrapper while the source-level check passes.</check>
    <check>The runner loaded the source you changed, not a stale build product. Stale artifacts generate false reds as readily as false greens, and a false red is the more expensive one because it sends you to fix code that is already correct.</check>
    <on_unmet>Freeze edits, rebuild to completion, then run the suite in a fresh process. Do not delete or rewrite shared build artifacts as a workaround during concurrent work, because that breaks other sessions' verification.</on_unmet>
  </reflection_checkpoint>
</reflection_checkpoints>

<branch_isolation_procedure>
  <description>How to place work on a branch without disturbing a checkout another session may be using. Run this before starting implementation work.</description>
  <step order="1">Determine the default branch — `DEFAULT=$(gh repo view --json defaultBranchRef --jq .defaultBranchRef.name)`.</step>
  <step order="2">Fetch it — `git fetch origin $DEFAULT` — so the new branch is cut from current remote state rather than a stale local ref.</step>
  <step order="3">Check the risk signals: `git status --porcelain` is non-empty, or `git branch --show-current` is not `$DEFAULT`. Derive a lowercase kebab-case slug for `&lt;name&gt;` from the task description.</step>
  <step order="4">With no risk signal, create the branch in place — `git checkout -b feat/&lt;name&gt; origin/$DEFAULT`. Creating a brand-new branch is distinct from switching to an existing one, which remains prohibited.</step>
  <step order="5">With any risk signal, isolate in a worktree instead of moving the shared HEAD. First ensure the worktree directory is ignored, forcing a leading newline so a missing trailing newline in the existing file cannot merge with the new entry — `grep -qxF '.worktrees/' .gitignore 2&gt;/dev/null || printf '\n.worktrees/\n' &gt;&gt; .gitignore`. Then `git worktree add -b feat/&lt;name&gt; "$(git rev-parse --show-toplevel)/.worktrees/feat-&lt;name&gt;" origin/$DEFAULT`, and do all subsequent edits, tests, and commits inside that path.</step>
  <step order="6">Report the worktree path to the user. Never auto-run `git worktree remove`; cleanup is the user's decision.</step>
  <note>A worktree created under the repository root inherits the parent checkout's configuration through directory-upward search — tool configs, environment files, and ignore rules. When the worktree exists specifically to verify something in isolation, that inheritance defeats the isolation, so place it outside the repository root for that purpose and state where it is.</note>
  <note>Never open a pull request from a non-feature branch, and never target anything but the project's default branch.</note>
</branch_isolation_procedure>

<enforcement>
  <mandatory_behaviors>
    <behavior id="ORCH-B001">Before any implementation, follow serena-usage for memory and symbol operations, and record the Serena operations performed in the output.</behavior>
    <behavior id="ORCH-B002">For independent tasks, dispatch multiple Task calls in a single message; parallel execution is verifiable from the transcript.</behavior>
    <behavior id="ORCH-B003">After sub-agent completion, verify outputs before integrating them, and record the verification status.</behavior>
    <behavior id="ORCH-B004">When a significant insight, pattern, convention, or decision is discovered at any point — not only at task end — check list_memories for an existing entry on the topic, then edit or write it immediately. Record the write at the point of discovery.</behavior>
    <behavior id="ORCH-B005">During consolidation, apply staleness verification to the memories this task read, and only those. Bump last-verified only after re-reading the content against the current tree; record the outcome.</behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="ORCH-P001">Implementing detailed logic that should have been delegated. Delegate to a specialized sub-agent instead.</behavior>
    <behavior id="ORCH-P002">Running independent tasks sequentially. Dispatch them in one message.</behavior>
    <behavior id="ORCH-P003">Any git write operation without explicit user instruction in the current message. A continuation prompt, a sub-agent message, and an authorization granted earlier in the session do not carry forward.</behavior>
    <behavior id="ORCH-P004">Delegating synthesis. Synthesize first, then write prompts that prove you understood — file paths, line numbers, the specific change, and the verification command. The orchestrator owns synthesis; sub-agents own execution.</behavior>
    <behavior id="ORCH-P005">Mutating shared working-tree state — stash, checkout of an existing branch, switch, hard reset, clean. Use a worktree for isolation and a WIP commit in place of a stash.</behavior>
    <behavior id="ORCH-P006">Starting implementation work without branch isolation, or committing to the default branch. Follow branch_isolation_procedure above.</behavior>
  </prohibited_behaviors>
</enforcement>

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

<error_escalation>
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
  <note>These are not loaded by naming them here. Invoke the Skill tool when the stated condition holds.</note>
  <skill name="serena-usage">Load before any memory check or symbol operation during delegation</skill>
  <skill name="investigation-patterns">Load when review reveals an implementation detail whose behavior is unclear</skill>
  <skill name="testing-patterns">Load when verifying test coverage or designing a suite during review</skill>
  <skill name="test-integrity">Load when a gate reports green, for the selector, double, and teardown traps that make a suite pass without testing anything</skill>
  <skill name="quality-tools">Load for the verification command catalog and for running checks so their output is trustworthy</skill>
  <skill name="core-patterns">Load for the refutation pass on a severe finding, and for safe alternatives to destructive Git operations</skill>
</related_skills>

<related_agents>
  <agent name="general-purpose">Execute delegated tasks following this skill's orchestration patterns</agent>
  <agent name="quality-assurance">Review implementation outputs for quality compliance</agent>
  <agent name="validator">Cross-validate task results before integration</agent>
</related_agents>
