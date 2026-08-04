---
name: Workflow Patterns
description: Patterns for output formats, reflection checkpoints, agent references, and self-evaluation shared across agents and commands.
version: 3.0.0
---

<purpose>
  Provide standardized patterns for output formatting, workflow checkpoints, agent references, and self-evaluation shared across agents and commands.
</purpose>

<tools>
  <tool name="output_format_template">
    <description>Standard agent output format with status criteria</description>
    <use_case>Include in agents for consistent output structure</use_case>
  </tool>

  <tool name="reflection_checkpoint_template">
    <description>Standard analysis quality checkpoint for workflow phases</description>
    <use_case>Include in workflows for consistent quality gates</use_case>
  </tool>

  <tool name="self_evaluate_template">
    <description>Standard self-evaluation phase for agents producing reports</description>
    <use_case>Include in agents for consistent quality assessment</use_case>
  </tool>

  <tool name="prepare_phase_template">
    <description>Standard Serena initialization phase for commands</description>
    <use_case>Include at start of command workflows for Serena initialization</use_case>
  </tool>
</tools>

<patterns>
  <pattern name="output_format">
    <description>Standard agent output format. Every finding carries the evidence that backs it, so
      the reader can check the report rather than trust a number it asserts about itself.</description>
    <example>
<output>
  <format>
{
  "status": "success|warning|error",
  "status_criteria": "inherits core-patterns#status_determination",
  "summary": "What was asked, what was found, and what remains unchecked",
  "verification": "The exact command(s) run and their exit status, or \"none run\" — never omitted",
  "findings": [
    {
      "claim": "...",
      "evidence_tier": "verified|inferred|assumed",
      "evidence": "file.ts:42, or the command whose output shows this",
      "detail": "..."
    }
  ],
  "gaps": ["Anything asked for that was not done, and why"],
  "next_actions": []
}
  </format>
</output>
    </example>
    <rule>`gaps` is not optional. An empty array is a claim that nothing was left undone, and it is
      checkable; omitting the field hides the question.</rule>
  </pattern>

  <pattern name="output_status_criteria">
    <description>Status criteria for agent output, defined by the state of the evidence rather than by
      a self-assigned score. Full definitions in core-patterns#status_determination.</description>
    <example>
"status_criteria": {
  "success": "Every check the task set out to make was made, and none failed",
  "warning": "Completed, but a check could not be run or a gap remains — the gap is named in summary",
  "error": "A blocker prevented the core question from being answered, or a check failed"
}
    </example>
  </pattern>

  <pattern name="reflection_checkpoint">
    <description>A quality gate between workflow phases. Each check must be answerable with an
      artifact — a path, a command, a name — so that failing it is visible in the transcript.</description>
    <example>
<reflection_checkpoint id="analysis_quality">
  <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
  <check>Name the files read and the specific lines the conclusion rests on.</check>
  <check>Name what is still unknown, or state that nothing material is.</check>
  <on_unmet>Gather the missing evidence before proceeding. If only the user can supply it, ask with
    AskUserQuestion rather than assuming.</on_unmet>
</reflection_checkpoint>
    </example>
    <rule>Phrase checks so they can fail. "Have I gathered sufficient evidence?" cannot — it is
      answered yes by whatever evidence was gathered. "Name the files read" can.</rule>
  </pattern>

  <pattern name="prepare_phase">
    <description>Standard Serena initialization phase for workflows</description>
    <example>
<phase name="prepare">
  <objective>Initialize Serena and load the memories that apply to this task type</objective>
  <step order="1">
    <action>Activate the project</action>
    <tool>Serena activate_project, check_onboarding_performed</tool>
    <output>Project active; onboarding status known</output>
  </step>
  <step order="2">
    <action>List memories and filter by task type per serena-usage#memory_reading_by_task_type</action>
    <tool>Serena list_memories</tool>
    <output>Named shortlist, or an explicit "nothing matched"</output>
  </step>
  <step order="3">
    <action>Read only the shortlisted entries</action>
    <tool>Serena read_memory</tool>
    <output>The memories read, named in the report so the reader knows what informed the work</output>
  </step>
</phase>
    </example>
  </pattern>

  <pattern name="failure_handling">
    <description>Failure-handling phase template for exceptional paths aligned with shared workflow usage</description>
    <example>
<phase name="failure_handling">
  <objective>Handle errors and edge cases gracefully</objective>
  <step order="1">If tool call fails: log error and attempt alternative approach</step>
  <step order="2">If data unavailable: document gap and continue with bounded analysis</step>
  <step order="3">If contradictory evidence: flag uncertainty and request clarification</step>
</phase>
    </example>
  </pattern>

  <pattern name="agent_ref">
    <description>Standard agent reference syntax for commands</description>
    <example>
<agents>
  <agent ref="explore" readonly="true" />
  <agent ref="design" readonly="true" />
</agents>

Use ref attribute to reference agent defined in agents/ directory.
readonly attribute indicates whether agent can modify files.
    </example>
  </pattern>

  <pattern name="self_evaluate_phase">
    <description>Final pass before returning a report. It looks for what is missing from the report,
      which is something a model can actually do, rather than rating what is present, which it cannot.</description>
    <example>
<phase name="self_evaluate">
  <objective>Find what the report claims but did not establish</objective>
  <step order="1">
    <action>Re-read the report and tag each finding verified, inferred, or assumed. Any finding tagged
      verified must name the command or the file:line that backs it; if it cannot, downgrade it.</action>
    <output>Findings tagged, over-claims downgraded</output>
  </step>
  <step order="2">
    <action>List anything the request asked for that the report does not answer, and say why —
      not attempted, blocked, or judged out of scope.</action>
    <output>Gap list, possibly empty</output>
  </step>
  <step order="3">
    <action>Set status per core-patterns#status_determination from what steps 1 and 2 found, and append
      the self_feedback section.</action>
    <output>Status and self_feedback</output>
  </step>
</phase>
    </example>
  </pattern>

  <pattern name="convention_adoption_gate">
    <description>A convention is not adopted until a machine gate enforces it. A rule that lives only in a document is advisory, and it erodes at the rate new code is written — so the definition of done for adding a rule includes its enforcement mechanism, not just the prose.</description>
    <rule>When adding a coding or process convention, the task is complete only once a matching automated check exists and runs in the project's normal verification set.</rule>
    <rule>If a rule cannot be mechanically checked, reconsider stating it. An unenforceable rule costs review attention on every change and buys compliance only while someone remembers it.</rule>
    <gate_categories>
      <category name="formatting_and_lint">Style and idiom rules, enforced by the project's formatter and linter configuration rather than by review comments</category>
      <category name="boundary_checks">Import and layering constraints, enforced by a dependency or import-boundary checker</category>
      <category name="unused_surface">Dead export and unreachable code detection, so a removal convention stays true over time</category>
      <category name="project_policy">Rules no off-the-shelf tool knows about, written as a test in the normal suite (see quality-tools for how to author one without it becoming noisy)</category>
    </gate_categories>
    <example>
      <note>Adding "all new modules must declare explicit exports"</note>
      <note>Not done: the rule is written in the conventions document</note>
      <note>Done: the rule is written down AND a lint rule or boundary check fails on a module that violates it</note>
    </example>
  </pattern>

  <pattern name="self_feedback_output">
    <description>Self-feedback section appended by commands that run self_evaluate_phase</description>
    <example>
<self_feedback>
  <verification>The command(s) actually run and their exit status, or "none run"</verification>
  <weakest_claim>The finding resting on the thinnest evidence, and what would confirm it</weakest_claim>
  <gaps>
    - Asked for but not done, with the reason (omit the element only if there are none)
  </gaps>
</self_feedback>
    </example>
    <rule>Name the weakest claim, not the overall quality. "Which part of this is most likely wrong"
      has an answer the model can find; "how good is this out of 100" does not.</rule>
  </pattern>
</patterns>

<best_practices>
  <practice priority="critical">Use output_format for all agents that return structured results</practice>
  <practice priority="critical">Include reflection_checkpoint at key workflow decision points</practice>
  <practice priority="critical">Include prepare_phase for Serena initialization in commands</practice>
  <practice priority="high">Add self_evaluate_phase for agents producing reports or recommendations</practice>
  <practice priority="high">Use failure_handling phase in all workflows</practice>
  <practice priority="medium">Use agent_ref syntax for consistent agent references in commands</practice>
  <practice priority="high">Treat a new convention as unadopted until a machine gate enforces it, and reconsider any rule that cannot be mechanically checked (convention_adoption_gate)</practice>
</best_practices>

<rules priority="critical">
  <rule>Output status follows core-patterns#status_determination — the state of the evidence, not a score</rule>
  <rule>Every reflection checkpoint check must be answerable with an artifact, and must be able to fail</rule>
  <rule>Commands must include prepare_phase for Serena initialization</rule>
</rules>

<rules priority="standard">
  <rule>Include failure_handling phase in complex workflows</rule>
  <rule>Use self_feedback_output format for self-evaluation results</rule>
  <rule>Use agent_ref with readonly attribute for clarity</rule>
</rules>

<constraints>
  <must>Use output_status_criteria as defined here for every structured output</must>
  <must>Tag every finding with an evidence tier and the evidence itself</must>
  <must>Include the verification field — the command run, or "none run"</must>
  <must>Include prepare_phase in command workflows</must>
  <avoid>Confidence scores and numeric self-gating in any output or checkpoint</avoid>
  <avoid>Omitting failure_handling in complex workflows</avoid>
  <avoid>Omitting Serena initialization in commands</avoid>
</constraints>

<related_skills>
  <skill name="core-patterns">Base templates for error escalation, decision criteria, enforcement</skill>
  <skill name="parallelization-patterns">Parallel execution and timeout configuration</skill>
  <skill name="serena-usage">Serena MCP operations (memory, symbol search, code navigation)</skill>
  <skill name="quality-tools">Tool catalog and project-local policy gates that make a convention enforceable</skill>
</related_skills>

<related_agents>
  <agent name="validator">Verify workflow consistency and checkpoint completeness</agent>
</related_agents>
