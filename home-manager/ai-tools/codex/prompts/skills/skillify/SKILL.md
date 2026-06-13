---
name: skillify
description: Capture session as reusable skill
---

<purpose>
Analyze the current session and extract a reusable skill (SKILL.md) from the work performed. Conducts a structured 4-round interview to clarify the skill scope, then writes the SKILL.md file after user confirmation.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="tools">serena-usage</skill>
</refs>
<rules priority="critical">
  <rule>Never write the SKILL.md file without explicit user confirmation after presenting the draft</rule>
  <rule>Conduct exactly 4 rounds of interview questions; do not skip or extend</rule>
  <rule>The skill must be generalized from the session — not a session transcript</rule>
  <rule>The skill must be independently executable: a new agent with only the SKILL.md should be able to perform the task</rule>
</rules>
<rules priority="standard">
  <rule>Use AskUserQuestion for all interview rounds to enable structured responses</rule>
  <rule>Write to ~/.codex/skills/<skill-name>/SKILL.md (or the local skills directory)</rule>
  <rule>Infer skill name from the session if the argument is empty; confirm with user</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Writing a skill as a session transcript — skills must be generalized patterns, not "what we did today"</practice>
    <practice>Asking every possible question before drafting — 4 focused rounds are sufficient; over-interviewing delays the draft</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Generalize from concrete session evidence: identify the repeatable pattern, not the one-off specifics</principle>
    <principle>A skill is independently executable — every section (purpose, workflow, constraints) must be self-contained with no implicit dependencies on the current session's context</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="analyze">
    <step order="1">
      <action>Analyze the current session: what problem was solved, what workflow was used, what decisions were made</action>
      <tool>Conversation context analysis</tool>
      <output>Session summary: problem domain, workflow steps, key decisions</output>
    </step>
    <step order="2">
      <action>Identify the generalizable pattern: what repeatable skill emerges from this session?</action>
      <tool>Pattern extraction</tool>
      <output>Candidate skill name and high-level purpose</output>
    </step>
  </phase>
  <phase name="interview">
    <step order="1">
      <action>Round 1: Confirm skill name and purpose scope with user</action>
      <tool>AskUserQuestion</tool>
      <output>Confirmed skill name and purpose</output>
    </step>
    <step order="2">
      <action>Round 2: Clarify workflow steps — what are the mandatory phases and their order?</action>
      <tool>AskUserQuestion</tool>
      <output>Workflow phases confirmed</output>
    </step>
    <step order="3">
      <action>Round 3: Identify constraints — what must always happen, what must never happen?</action>
      <tool>AskUserQuestion</tool>
      <output>Rules and constraints confirmed</output>
    </step>
    <step order="4">
      <action>Round 4: Identify edge cases and failure modes — when does this skill not apply?</action>
      <tool>AskUserQuestion</tool>
      <output>Scope boundaries and failure modes confirmed</output>
    </step>
  </phase>
  <phase name="draft">
    <step order="1">
      <action>Draft SKILL.md using confirmed interview answers; include all required sections</action>
      <tool>Content generation</tool>
      <output>Draft SKILL.md presented to user</output>
    </step>
    <step order="2">
      <action>Ask user to confirm or request revisions before writing</action>
      <tool>AskUserQuestion (confirm/revise/cancel)</tool>
      <output>User decision</output>
    </step>
  </phase>
  <phase name="write">
    <step order="1">
      <action>If user confirmed: write SKILL.md to the appropriate skills directory</action>
      <tool>Write tool (conditional on user confirmation)</tool>
      <output>SKILL.md written with confirmation of path</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<skill_md_template>
  <required_sections>
    <section>name (frontmatter)</section>
    <section>description (frontmatter)</section>
    <section>purpose</section>
    <section>rules (critical and standard)</section>
    <section>workflow (phases with steps)</section>
    <section>constraints</section>
  </required_sections>
  <optional_sections>
    <section>refs</section>
    <section>agents</section>
    <section>output</section>
    <section>ai_principles</section>
  </optional_sections>
</skill_md_template>
<output>
  <format>
    <draft_presentation>Full SKILL.md draft shown to user before writing</draft_presentation>
    <confirmation>File path written after user approval</confirmation>
  </format>
</output>
<constraints>
  <must>Conduct exactly 4 interview rounds using AskUserQuestion</must>
  <must>Present the draft to the user and receive explicit confirmation before writing</must>
  <must>Generalize the skill — not a session transcript</must>
  <must>Include all required SKILL.md sections in the draft</must>
  <avoid>Writing the file without user confirmation</avoid>
  <avoid>Skipping interview rounds</avoid>
  <avoid>Including session-specific details that won't generalize</avoid>
</constraints>
