---
argument-hint: [previous-command]
description: Review command for Claude Code's recent work
---

<purpose>
Multi-faceted review of Claude Code's work within the same session, automatically selecting appropriate review mode and executing efficiently in parallel.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">execution-workflow</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<rules priority="critical">
  <rule>Launch all Task tools simultaneously in one message (timeout avoidance)</rule>
  <rule>Auto-select mode based on previous command</rule>
  <rule>Review only changed code in execute mode, not existing issues</rule>
  <rule>Provide concrete fix proposals, not abstract theories</rule>
</rules>
<rules priority="standard">
  <rule>Use execution-workflow skill for code review methodology</rule>
  <rule>Check Serena memories for existing patterns</rule>
  <rule>Target session operations, not git diff</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Reviewing work sequentially across quality dimensions before synthesizing — AI can launch all review agents (quality, security, design, docs, performance, test) simultaneously in a single pass</practice>
    <practice>Providing general impressions without anchoring to code — every feedback item must cite a specific file:line reference, replacing vague commentary with precise evidence</practice>
    <practice>Waiting for user to triage and prioritize issues — AI should assign severity and priority levels automatically based on impact assessment</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Parallelize all review dimensions simultaneously; no dimension should block another since they operate on the same diff</principle>
    <principle>Anchor each finding to a specific location (file:line) and include a concrete fix proposal — never report issues without accompanying remediation</principle>
    <principle>Calibrate severity levels by actual runtime impact, not style preference; distinguish critical (data loss/security) from warning (degraded behavior) from info (style)</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Activate Serena project with activate_project</action>
      <tool>Serena activate_project</tool>
      <output>Project activated</output>
    </step>
    <step order="2">
      <action>Check list_memories for relevant patterns</action>
      <tool>Serena list_memories</tool>
      <output>Full memory index</output>
    </step>
    <step order="3">
      <action>Classify task type as "review". Apply memory_reading_by_task_type filter
        (serena-usage skill): prioritize {project}-conventions → code-quality-* → architecture-*.
        Filter the memory index from step 2 against these categories; record matched names.</action>
      <tool>serena-usage#memory_reading_by_task_type (reference only)</tool>
      <output>Filtered priority memory list for review tasks</output>
    </step>
    <step order="4">
      <action>Load only memories matching the prioritized categories with read_memory;
        skip categories absent from the index</action>
      <tool>Serena read_memory</tool>
      <output>Prioritized patterns loaded</output>
    </step>

  </phase>
  <phase name="analyze">
    <step order="1">
      <action>What was the previous command? (/define, /execute, /bug, /ask, other)</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>What files/work need to be reviewed?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="3">
      <action>Which agents should run in parallel?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="4">
      <action>What metrics are relevant for this mode?</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <phase name="select">
    <step order="1">
      <action>Determine mode based on previous command</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>After /define: Execution plan feedback</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="3">
      <action>After /execute: Work content feedback</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="4">
      <action>After /bug: Investigation quality feedback</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="5">
      <action>After /ask: Answer accuracy feedback</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="6">
      <action>Other: Recent work feedback</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
  <phase name="execute">
    <step order="1">
      <action>Launch all agents in parallel</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>
    <step order="2">
      <action>Collect agent results</action>
      <tool>Task tool and Serena read/search tools as needed</tool>
      <output>Step result recorded for the phase</output>
    </step>

  </phase>
  <reflection_checkpoint id="review_quality">
    <question>Did all agents complete successfully?</question>
    <question>Is the feedback specific and actionable?</question>
    <question>Have I assigned priority levels to all issues?</question>
    <threshold>If confidence less than 70, gather additional context or re-run agents</threshold>
  </reflection_checkpoint>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Detect and classify failures during command execution</action>
      <tool>Error analysis and severity assessment</tool>
      <output>Failure classification and impact summary</output>
    </step>
    <step order="2">
      <action>Apply recovery path or escalate with concrete blocker details</action>
      <tool>Retry policy and fallback strategy</tool>
      <output>Recovered flow or explicit blocker report</output>
    </step>
  </phase>
  <phase name="persist">
    <objective>Capture reusable review patterns to Serena memory</objective>
    <step order="1">
      <action>Evaluate memory_auto_creation_triggers: did this review reveal a recurring quality issue,
        a reusable review pattern, or a project convention worth recording?
        Call list_memories to check if a memory for this topic already exists.</action>
      <tool>Serena list_memories, evaluation against trigger list</tool>
      <output>Trigger match: yes/no; existing memory: yes/no</output>
    </step>
    <step order="2">
      <action>If trigger matched: use edit_memory (existing topic) or write_memory (new topic).
        For write_memory: prepend memory_content_format frontmatter (serena-usage skill)
        with domain, status=active, created=YYYY-MM, last-verified=YYYY-MM.
        For edit_memory on a memory lacking frontmatter: add it, updating last-verified.
        If no trigger matched: output "persist: no triggers matched — skip"</action>
      <tool>Serena edit_memory or write_memory</tool>
      <output>Memory entry updated with frontmatter (name listed), or explicit skip reason</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<modes>
  <mode name="define">
    <target>Execution plan from conversation history</target>
    <aspects>Step granularity, dependencies, risk identification, completeness, feasibility</aspects>
    <agents>
      <agent name="plan" subagent_type="general-purpose" readonly="true">Execution plan review</agent>
      <agent name="estimation" subagent_type="general-purpose" readonly="true">Estimation validity review</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="execute">
    <target>Files modified via Edit/Write tools</target>
    <agents>
      <agent name="quality" subagent_type="quality-assurance" readonly="true">Naming, DRY, readability</agent>
      <agent name="security" subagent_type="security" readonly="true">OWASP Top 10, input validation, auth</agent>
      <agent name="design" subagent_type="design" readonly="true">Architecture consistency, patterns</agent>
      <agent name="docs" subagent_type="docs" readonly="true">Accuracy, structure, completeness</agent>
      <agent name="performance" subagent_type="performance" readonly="true">Performance review</agent>
      <agent name="test" subagent_type="test" readonly="true">Test coverage review</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="general">
    <target>Recent Claude Code work</target>
    <agents>
      <agent name="review" subagent_type="quality-assurance" readonly="true">Comprehensive work review</agent>
      <agent name="complexity" subagent_type="code-quality" readonly="true">Code complexity review</agent>
      <agent name="memory" subagent_type="general-purpose" readonly="true">Consistency check with existing patterns</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="bug">
    <target>Investigation results from conversation history</target>
    <aspects>Evidence collection, hypothesis validity, root cause accuracy, log utilization</aspects>
    <metrics>Confidence (0-100), Log Utilization (0-100), Objectivity (0-100)</metrics>
    <agents>
      <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">Investigation methodology evaluation</agent>
      <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Log analysis and dependency investigation evaluation</agent>
      <agent name="explore" subagent_type="explore" readonly="true">Code path coverage evaluation</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="ask">
    <target>Answer and evidence from conversation history</target>
    <aspects>Evidence citation quality, conclusion validity, reference accuracy, confidence calibration</aspects>
    <metrics>Confidence (0-100), Evidence Coverage (0-100)</metrics>
    <note>Subset of ask.md agents focused on answer evaluation; design/performance agents omitted as they evaluate questions, not answers</note>
    <agents>
      <agent name="explore" subagent_type="explore" readonly="true">Evidence gathering evaluation</agent>
      <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">Answer accuracy assessment</agent>
      <agent name="code-quality" subagent_type="code-quality" readonly="true">Reference precision and conclusion validity</agent>
    </agents>
    <fact_check>Use fact-check skill patterns for external source verification via Context7</fact_check>
    <execution>All agents in parallel</execution>
  </mode>
</modes>
<agents>
  <agent name="plan" subagent_type="general-purpose" readonly="true">Execution plan review</agent>
  <agent name="estimation" subagent_type="general-purpose" readonly="true">Estimation validity review</agent>
  <agent name="quality" subagent_type="quality-assurance" readonly="true">Naming, DRY, readability</agent>
  <agent name="security" subagent_type="security" readonly="true">OWASP Top 10, input validation, auth</agent>
  <agent name="design" subagent_type="design" readonly="true">Architecture consistency, patterns</agent>
  <agent name="docs" subagent_type="docs" readonly="true">Accuracy, structure, completeness</agent>
  <agent name="performance" subagent_type="performance" readonly="true">Performance review</agent>
  <agent name="test" subagent_type="test" readonly="true">Test coverage review</agent>
  <agent name="review" subagent_type="quality-assurance" readonly="true">Comprehensive work review</agent>
  <agent name="complexity" subagent_type="code-quality" readonly="true">Code complexity review</agent>
  <agent name="memory" subagent_type="general-purpose" readonly="true">Consistency check with existing patterns</agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Log analysis and dependency investigation evaluation</agent>
  <agent name="explore" subagent_type="explore" readonly="true">Evidence and code path coverage evaluation</agent>
  <agent name="code-quality" subagent_type="code-quality" readonly="true">Reference precision and conclusion validity</agent>
</agents>
<execution_graph>
  <sequential_phase id="mode_selection" depends_on="none">
    <action>Select one review mode from the modes section based on the previous command</action>
  </sequential_phase>
  <parallel_group id="selected_review" depends_on="mode_selection">
    <agent>Agents listed in the selected mode</agent>
  </parallel_group>
  <sequential_phase id="synthesis" depends_on="selected_review">
    <action>Compile the review report with metrics, findings, and recommended actions</action>
  </sequential_phase>
</execution_graph>
<decision_criteria inherits="core-patterns#decision_criteria">
  <criterion name="confidence_calculation">
    <factor name="review_depth" weight="0.4">
      <score range="90-100">All code paths and edge cases reviewed</score>
      <score range="70-89">Main code paths reviewed</score>
      <score range="50-69">Surface level review</score>
      <score range="0-49">Minimal review</score>
    </factor>
    <factor name="feedback_actionability" weight="0.3">
      <score range="90-100">All feedback is specific and actionable</score>
      <score range="70-89">Most feedback actionable</score>
      <score range="50-69">Some vague feedback</score>
      <score range="0-49">Mostly vague feedback</score>
    </factor>
    <factor name="issue_prioritization" weight="0.3">
      <score range="90-100">Clear priority levels with rationale</score>
      <score range="70-89">Priority levels assigned</score>
      <score range="50-69">Partial prioritization</score>
      <score range="0-49">No prioritization</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="success_case">
      <input>review_depth=93, feedback_actionability=92, issue_prioritization=92</input>
      <calculation>(93*0.4)+(92*0.3)+(92*0.3) = 92.4</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>review_depth=80, feedback_actionability=80, issue_prioritization=80</input>
      <calculation>(80*0.4)+(80*0.3)+(80*0.3) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>review_depth=79, feedback_actionability=79, issue_prioritization=79</input>
      <calculation>(79*0.4)+(79*0.3)+(79*0.3) = 79</calculation>
      <expected_status>warning</expected_status>
      <reasoning>79 is below success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>review_depth=59, feedback_actionability=59, issue_prioritization=59</input>
      <calculation>(59*0.4)+(59*0.3)+(59*0.3) = 59</calculation>
      <expected_status>error</expected_status>
      <reasoning>59 is at error threshold</reasoning>
    </test>
    <test name="error_case">
      <input>review_depth=35, feedback_actionability=45, issue_prioritization=40</input>
      <calculation>(35*0.4)+(45*0.3)+(40*0.3) = 39.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores yield error status</reasoning>
    </test>
  </validation_tests>
</decision_criteria>
<output>
  <format>
    <feedback_results mode="{Mode}">
      <evaluation_scores>
- {Metric1}: XX/100
- {Metric2}: XX/100
- Overall: XX/100</evaluation_scores>
      <critical>Immediate Fix Required
- [Category] Issue: Location
- Problem: Description
- Fix: Proposal</critical>
      <warning>Fix Recommended
- [Category] Issue: Location
- Problem: Description
- Recommendation: Proposal</warning>
      <good_practice>[Category] Commendable aspects</good_practice>
      <fact_check_results>
        <verified_claims>Claims confirmed against external sources (Context7, WebSearch)</verified_claims>
        <flagged_claims>Claims with verification confidence below 80
- Claim: {claim}
- Source referenced: {source}
- Verification result: {result}
- Confidence: {XX}/100
- Evidence: {evidence}
- Recommendation: {correction}</flagged_claims>
        <unverifiable_claims>Claims that could not be checked due to unavailable sources</unverifiable_claims>
      </fact_check_results>
      <recommended_actions>
- [High] Action
- [Medium] Action
- [Low] Action</recommended_actions>
    </feedback_results>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="FB-B001" priority="critical">
      <trigger>When providing feedback</trigger>
      <action>Include specific file:line references</action>
      <verification>References in all feedback items</verification>
    </behavior>
    <behavior id="FB-B002" priority="critical">
      <trigger>When identifying issues</trigger>
      <action>Provide suggested improvements</action>
      <verification>Suggestions for each issue</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="FB-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Providing feedback without code analysis</action>
      <response>Block feedback, require analysis first</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor code quality issue in reviewed work</example>
    <example severity="medium">Unclear quality metric or missing test coverage</example>
    <example severity="high">Critical security flaw or major design issue in reviewed work</example>
    <example severity="critical">Data loss risk or security breach in reviewed work</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="execute">Primary target for feedback after implementation</command>
  <command name="define">Feedback on execution plans</command>
  <command name="bug">Feedback on investigation quality</command>
  <command name="ask">Feedback on answer accuracy</command>
  <command name="upstream">Review before submitting upstream PR</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="execution-workflow">Understanding work review methodology</skill>
  <skill name="investigation-patterns">Evaluating evidence quality in investigations</skill>
  <skill name="testing-patterns">Assessing test coverage and quality</skill>
  <skill name="fact-check">Verifying external source claims</skill>
</related_skills>
<constraints>
  <must>Launch all agents simultaneously (no sequential execution)</must>
  <must>Review only changed code in execute mode</must>
  <must>Provide concrete, actionable feedback</must>
  <avoid>Abstract theories without specific proposals</avoid>
  <avoid>Reviewing existing code quality issues</avoid>
  <avoid>Sequential agent execution (causes timeout)</avoid>
</constraints>
