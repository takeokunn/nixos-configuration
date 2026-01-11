---
argument-hint: [previous-command]
description: Review command for Claude Code's recent work
---

<purpose>
Multi-faceted review of Claude Code's work within the same session, automatically selecting appropriate review mode and executing efficiently in parallel.
</purpose>

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

<parallelization>
  <capability>
    <parallel_safe>true</parallel_safe>
    <read_only>true</read_only>
    <modifies_state>none</modifies_state>
  </capability>
  <execution_strategy>
    <max_parallel_agents>16</max_parallel_agents>
    <timeout_per_agent>180000</timeout_per_agent>
  </execution_strategy>
</parallelization>

<workflow>
  <phase name="analyze">
    <objective>Determine review scope and appropriate mode selection</objective>
    <step>1. What was the previous command? (/define, /execute, /bug, /ask, other)</step>
    <step>2. What files/work need to be reviewed?</step>
    <step>3. Which agents should run in parallel?</step>
    <step>4. What metrics are relevant for this mode?</step>
  </phase>
  <phase name="select">
    <objective>Select review mode and configure appropriate agents</objective>
    <step>1. Determine mode based on previous command</step>
    <step>2. After /define: Execution plan feedback</step>
    <step>3. After /execute: Work content feedback</step>
    <step>4. After /bug: Investigation quality feedback</step>
    <step>5. After /ask: Answer accuracy feedback</step>
    <step>6. Other: Recent work feedback</step>
  </phase>
  <reflection_checkpoint id="analysis_quality">
    <question>Have I gathered sufficient evidence to proceed?</question>
    <question>Are there gaps in my understanding?</question>
    <threshold>If confidence less than 70, seek more evidence or ask user</threshold>
  </reflection_checkpoint>
  <phase name="execute">
    <objective>Execute parallel review analysis across selected agents</objective>
    <step>1. Launch all agents in parallel</step>
    <step>2. Collect agent results</step>
  </phase>
  <reflection_checkpoint id="review_quality">
    <question>Did all agents complete successfully?</question>
    <question>Is the feedback specific and actionable?</question>
    <question>Have I assigned priority levels to all issues?</question>
    <threshold>If confidence less than 70, gather additional context or re-run agents</threshold>
  </reflection_checkpoint>
  <phase name="failure_handling">
    <objective>Handle execution failures and incomplete data gracefully</objective>
    <step>1. If tool call fails: Log error, attempt alternative approach</step>
    <step>2. If data unavailable: Document gap, proceed with partial analysis</step>
    <step>3. If contradictory evidence: Flag uncertainty, request user clarification</step>
  </phase>
  <phase name="synthesize">
    <objective>Compile comprehensive feedback report with actionable recommendations</objective>
    <step>1. Compile feedback with metrics</step>
    <step>2. Generate actionable recommendations</step>
  </phase>
</workflow>

<decision_criteria>
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
    <test name="comprehensive_review">
      <input>review_depth=95, feedback_actionability=90, issue_prioritization=95</input>
      <calculation>(95*0.4)+(90*0.3)+(95*0.3) = 38+27+28.5 = 93.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>Deep code analysis with actionable, prioritized feedback yields high confidence</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>review_depth=80, feedback_actionability=75, issue_prioritization=80</input>
      <calculation>(80*0.4)+(75*0.3)+(80*0.3) = 32+22.5+24 = 78.5</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Major issues found with partial prioritization results in 78.5, triggers warning</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>review_depth=85, feedback_actionability=75, issue_prioritization=80</input>
      <calculation>(85*0.4)+(75*0.3)+(80*0.3) = 34+22.5+24 = 80.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>Weighted average 80.5 meets success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>review_depth=60, feedback_actionability=55, issue_prioritization=60</input>
      <calculation>(60*0.4)+(55*0.3)+(60*0.3) = 24+16.5+18 = 58.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Weighted average 58.5 is below 60, triggers error</reasoning>
    </test>
    <test name="superficial_review">
      <input>review_depth=50, feedback_actionability=45, issue_prioritization=50</input>
      <calculation>(50*0.4)+(45*0.3)+(50\*0.3) = 20+13.5+15 = 48.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Surface level review with vague feedback results in 48.5, triggers error</reasoning>
    </test>
  </validation_tests>
</decision_criteria>

<modes>
  <mode name="define">
    <target>Execution plan from conversation history</target>
    <aspects>Step granularity, dependencies, risk identification, completeness, feasibility</aspects>
    <agents>
      <agent name="plan" subagent_type="Plan" readonly="true">Execution plan review</agent>
      <agent name="estimation" subagent_type="general-purpose" readonly="true">Estimation validity review</agent>
      <agent name="fact-check" subagent_type="fact-check" readonly="true">External source verification</agent>
    </agents>
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
      <agent name="fact-check" subagent_type="fact-check" readonly="true">External source verification</agent>
    </agents>
    <execution>All agents in parallel</execution>
  </mode>
  <mode name="general">
    <target>Recent Claude Code work</target>
    <agents>
      <agent name="review" subagent_type="quality-assurance" readonly="true">Comprehensive work review</agent>
      <agent name="complexity" subagent_type="code-quality" readonly="true">Code complexity review</agent>
      <agent name="memory" subagent_type="general-purpose" readonly="true">Consistency check with existing patterns</agent>
      <agent name="fact-check" subagent_type="fact-check" readonly="true">External source verification</agent>
    </agents>
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
      <agent name="fact-check" subagent_type="fact-check" readonly="true">External source verification</agent>
    </agents>
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
      <agent name="fact-check" subagent_type="fact-check" readonly="true">External source verification</agent>
    </agents>
    <execution>All agents in parallel</execution>
  </mode>
</modes>

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

<error_escalation>
  <level severity="low">
    <example>Minor code quality issue in reviewed work</example>
    <action>Note in report, proceed</action>
  </level>
  <level severity="medium">
    <example>Unclear quality metric or missing test coverage</example>
    <action>Document issue, use AskUserQuestion for clarification</action>
  </level>
  <level severity="high">
    <example>Critical security flaw or major design issue in reviewed work</example>
    <action>STOP, present options to user</action>
  </level>
  <level severity="critical">
    <example>Data loss risk or security breach in reviewed work</example>
    <action>BLOCK operation, require explicit user acknowledgment</action>
  </level>
</error_escalation>

<related_commands>
  <command name="execute">Primary target for feedback after implementation</command>
  <command name="define">Feedback on execution plans</command>
  <command name="bug">Feedback on investigation quality</command>
  <command name="ask">Feedback on answer accuracy</command>
  <command name="upstream">Review before submitting upstream PR</command>
</related_commands>

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
