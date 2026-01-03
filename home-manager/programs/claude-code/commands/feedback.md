---
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

<workflow>
<phase name="analyze">
<step>What was the previous command? (/define, /execute, /bug, /ask, other)</step>
<step>What files/work need to be reviewed?</step>
<step>Which agents should run in parallel?</step>
<step>What metrics are relevant for this mode?</step>
</phase>
<phase name="select">
<step>Determine mode based on previous command</step>
<step>After /define: Execution plan feedback</step>
<step>After /execute: Work content feedback</step>
<step>After /bug: Investigation quality feedback</step>
<step>After /ask: Answer accuracy feedback</step>
<step>Other: Recent work feedback</step>
</phase>
<phase name="execute">
<step>Launch all agents in parallel</step>
<step>Collect agent results</step>
</phase>
<phase name="synthesize">
<step>Compile feedback with metrics</step>
<step>Generate actionable recommendations</step>
</phase>
</workflow>

<modes>
<mode name="define">
<target>Execution plan from conversation history</target>
<aspects>Step granularity, dependencies, risk identification, completeness, feasibility</aspects>
<agents>
<agent name="plan" subagent_type="Plan" readonly="true">Execution plan review</agent>
<agent name="estimation" subagent_type="general-purpose" readonly="true">Estimation validity review</agent>
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
</agents>
<execution>All agents in parallel</execution>
</mode>
<mode name="general">
<target>Recent Claude Code work</target>
<agents>
<agent name="review" subagent_type="quality-assurance" readonly="true">Comprehensive work review</agent>
<agent name="complexity" subagent_type="code-quality" readonly="true">Code complexity review</agent>
<agent name="memory" subagent_type="general-purpose" readonly="true">Consistency check with existing patterns</agent>
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
<recommended_actions>
- [High] Action
- [Medium] Action
- [Low] Action</recommended_actions>
</feedback_results>
</format>
</output>

<constraints>
<must>Launch all agents simultaneously (no sequential execution)</must>
<must>Review only changed code in execute mode</must>
<must>Provide concrete, actionable feedback</must>
<avoid>Abstract theories without specific proposals</avoid>
<avoid>Reviewing existing code quality issues</avoid>
<avoid>Sequential agent execution (causes timeout)</avoid>
</constraints>
