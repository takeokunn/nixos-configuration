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
<step>What was the previous command? (/define, /execute, other)</step>
<step>What files/work need to be reviewed?</step>
<step>Which agents should run in parallel?</step>
<step>What metrics are relevant for this mode?</step>
</phase>
<phase name="mode_selection">
<mode condition="After /define">Execution plan feedback</mode>
<mode condition="After /execute">Work content feedback</mode>
<mode condition="Other">Recent work feedback</mode>
</phase>
<phase name="execute">Launch all agents in parallel</phase>
<phase name="synthesize">Compile feedback with metrics</phase>
</workflow>

<modes>
<mode name="define">
<target>Execution plan from conversation history</target>
<aspects>Step granularity, dependencies, risk identification, completeness, feasibility</aspects>
<agents>
<agent name="plan">Execution plan review</agent>
<agent name="estimation">Estimation validity review</agent>
</agents>
</mode>
<mode name="execute">
<target>Files modified via Edit/Write tools</target>
<agents>
<agent name="quality">Naming, DRY, readability</agent>
<agent name="security">OWASP Top 10, input validation, auth</agent>
<agent name="design">Architecture consistency, patterns</agent>
<agent name="docs">Accuracy, structure, completeness</agent>
<agent name="performance">Performance review</agent>
<agent name="test">Test coverage review</agent>
</agents>
<execution>All agents in parallel</execution>
</mode>
<mode name="general">
<target>Recent Claude Code work</target>
<agents>
<agent name="review">Comprehensive work review</agent>
<agent name="complexity">Code complexity review</agent>
<agent name="memory">Consistency check with existing patterns</agent>
</agents>
</mode>
</modes>

<output>
<format>
## {Mode} Feedback Results

### Evaluation Scores
- {Metric1}: XX/100
- {Metric2}: XX/100
- Overall: XX/100

### Critical
Immediate Fix Required
- [Category] Issue: Location
- Problem: Description
- Fix: Proposal

### Warning
Fix Recommended
- [Category] Issue: Location
- Problem: Description
- Recommendation: Proposal

### Good Practice
[Category] Commendable aspects

### Recommended Actions
- [High] Action
- [Medium] Action
- [Low] Action
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
