---
description: Review command for Claude Code's recent work
modes:
  - name: define
    description: Comprehensive review of /define execution plan
    agents:
      - name: plan
        description: Execution plan review
      - name: estimation
        description: Estimation validity review
  - name: execute
    description: Comprehensive review of /execute work
    agents:
      - name: quality
        description: Code quality review
      - name: security
        description: Security review
      - name: design
        description: Architecture consistency review
      - name: docs
        description: Documentation quality review
      - name: performance
        description: Performance review
      - name: test
        description: Test coverage review
      - name: accessibility
        description: Accessibility review
      - name: error-handling
        description: Error handling review
  - name: general
    description: Review of Claude Code's recent work
    agents:
      - name: review
        description: Comprehensive work review
      - name: complexity
        description: Code complexity review
      - name: memory
        description: Consistency check with existing patterns
skills:
  - name: execution-workflow
    description: Code review methodology
  - name: serena-usage
    description: Serena MCP tool patterns
  - name: context7-usage
    description: Context7 documentation retrieval
---

<purpose>
Multi-faceted review of Claude Code's work within the same session, automatically selecting appropriate review mode and executing efficiently in parallel.
</purpose>

<instructions priority="critical">
<instruction>Launch all Task tools simultaneously in one message (timeout avoidance)</instruction>
<instruction>Auto-select mode based on previous command</instruction>
<instruction>Review only changed code in execute mode, not existing issues</instruction>
<instruction>Provide concrete fix proposals, not abstract theories</instruction>
</instructions>

<instructions priority="standard">
<instruction>Use execution-workflow skill for code review methodology</instruction>
<instruction>Check Serena memories for existing patterns</instruction>
<instruction>Target session operations, not git diff</instruction>
</instructions>

<thinking_process>
<step>What was the previous command? (/define, /execute, other)</step>
<step>What files/work need to be reviewed?</step>
<step>Which agents should run in parallel?</step>
<step>What metrics are relevant for this mode?</step>
</thinking_process>

<mode_selection>
<mode condition="After /define" name="define" description="Execution plan feedback" />
<mode condition="After /execute" name="execute" description="Work content feedback" />
<mode condition="Other" name="general" description="Recent work feedback" />
</mode_selection>

<modes>
<mode name="define">
<target>Execution plan from conversation history</target>
<aspects>
<aspect>Step granularity</aspect>
<aspect>Dependencies and sequencing</aspect>
<aspect>Risk identification</aspect>
<aspect>Completeness</aspect>
<aspect>Feasibility</aspect>
</aspects>
</mode>

<mode name="execute">
<target>Files modified via Edit/Write tools</target>
<agents>
<agent name="quality" aspects="Naming, DRY, readability" />
<agent name="security" aspects="OWASP Top 10, input validation, auth" />
<agent name="design" aspects="Architecture consistency, patterns" />
<agent name="docs" aspects="Accuracy, structure, completeness" />
</agents>
<execution>4 agents in parallel</execution>
</mode>

<mode name="general">
<target>Recent Claude Code work</target>
<review_by_type>
<type name="research" aspects="Comprehensiveness, accuracy" />
<type name="documentation" aspects="Accuracy, readability" />
<type name="code" aspects="Quality, security, consistency" />
</review_by_type>
</mode>
</modes>

<output_format>
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
</output_format>

<constraints>
<must>Launch all agents simultaneously (no sequential execution)</must>
<must>Review only changed code in execute mode</must>
<must>Provide concrete, actionable feedback</must>
<avoid>Abstract theories without specific proposals</avoid>
<avoid>Reviewing existing code quality issues</avoid>
<avoid>Sequential agent execution (causes timeout)</avoid>
</constraints>
