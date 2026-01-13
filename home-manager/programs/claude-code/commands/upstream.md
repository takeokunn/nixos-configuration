---
argument-hint: [upstream-url]
description: Upstream PR preparation and review command
---

<purpose>
Review and prepare changes before submitting PRs to upstream OSS repositories, auto-fetching contribution guidelines, analyzing code changes, evaluating tests, and generating compliant PR metadata.
</purpose>

<rules priority="critical">
  <rule>Read-only operation: analyze and report only, no file modifications</rule>
  <rule>Auto-fetch CONTRIBUTING.md from upstream with fallback hierarchy (root, .github/, docs/)</rule>
  <rule>Launch all gather-phase agents in parallel</rule>
  <rule>Verify gh CLI authentication before PR history operations</rule>
</rules>

<rules priority="standard">
  <rule>Use gh CLI for all GitHub API operations</rule>
  <rule>Check Serena memories for existing contribution patterns</rule>
  <rule>Provide structured checklist output with actionable items</rule>
  <rule>Include local verification commands in output</rule>
  <rule>Always include a (Recommended) option when presenting choices via AskUserQuestion</rule>
</rules>

<parallelization>
  <capability>
    <parallel_safe>true</parallel_safe>
    <read_only>true</read_only>
    <modifies_state>none</modifies_state>
  </capability>
  <execution_strategy>
    <max_parallel_agents>16</max_parallel_agents>
    <timeout_per_agent>300000</timeout_per_agent>
    <retry_policy>
      <max_retries>2</max_retries>
      <retry_conditions>
        <condition>WebFetch timeout or failure</condition>
        <condition>gh CLI rate limit or auth error</condition>
      </retry_conditions>
    </retry_policy>
  </execution_strategy>
</parallelization>

<workflow>
  <phase name="preflight">
    <objective>Verify environment and detect upstream repository</objective>
    <step order="1">
      <action>Check Serena memories for contribution patterns of upstream repo</action>
      <tool>Serena list_memories, read_memory</tool>
      <output>Relevant patterns and past contribution experiences</output>
    </step>
    <step order="2">
      <action>Verify gh CLI authentication status</action>
      <tool>Bash: gh auth status</tool>
      <output>Authentication confirmation or error</output>
    </step>
    <step order="3">
      <action>Detect upstream repository from git remotes</action>
      <tool>Bash: git remote -v</tool>
      <output>Upstream URL (prefer upstream remote, fallback to origin)</output>
    </step>
    <step order="4">
      <action>If multiple remotes or detection confidence below 80, ask user</action>
      <tool>AskUserQuestion</tool>
      <output>Confirmed upstream URL</output>
    </step>
    <step order="5">
      <action>Get current branch and pending changes</action>
      <tool>Bash: git status, git diff</tool>
      <output>Branch name, change summary</output>
    </step>
    <step order="6">
      <action>Compare local branch with upstream default branch</action>
      <tool>Bash: git diff upstream/main...HEAD --stat</tool>
      <output>Summary of divergent changes</output>
    </step>
  </phase>
  <reflection_checkpoint id="preflight_complete" after="preflight">
    <questions>
      <question weight="0.4">Is gh CLI authenticated?</question>
      <question weight="0.3">Is upstream repository clearly identified?</question>
      <question weight="0.3">Are there changes to review?</question>
    </questions>
    <threshold min="70" action="stop">
      <below_threshold>Stop and report to user</below_threshold>
    </threshold>
  </reflection_checkpoint>
  <phase name="gather">
    <objective>Collect all necessary information in parallel</objective>
    <step order="1">
      <action>Fetch CONTRIBUTING.md from upstream</action>
      <tool>guidelines agent (WebFetch with fallback: root, .github/, docs/)</tool>
      <output>Contribution guidelines content</output>
    </step>
    <step order="2">
      <action>Analyze code changes against upstream patterns</action>
      <tool>changes agent (quality-assurance)</tool>
      <output>Code quality assessment</output>
    </step>
    <step order="3">
      <action>Evaluate test coverage and appropriateness</action>
      <tool>tests agent</tool>
      <output>Test evaluation report</output>
    </step>
    <step order="4">
      <action>Fetch author's past PRs to upstream</action>
      <tool>history agent (gh pr list --author @me --repo upstream --state all --limit 20)</tool>
      <output>Past PR feedback patterns</output>
    </step>
  </phase>
  <reflection_checkpoint id="gather_complete" after="gather">
    <questions>
      <question weight="0.4">Were contribution guidelines successfully fetched?</question>
      <question weight="0.3">Have code changes been analyzed?</question>
      <question weight="0.3">Has past PR history been retrieved?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Document gaps and proceed with available data</below_threshold>
    </threshold>
  </reflection_checkpoint>
  <phase name="synthesize">
    <objective>Generate PR metadata and verification steps</objective>
    <step order="1">
      <action>Generate PR title and description following contribution guide</action>
      <tool>docs agent</tool>
      <output>Compliant PR metadata draft</output>
    </step>
    <step order="2">
      <action>Determine local verification commands</action>
      <tool>Analyze package.json, Makefile, CI config</tool>
      <output>Test and build commands</output>
    </step>
    <step order="3">
      <action>Detect change types and generate manual QA checklist</action>
      <tool>Analyze diff for UI components, API endpoints, integration points using detection_rules</tool>
      <output>Change type detection (ui, api, integration) and applicable manual QA items</output>
    </step>
    <step order="4">
      <action>Compile checklist with all findings</action>
      <tool>Consolidate agent outputs</tool>
      <output>Structured checklist report</output>
    </step>
  </phase>
  <phase name="self_evaluate">
    <objective>Brief quality assessment of review output</objective>
    <step order="1">
      <action>Cross-validate guideline compliance with code review findings</action>
      <tool>validator agent</tool>
      <output>Validation report with consistency check</output>
    </step>
    <step order="2">
      <action>Calculate confidence using decision_criteria: guideline_compliance (40%), code_quality (30%), test_coverage (30%)</action>
      <tool>Decision criteria evaluation</tool>
      <output>Confidence score</output>
    </step>
    <step order="3">
      <action>Identify top 1-2 critical issues if confidence below 80 or review gaps detected</action>
      <tool>Gap analysis</tool>
      <output>Issue list</output>
    </step>
    <step order="4">
      <action>Append self_feedback section to output</action>
      <tool>Output formatting</tool>
      <output>Self-feedback section</output>
    </step>
  </phase>
  <phase name="failure_handling">
    <objective>Handle errors and missing data gracefully</objective>
    <step order="1">
      <action>If CONTRIBUTING.md not found: use generic OSS best practices</action>
    </step>
    <step order="2">
      <action>If gh CLI fails: skip PR history, note in report</action>
    </step>
    <step order="3">
      <action>If upstream detection ambiguous: ask user to specify</action>
    </step>
    <step order="4">
      <action>If WebFetch timeout or network unavailable: retry once, then note in report</action>
    </step>
    <step order="5">
      <action>If no changes detected (clean working tree): report to user, exit gracefully</action>
    </step>
    <step order="6">
      <action>If agent timeout: retry once with reduced scope, proceed with partial data</action>
    </step>
  </phase>
</workflow>

<decision_criteria>
  <criterion name="confidence_calculation">
    <factor name="guideline_compliance" weight="0.4">
      <score range="90-100">All contribution guidelines verified and followed</score>
      <score range="70-89">Core guidelines followed, minor gaps</score>
      <score range="50-69">Some guidelines unclear or not followed</score>
      <score range="0-49">Major guideline violations</score>
    </factor>
    <factor name="code_quality" weight="0.3">
      <score range="90-100">Code review passed, patterns consistent</score>
      <score range="70-89">Minor style issues only</score>
      <score range="50-69">Several quality issues found</score>
      <score range="0-49">Major quality issues</score>
    </factor>
    <factor name="test_coverage" weight="0.3">
      <score range="90-100">Tests appropriate and comprehensive</score>
      <score range="70-89">Tests present and adequate</score>
      <score range="50-69">Tests incomplete or unclear</score>
      <score range="0-49">Tests missing or failing</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="ready_for_pr">
      <input>guideline_compliance=95, code_quality=90, test_coverage=95</input>
      <calculation>(95*0.4)+(90*0.3)+(95*0.3) = 38+27+28.5 = 93.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>Full guideline compliance with quality code and comprehensive tests yields high confidence</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>guideline_compliance=80, code_quality=75, test_coverage=80</input>
      <calculation>(80*0.4)+(75*0.3)+(80*0.3) = 32+22.5+24 = 78.5</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Core guidelines followed but quality issues results in 78.5, triggers warning</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>guideline_compliance=85, code_quality=75, test_coverage=80</input>
      <calculation>(85*0.4)+(75*0.3)+(80*0.3) = 34+22.5+24 = 80.5</calculation>
      <expected_status>success</expected_status>
      <reasoning>Weighted average 80.5 meets success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>guideline_compliance=60, code_quality=55, test_coverage=60</input>
      <calculation>(60*0.4)+(55*0.3)+(60*0.3) = 24+16.5+18 = 58.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Weighted average 58.5 is below 60, triggers error</reasoning>
    </test>
    <test name="not_ready">
      <input>guideline_compliance=50, code_quality=45, test_coverage=50</input>
      <calculation>(50*0.4)+(45*0.3)+(50*0.3) = 20+13.5+15 = 48.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Multiple guideline violations with quality issues results in 48.5, triggers error</reasoning>
    </test>
  </validation_tests>
</decision_criteria>

<agents>
  <agent name="guidelines" subagent_type="docs" readonly="true">Parse CONTRIBUTING.md and extract requirements</agent>
  <agent name="changes" subagent_type="quality-assurance" readonly="true">Review code changes for quality and patterns</agent>
  <agent name="tests" subagent_type="test" readonly="true">Evaluate test coverage and appropriateness</agent>
  <agent name="history" subagent_type="general-purpose" readonly="true">Analyze author past PR feedback patterns via gh CLI</agent>
  <agent name="metadata" subagent_type="docs" readonly="true">Generate compliant PR title and description</agent>
  <agent name="verify" subagent_type="devops" readonly="true">Determine local verification commands and detect change types for manual QA checklist</agent>
  <agent name="validator" subagent_type="validator" readonly="true">Cross-validate guideline compliance and code review findings</agent>
</agents>

<execution_graph>
  <parallel_group id="gather" depends_on="none">
    <agent>guidelines</agent>
    <agent>changes</agent>
    <agent>tests</agent>
    <agent>history</agent>
  </parallel_group>
  <parallel_group id="post_gather" depends_on="gather">
    <agent>metadata</agent>
    <agent>verify</agent>
  </parallel_group>
  <sequential_phase id="validation" depends_on="post_gather">
    <agent>validator</agent>
    <reason>Cross-validate all findings before final output</reason>
  </sequential_phase>
</execution_graph>

<delegation>
  <requirement>Upstream repository URL or detection</requirement>
  <requirement>Current branch and pending changes</requirement>
  <requirement>Contribution guidelines (if available)</requirement>
  <requirement>Explicit no-modification prohibition</requirement>
  <requirement>Sub-agents must use AskUserQuestion for user interactions</requirement>
</delegation>

<output>
  <status_criteria>
    <status name="ready">Confidence score >= 80, no critical issues</status>
    <status name="needs_work">Confidence score 60-79, or has warning-level issues</status>
    <status name="blocked">Confidence score below 60, or has critical issues</status>
  </status_criteria>
  <format>
    <upstream_review>
      <summary>
        <upstream_repo>owner/repo</upstream_repo>
        <branch>feature-branch</branch>
        <changes_summary>Brief description of changes</changes_summary>
        <overall_score>XX/100</overall_score>
        <status>ready|needs_work|blocked</status>
      </summary>
      <checklist>
        <section name="Contribution Guidelines">
          <item status="pass|fail|warn">Guideline item description</item>
        </section>
        <section name="Code Quality">
          <item status="pass|fail|warn" priority="high|medium|low">Issue description with location</item>
        </section>
        <section name="Test Coverage">
          <item status="pass|fail|warn">Test evaluation item</item>
        </section>
        <section name="Past Review Patterns">
          <item status="info">Recurring feedback pattern to address</item>
        </section>
      </checklist>
      <pr_metadata>
        <title>Suggested PR title following contribution guide</title>
        <description>Suggested PR description with sections per upstream template</description>
      </pr_metadata>
      <local_verification>
        <command purpose="lint">npm run lint</command>
        <command purpose="test">npm test</command>
        <command purpose="build">npm run build</command>
      </local_verification>
      <manual_verification>
        <qa_category type="ui" condition="ui_changes_detected">
          <description>Visual and UI verification for frontend changes</description>
          <item>Verify visual layout matches design expectations</item>
          <item>Test responsive behavior at breakpoints (mobile/tablet/desktop)</item>
          <item>Check dark mode and theme compatibility</item>
          <item>Verify accessibility (keyboard navigation, screen reader)</item>
        </qa_category>
        <qa_category type="api" condition="api_changes_detected">
          <description>API endpoint verification for backend changes</description>
          <item>Verify endpoint responses with curl or Postman</item>
          <item>Test error responses (4xx, 5xx) with invalid inputs</item>
          <item>Verify authentication and authorization behavior</item>
          <item>Check response payload structure matches documentation</item>
        </qa_category>
        <qa_category type="integration" condition="integration_changes_detected">
          <description>Integration flow verification for cross-component changes</description>
          <item>Test complete user flow end-to-end</item>
          <item>Verify cross-component data flow correctness</item>
          <item>Test state persistence across page navigation</item>
          <item>Verify error handling propagation between components</item>
        </qa_category>
        <detection_rules>
          <rule type="ui">Changes to *.css, *.scss, *.html, *.jsx, *.tsx, *.vue, *.svelte files</rule>
          <rule type="api">Changes to **/api/**, **/routes/**, **/handlers/**, *.openapi.*, *.swagger.* files</rule>
          <rule type="integration">Changes spanning 3+ modules or external service configurations</rule>
        </detection_rules>
        <empty_state>If no change types detected, omit manual_verification section or display: No manual verification required for this change type</empty_state>
      </manual_verification>
      <recommended_actions>
        <action priority="high">Critical action before PR</action>
        <action priority="medium">Recommended improvement</action>
        <action priority="low">Optional enhancement</action>
      </recommended_actions>
      <self_feedback>
        <confidence>XX/100 (based on guideline_compliance, code_quality, test_coverage)</confidence>
        <issues>
          <issue severity="critical">Issue description (if any, max 2 total)</issue>
          <issue severity="warning">Issue description (if any)</issue>
        </issues>
      </self_feedback>
    </upstream_review>
  </format>
</output>

<enforcement>
  <mandatory_behaviors>
    <behavior id="UP-B001" priority="critical">
      <trigger>Before any GitHub operations</trigger>
      <action>Verify gh CLI authentication</action>
      <verification>Auth check in preflight phase</verification>
    </behavior>
    <behavior id="UP-B002" priority="critical">
      <trigger>When fetching CONTRIBUTING.md</trigger>
      <action>Check all three locations with fallback</action>
      <verification>Fallback hierarchy in gather phase</verification>
    </behavior>
    <behavior id="UP-B003" priority="critical">
      <trigger>When providing PR metadata</trigger>
      <action>Follow detected contribution guidelines format</action>
      <verification>Metadata matches upstream conventions</verification>
    </behavior>
    <behavior id="UP-B004" priority="standard">
      <trigger>When generating final output</trigger>
      <action>Include manual QA checklist based on detected change types</action>
      <verification>Manual verification section present when UI, API, or integration changes detected</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="UP-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Modifying any files</action>
      <response>Block operation, this is read-only command</response>
    </behavior>
    <behavior id="UP-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Creating PR without user confirmation</action>
      <response>Only provide review and suggestions, user creates PR</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<error_escalation>
  <level severity="low">
    <example>Minor style inconsistency with upstream</example>
    <action>Note in checklist as warning, proceed</action>
  </level>
  <level severity="medium">
    <example>CONTRIBUTING.md not found or gh CLI rate limited</example>
    <action>Document gap, use fallback, proceed with partial data</action>
  </level>
  <level severity="high">
    <example>Major guideline violation or breaking change detected</example>
    <action>STOP, present issues with recommended fixes</action>
  </level>
  <level severity="critical">
    <example>gh auth failure or no upstream detected</example>
    <action>BLOCK operation, require user to fix environment</action>
  </level>
</error_escalation>

<related_commands>
  <command name="execute">After upstream review, implement recommended fixes</command>
  <command name="feedback">Additional review after fixes applied</command>
  <command name="define">If requirements for contribution unclear</command>
</related_commands>

<related_skills>
  <skill name="execution-workflow">Understanding PR review methodology</skill>
  <skill name="testing-patterns">Evaluating test appropriateness</skill>
  <skill name="fact-check">Verifying contribution guideline compliance</skill>
</related_skills>

<constraints>
  <must>Verify gh CLI authentication before operations</must>
  <must>Check CONTRIBUTING.md in all three locations</must>
  <must>Provide structured checklist output</must>
  <must>Include local verification commands</must>
  <must>Include manual QA checklist when UI, API, or integration changes detected</must>
  <avoid>Modifying any files</avoid>
  <avoid>Creating PR automatically</avoid>
  <avoid>Proceeding without upstream confirmation when ambiguous</avoid>
</constraints>
