---
name: upstream
description: Upstream PR preparation and review command
---

<purpose>
Prepare a pull request to an upstream open-source repository. Reads CONTRIBUTING.md, samples existing PRs for style, validates the local changes against upstream conventions, and generates PR metadata (title, description, checklist). Read-only investigation; never creates or submits the PR.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">investigation-patterns</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<rules priority="critical">
  <rule>HARD BLOCK: Never run gh pr create, git push, or any command that creates or submits a PR — this command ends before that step</rule>
  <rule>HARD BLOCK: Never create PRs from non-feature branches (e.g., main → main); validate branch setup before proceeding</rule>
  <rule>Read-only: never modify local files or commit history</rule>
  <rule>Always fetch upstream CONTRIBUTING.md before generating PR metadata</rule>
  <rule>Always sample at least 10 merged PRs from the upstream repository for style reference</rule>
</rules>
<rules priority="standard">
  <rule>Use gh api to fetch upstream PR samples without requiring checkout</rule>
  <rule>Validate that local changes are rebased on the latest upstream default branch</rule>
  <rule>Generate PR title, description, and checklist in the upstream's preferred style</rule>
  <rule>Flag any gaps between local changes and upstream contribution requirements</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Manually reading upstream PRs one at a time — AI can fetch and analyze 10+ PRs simultaneously to infer style conventions</practice>
    <practice>Guessing PR title and description style — AI must verify conventions from actual upstream PRs, not from training data assumptions</practice>
    <practice>Assuming local changes are ready — AI must validate against upstream's CONTRIBUTING.md requirements and CI expectations</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Fetch and analyze upstream PRs in parallel; do not serialize sample collection</principle>
    <principle>Treat CONTRIBUTING.md as authoritative; training-data assumptions about the project's conventions are likely stale</principle>
    <principle>The output of this command is PR metadata only — the user decides when and whether to submit</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="validate">
    <step order="1">
      <action>Confirm the current branch is a feature branch (not main/master/develop)</action>
      <tool>Bash (git branch --show-current; git log --oneline upstream/main..HEAD)</tool>
      <output>Branch name and commit count ahead of upstream</output>
    </step>
    <step order="2">
      <action>Confirm local branch is rebased on latest upstream default branch</action>
      <tool>Bash (git fetch upstream; git log --oneline HEAD..upstream/main)</tool>
      <output>Rebase status: up-to-date or N commits behind</output>
    </step>
  </phase>
  <phase name="research">
    <step order="1">
      <action>Fetch upstream CONTRIBUTING.md (and CODE_OF_CONDUCT.md if present)</action>
      <tool>Bash (gh api or curl) or WebFetch</tool>
      <output>Contribution requirements and style guidelines</output>
    </step>
    <step order="2">
      <action>Fetch at least 10 recently merged PRs from upstream; analyze title format, description structure, checklist items</action>
      <tool>Bash (gh api repos/{owner}/{repo}/pulls?state=closed&per_page=10) — all in parallel</tool>
      <output>PR style conventions: title pattern, description template, label usage</output>
    </step>
    <step order="3">
      <action>Review local changes: summarize what changed, why, and what tests cover it</action>
      <tool>Bash (git diff upstream/main..HEAD); Read tool for changed files</tool>
      <output>Change summary: files modified, purpose, test coverage</output>
    </step>
  </phase>
  <phase name="generate">
    <step order="1">
      <action>Generate PR title following upstream convention inferred from sampled PRs</action>
      <tool>Content generation</tool>
      <output>PR title</output>
    </step>
    <step order="2">
      <action>Generate PR description following upstream template; include motivation, changes, and test plan</action>
      <tool>Content generation</tool>
      <output>PR description in upstream preferred format</output>
    </step>
    <step order="3">
      <action>Generate contribution checklist based on CONTRIBUTING.md requirements; pre-check items that are satisfied</action>
      <tool>Content generation</tool>
      <output>Checklist with satisfaction status per item</output>
    </step>
    <step order="4">
      <action>Flag any gaps: items in CONTRIBUTING.md not satisfied by local changes</action>
      <tool>Gap analysis</tool>
      <output>Gaps list with severity and remediation suggestion</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<output>
  <format>
    <validation_status>
      <branch>Current branch name and whether it is a valid feature branch</branch>
      <rebase_status>Up-to-date or N commits behind upstream</rebase_status>
    </validation_status>
    <pr_metadata>
      <title>Generated PR title in upstream style</title>
      <description>Full PR description body in upstream template format</description>
      <checklist>
        <item satisfied="true|false">Contribution requirement item</item>
      </checklist>
    </pr_metadata>
    <gaps>
      <gap severity="blocking|warning">Description and remediation</gap>
    </gaps>
    <next_steps>
      Manual steps the user must take to submit:
      1. Address any blocking gaps
      2. Run: gh pr create --title "..." --body "..." --repo upstream/repo
      (This command does NOT run the above — user must execute it manually)
    </next_steps>
  </format>
</output>
<constraints>
  <must>HARD BLOCK: Never run gh pr create, git push to upstream, or any PR submission command</must>
  <must>HARD BLOCK: Validate feature branch setup before proceeding</must>
  <must>Fetch upstream CONTRIBUTING.md before generating any PR metadata</must>
  <must>Sample at least 10 upstream PRs for style reference</must>
  <must>Keep all file operations read-only</must>
  <avoid>Creating, submitting, or pushing PRs</avoid>
  <avoid>Modifying local files, commits, or branch history</avoid>
  <avoid>Generating PR metadata without reading upstream conventions</avoid>
  <avoid>Assuming PR style from training data without upstream verification</avoid>
</constraints>
