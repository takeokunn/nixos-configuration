---
argument-hint: [upstream-url]
description: Upstream PR preparation and review command
---

<purpose>
Review and prepare changes before submitting PRs to upstream OSS repositories, auto-fetching contribution guidelines, analyzing code changes, evaluating tests, and generating compliant PR metadata.
</purpose>

<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
  <skill use="ecosystem">devenv-ecosystem</skill>
</refs>

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
  <rule>Include comprehensive local reproduction steps with Nix-first ecosystem detection</rule>
  <rule>Always include a (Recommended) option when presenting choices via AskUserQuestion</rule>
</rules>

<parallelization inherits="parallelization-patterns#parallelization_readonly" />

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
      <action>Fetch .github/PULL_REQUEST_TEMPLATE.md from upstream</action>
      <tool>pr_template agent (WebFetch: https://raw.githubusercontent.com/{owner}/{repo}/{default_branch}/.github/PULL_REQUEST_TEMPLATE.md)</tool>
      <output>PR template structure with required sections, or null if not found</output>
    </step>
    <step order="3">
      <action>Analyze code changes against upstream patterns</action>
      <tool>changes agent (quality-assurance)</tool>
      <output>Code quality assessment</output>
    </step>
    <step order="4">
      <action>Evaluate test coverage and appropriateness</action>
      <tool>tests agent</tool>
      <output>Test evaluation report</output>
    </step>
    <step order="5">
      <action>Sample 10 recently merged PRs from upstream for pattern learning</action>
      <tool>pr_samples agent (gh pr list --repo {owner}/{repo} --state merged --limit 10 --json title,body,number,author)</tool>
      <output>PR title patterns, description structure, common sections</output>
    </step>
  </phase>
  <reflection_checkpoint id="gather_complete" after="gather">
    <questions>
      <question weight="0.3">Were contribution guidelines successfully fetched?</question>
      <question weight="0.2">Was PR template fetched or confirmed absent?</question>
      <question weight="0.25">Have code changes been analyzed?</question>
      <question weight="0.25">Were PR samples retrieved for pattern learning?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Document gaps and proceed with available data</below_threshold>
    </threshold>
    <serena_validation>
      <tool>think_about_collected_information</tool>
      <trigger>After gather phase completes</trigger>
    </serena_validation>
  </reflection_checkpoint>
  <phase name="synthesize">
    <objective>Generate PR metadata, verification steps, and comprehensive task breakdown</objective>
    <step order="1">
      <action>Generate PR title and description: use PR template sections if available; otherwise derive structure from 10 sampled merged PR patterns</action>
      <tool>metadata agent with pr_template output (if found) and pr_samples patterns as fallback</tool>
      <output>Template-compliant PR metadata; template_source indicates whether upstream_template, sampled_patterns, or none was used</output>
    </step>
    <step order="2">
      <action>Detect project ecosystem and generate local reproduction steps</action>
      <tool>Analyze flake.nix, Makefile, Cargo.toml, go.mod, package.json with Nix-first priority</tool>
      <output>Ecosystem detection, environment setup, service dependencies, verification commands</output>
    </step>
    <step order="3">
      <action>Detect change types and generate context-injected manual QA checklist</action>
      <tool>Analyze diff for UI components, API endpoints, database migrations, config changes, security files, integration points using detection_rules</tool>
      <output>Change type detection (ui, api, database, config, security, integration) with actual paths, endpoints, and component names injected into qa_step commands</output>
    </step>
    <step order="4">
      <action>Compile checklist with all findings</action>
      <tool>Consolidate agent outputs</tool>
      <output>Structured checklist report</output>
    </step>
    <step order="5">
      <action>Generate comprehensive task breakdown for /execute handoff</action>
      <tool>Categorize all identified issues into phased_tasks phases (code_fixes, test_updates, documentation, commit_prep, final_verification)</tool>
      <output>Phased task list with dependencies using ID format: CF-XXX, TU-XXX, DOC-XXX, GIT-XXX, VER-XXX</output>
    </step>
    <step order="6">
      <action>Build dependency graph for parallel execution optimization</action>
      <tool>Analyze task dependencies to identify parallel-safe phases</tool>
      <output>Dependency graph with parallel groups</output>
    </step>
    <step order="7">
      <action>Compile execute_handoff section with decisions, references, and constraints</action>
      <tool>Consolidate contribution guidelines, code patterns, and past feedback into actionable references</tool>
      <output>Complete execute_handoff for /execute command consumption</output>
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
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
</workflow>

<decision_criteria inherits="core-patterns#decision_criteria">
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
</decision_criteria>

<agents>
  <agent name="guidelines" subagent_type="docs" readonly="true">Parse CONTRIBUTING.md and extract requirements</agent>
  <agent name="pr_template" subagent_type="docs" readonly="true">Fetch and parse .github/PULL_REQUEST_TEMPLATE.md from upstream; extract required sections and structure; return null if not found (no fallback)</agent>
  <agent name="changes" subagent_type="quality-assurance" readonly="true">Review code changes for quality and patterns</agent>
  <agent name="tests" subagent_type="test" readonly="true">Evaluate test coverage and appropriateness</agent>
  <agent name="pr_samples" subagent_type="general-purpose" readonly="true">Sample 10 recently merged PRs from upstream via gh CLI; extract title patterns, description structure, and common sections for pattern learning</agent>
  <agent name="metadata" subagent_type="docs" readonly="true">Generate compliant PR title and description: use PR template sections if available; otherwise derive structure from 10 sampled merged PR patterns; set template_source accordingly</agent>
  <agent name="verify" subagent_type="devops" readonly="true">Detect ecosystem (Nix-first), service dependencies, generate local reproduction steps, detect change types (ui, api, database, config, security, integration) using detection_rules, and inject actual paths/endpoints/component names into manual QA steps</agent>
  <agent name="validator" subagent_type="validator" readonly="true">Cross-validate guideline compliance and code review findings</agent>
</agents>

<execution_graph>
  <parallel_group id="gather" depends_on="none">
    <agent>guidelines</agent>
    <agent>pr_template</agent>
    <agent>changes</agent>
    <agent>tests</agent>
    <agent>pr_samples</agent>
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
  <requirement>PR template from .github/PULL_REQUEST_TEMPLATE.md (if available)</requirement>
  <requirement>10 sampled merged PRs for pattern learning</requirement>
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
        <template_source>upstream_template|sampled_patterns|none</template_source>
        <title>
          <value>Suggested PR title following learned patterns from sampled PRs</value>
          <pattern_notes>Pattern observed from 10 sampled PRs (e.g., [type]: description, feat(scope): description)</pattern_notes>
        </title>
        <description>
          <sections>
            <section name="Summary" required="true">Content based on template or sampled patterns</section>
            <section name="Test Plan" required="false">Content if template requires or patterns suggest</section>
          </sections>
          <raw_markdown>Full PR description in markdown matching upstream conventions</raw_markdown>
        </description>
        <pattern_confidence>0-100 based on template availability (40%) and sample quality (60%)</pattern_confidence>
      </pr_metadata>
      <local_reproduction>
        <ecosystem_detection>
          <description>Auto-detect project ecosystem from configuration files</description>
          <priority_order>
            <ecosystem priority="1" indicator="flake.nix">Nix (flake-based)</ecosystem>
            <ecosystem priority="2" indicator="shell.nix or default.nix">Nix (legacy)</ecosystem>
            <ecosystem priority="3" indicator="Makefile">Make</ecosystem>
            <ecosystem priority="4" indicator="Cargo.toml">Rust/Cargo</ecosystem>
            <ecosystem priority="5" indicator="go.mod">Go</ecosystem>
            <ecosystem priority="6" indicator="package.json">Node.js/npm</ecosystem>
            <ecosystem priority="7" indicator="pyproject.toml or setup.py">Python</ecosystem>
            <ecosystem priority="8" indicator="Gemfile">Ruby</ecosystem>
          </priority_order>
          <detected_ecosystem>Ecosystem name based on files found</detected_ecosystem>
        </ecosystem_detection>
        <environment_setup>
          <description>Prerequisites and environment initialization</description>
          <prerequisite_commands ecosystem="nix-flake">
            <command order="1" purpose="enter-shell">nix develop</command>
            <command order="2" purpose="build-check">nix flake check</command>
          </prerequisite_commands>
          <prerequisite_commands ecosystem="nix-legacy">
            <command order="1" purpose="enter-shell">nix-shell</command>
          </prerequisite_commands>
          <prerequisite_commands ecosystem="make">
            <command order="1" purpose="setup">make setup or make deps (if target exists)</command>
          </prerequisite_commands>
          <prerequisite_commands ecosystem="cargo">
            <command order="1" purpose="fetch">cargo fetch</command>
            <command order="2" purpose="build-check">cargo check</command>
          </prerequisite_commands>
          <prerequisite_commands ecosystem="go">
            <command order="1" purpose="fetch">go mod download</command>
            <command order="2" purpose="build-check">go build ./...</command>
          </prerequisite_commands>
          <prerequisite_commands ecosystem="npm">
            <command order="1" purpose="install">npm install or npm ci</command>
          </prerequisite_commands>
          <prerequisite_commands ecosystem="python">
            <command order="1" purpose="venv">python -m venv .venv and source .venv/bin/activate</command>
            <command order="2" purpose="install">pip install -e . or uv sync</command>
          </prerequisite_commands>
          <environment_requirements>
            <requirement type="env_var">List of required environment variables detected from .env.example or config</requirement>
            <requirement type="tool">List of required tools (detected from CI config or README)</requirement>
          </environment_requirements>
        </environment_setup>
        <service_dependencies>
          <description>External services required for local testing</description>
          <detection_sources>
            <source>docker-compose.yml or compose.yml</source>
            <source>.env.example (DATABASE_URL, REDIS_URL patterns)</source>
            <source>CI workflow files (services section)</source>
            <source>README.md (development setup section)</source>
          </detection_sources>
          <detected_services>
            <service name="service-name" start_command="docker compose up -d service-name">Service description</service>
          </detected_services>
          <startup_command>docker compose up -d (if docker-compose.yml exists)</startup_command>
        </service_dependencies>
        <verification_commands>
          <description>Ecosystem-specific verification commands</description>
          <commands ecosystem="nix-flake">
            <command purpose="check">nix flake check</command>
            <command purpose="build">nix build</command>
            <command purpose="test">nix flake check (if tests defined in flake outputs)</command>
          </commands>
          <commands ecosystem="cargo">
            <command purpose="lint">cargo clippy</command>
            <command purpose="test">cargo test</command>
            <command purpose="build">cargo build --release</command>
          </commands>
          <commands ecosystem="go">
            <command purpose="lint">go vet ./...</command>
            <command purpose="test">go test ./...</command>
            <command purpose="build">go build ./...</command>
          </commands>
          <commands ecosystem="npm">
            <command purpose="lint">npm run lint</command>
            <command purpose="test">npm test</command>
            <command purpose="build">npm run build</command>
          </commands>
          <commands ecosystem="make">
            <command purpose="lint">make lint (if target exists)</command>
            <command purpose="test">make test</command>
            <command purpose="build">make build or make all</command>
          </commands>
          <commands ecosystem="python">
            <command purpose="lint">ruff check . or flake8</command>
            <command purpose="test">pytest</command>
            <command purpose="build">python -m build or pip install -e .</command>
          </commands>
        </verification_commands>
        <reproduction_steps>
          <description>Step-by-step local reproduction procedure</description>
          <step order="1">
            <action>Checkout branch</action>
            <command>git checkout [branch-name]</command>
            <expected_output>Switched to branch [branch-name]</expected_output>
          </step>
          <step order="2">
            <action>Setup environment</action>
            <command>Ecosystem-specific setup command from environment_setup</command>
            <expected_output>Dependencies installed, environment ready</expected_output>
          </step>
          <step order="3">
            <action>Start services (if needed)</action>
            <command>Commands from service_dependencies</command>
            <expected_output>All required services running</expected_output>
          </step>
          <step order="4">
            <action>Run verification</action>
            <command>Commands from verification_commands</command>
            <expected_output>All checks pass (lint, test, build)</expected_output>
          </step>
          <step order="5">
            <action>Manual testing</action>
            <command>Start application locally (e.g., npm run dev, cargo run, go run .)</command>
            <expected_output>Application running at expected URL/port</expected_output>
          </step>
          <step order="6">
            <action>Verify change behavior</action>
            <steps>Context-specific steps based on change type (UI, API, integration)</steps>
            <expected_output>Change works as expected</expected_output>
          </step>
        </reproduction_steps>
        <reproduction_confidence>
          <score>0-100 based on detection completeness</score>
          <factors>
            <factor name="ecosystem_detected" weight="0.3">Was ecosystem clearly identified</factor>
            <factor name="services_documented" weight="0.3">Are service dependencies clear</factor>
            <factor name="commands_verified" weight="0.4">Do verification commands exist in project</factor>
          </factors>
          <fallback_guidance>If reproduction not feasible locally, suggest: CI-based testing, containerized environment, or ask maintainers</fallback_guidance>
        </reproduction_confidence>
      </local_reproduction>
      <manual_verification>
        <description>Create a reproducible verification environment using devenv</description>
        <directory_structure>
          <base_path>/tmp/[repo-name]/[branch-name-or-issue-number]/</base_path>
          <files>
            <file name="devenv.nix">Ecosystem-specific development environment</file>
            <file name=".envrc">direnv integration with use devenv</file>
            <file name="README.md">Detailed verification instructions</file>
            <file name="fixtures/">Test fixtures and sample data directory</file>
            <file name=".git/">Initialized git repository</file>
          </files>
        </directory_structure>
        <workflow>
          <step order="1" action="create_directory">
            <command>mkdir -p /tmp/[repo-name]/[branch-name-or-issue-number]</command>
            <expected_output>Directory created at /tmp/[repo-name]/[branch-name-or-issue-number]</expected_output>
          </step>
          <step order="2" action="generate_devenv">
            <description>Generate ecosystem-specific devenv.nix based on detected ecosystem</description>
            <tool>Write tool</tool>
            <output_file>devenv.nix</output_file>
          </step>
          <step order="3" action="generate_envrc">
            <description>Generate .envrc for direnv integration</description>
            <tool>Write tool</tool>
            <output_file>.envrc</output_file>
            <content>eval "$(devenv direnvrc)"
use devenv</content>
          </step>
          <step order="4" action="generate_fixtures">
            <description>Generate verification files based on change type</description>
            <tool>Write tool</tool>
            <output_directory>fixtures/</output_directory>
          </step>
          <step order="5" action="generate_readme">
            <description>Generate README.md with detailed verification steps</description>
            <tool>Write tool</tool>
            <output_file>README.md</output_file>
          </step>
          <step order="6" action="initialize_git">
            <command>cd /tmp/[repo-name]/[branch-name-or-issue-number] &amp;&amp; git init &amp;&amp; git add .</command>
            <expected_output>Initialized git repository with all files staged</expected_output>
          </step>
        </workflow>
        <devenv_templates>
          <description>Generate devenv.nix configuration for verification environments based on detected ecosystem</description>
          <skill_reference>
            <skill>devenv-ecosystem</skill>
            <usage>Consult devenv-ecosystem skill for language configuration patterns, version pinning, package manager selection, services (databases, caches), git-hooks, scripts, processes, profiles, env/dotenv configuration, and all devenv.nix options</usage>
          </skill_reference>
        </devenv_templates>
        <fixtures_generation>
          <fixture_type change_type="api">
            <directory>fixtures/api/</directory>
            <files>
              <file name="request.json">Sample API request body</file>
              <file name="response.json">Expected API response</file>
              <file name="test.sh">curl commands for API testing</file>
            </files>
          </fixture_type>
          <fixture_type change_type="database">
            <directory>fixtures/data/</directory>
            <files>
              <file name="seed.sql">Test data seed script</file>
              <file name="verify.sql">Verification queries</file>
            </files>
          </fixture_type>
          <fixture_type change_type="config">
            <directory>fixtures/config/</directory>
            <files>
              <file name=".env.test">Test environment variables</file>
              <file name="config.test.json">Test configuration</file>
            </files>
          </fixture_type>
          <fixture_type change_type="ui">
            <directory>fixtures/screenshots/</directory>
            <files>
              <file name="expected/">Expected screenshot directory</file>
              <file name="viewports.json">Viewport configurations for testing</file>
            </files>
          </fixture_type>
          <fixture_type change_type="security">
            <directory>fixtures/security/</directory>
            <files>
              <file name="test_tokens.json">Test authentication tokens (non-production)</file>
              <file name="permissions.json">Permission matrix for testing</file>
              <file name="auth_test.sh">Authentication flow test script</file>
            </files>
          </fixture_type>
          <fixture_type change_type="integration">
            <directory>fixtures/integration/</directory>
            <files>
              <file name="mock_services.json">Mock service configurations</file>
              <file name="event_payloads.json">Sample event and webhook payloads</file>
              <file name="sequence.md">Integration test sequence documentation</file>
            </files>
          </fixture_type>
        </fixtures_generation>
        <readme_template>
          <sections>
            <section name="Overview">
              <description>PR summary and change description</description>
              <template>
# Verification Environment for [pr-title]

**Branch:** [branch-name-or-issue-number]
**Upstream:** [upstream-url]
**Ecosystem:** [detected-ecosystem]

[pr-description]
              </template>
            </section>
            <section name="Prerequisites">
              <description>Required tools and environment setup</description>
              <template>
## Prerequisites

- nix (with flakes enabled)
- direnv (for automatic environment activation)
- devenv
              </template>
            </section>
            <section name="Setup">
              <description>Step-by-step environment initialization</description>
              <template>
## Setup

1. Navigate to this directory
2. Run `direnv allow` to activate the environment
3. Wait for devenv to initialize
              </template>
            </section>
            <section name="Verification Steps">
              <description>Detailed manual verification procedures</description>
              <template>
## Verification Steps

(Generated based on detected change types)
              </template>
            </section>
            <section name="Expected Results">
              <description>What success looks like</description>
              <template>
## Expected Results

(Generated based on change analysis)
              </template>
            </section>
            <section name="Troubleshooting">
              <description>Common issues and solutions</description>
              <template>
## Troubleshooting

### direnv not activating
Run `direnv allow` in the directory.

### devenv not found
Install devenv: `nix profile install github:cachix/devenv`
              </template>
            </section>
          </sections>
        </readme_template>
        <detection_rules>
          <rule type="ui">Changes to *.css, *.scss, *.sass, *.less, *.html, *.jsx, *.tsx, *.vue, *.svelte, **/components/**, **/pages/**, **/views/**</rule>
          <rule type="api">Changes to **/api/**, **/routes/**, **/handlers/**, **/controllers/**, **/endpoints/**, *.openapi.*, *.swagger.*, **/graphql/**</rule>
          <rule type="database">Changes to **/migrations/**, **/schema/**, **/models/**, *.sql, **/prisma/**, **/drizzle/**, **/typeorm/**, **/sequelize/**</rule>
          <rule type="config">Changes to *.env*, *.config.*, docker-compose.*, compose.*, *.yaml, *.yml, *.toml, *.json (config), **/config/**, Dockerfile*, .docker/**</rule>
          <rule type="security">Changes to **/auth/**, **/authentication/**, **/authorization/**, **/permission/**, **/security/**, **/middleware/auth*, **/guards/**, *.key, *.pem, *.cert, **/crypto/**</rule>
          <rule type="integration">Changes spanning 3+ modules, external service configurations, message queue handlers, event emitters/listeners, webhook handlers</rule>
        </detection_rules>
        <context_injection>
          <description>The synthesize phase MUST replace these placeholders with actual values from diff analysis</description>
          <placeholder name="[repo-name]">Repository name extracted from git remote (e.g., owner/repo becomes repo)</placeholder>
          <placeholder name="[branch-name-or-issue-number]">Current branch name or related issue number</placeholder>
          <placeholder name="[detected-ecosystem]">Ecosystem detected from project files</placeholder>
          <placeholder name="[upstream-url]">Upstream repository URL</placeholder>
          <placeholder name="[pr-title]">Generated PR title</placeholder>
          <placeholder name="[pr-description]">Generated PR description</placeholder>
        </context_injection>
        <verification_confidence>
          <score>0-100 based on environment reproducibility</score>
          <factors>
            <factor name="ecosystem_detected" weight="0.3">Was ecosystem clearly identified</factor>
            <factor name="devenv_completeness" weight="0.4">Does devenv.nix include all required tools</factor>
            <factor name="readme_clarity" weight="0.3">Are verification steps clear and executable</factor>
          </factors>
        </verification_confidence>
        <empty_state>If no verification environment needed, skip directory creation</empty_state>
      </manual_verification>
      <task_breakdown>
        <dependency_graph>
          <description>Visual representation of task dependencies for parallel/sequential execution</description>
          <example>
Phase 1: Code Fixes (independent)
Phase 2: Test Updates (independent)
Phase 3: Documentation (depends on Phase 1)
Phase 4: Final Verification (depends on all)
          </example>
        </dependency_graph>
        <phased_tasks>
          <dependency_format>
            <description>Valid dependency formats for task dependencies field</description>
            <format type="none">None</format>
            <format type="list">CF-001, CF-002</format>
            <format type="phase">All previous phases</format>
          </dependency_format>
          <phase name="code_fixes" order="1" parallel_safe="true">
            <description>Lint errors, style issues, code quality improvements identified in review</description>
            <task id="CF-001">
              <files>List of files to modify</files>
              <overview>Brief description of what needs to be done</overview>
              <dependencies>None</dependencies>
            </task>
          </phase>
          <phase name="test_updates" order="2" parallel_safe="true">
            <description>Missing tests, coverage gaps, test improvements</description>
            <task id="TU-001">
              <files>List of test files</files>
              <overview>Test task description</overview>
              <dependencies>CF-001</dependencies>
            </task>
          </phase>
          <phase name="documentation" order="3" parallel_safe="true">
            <description>README, inline docs, changelog, API documentation</description>
            <task id="DOC-001">
              <files>Documentation files</files>
              <overview>Documentation task description</overview>
              <dependencies>CF-001</dependencies>
            </task>
          </phase>
          <phase name="commit_prep" order="4" parallel_safe="false">
            <description>Commit message formatting per contribution guidelines, rebasing onto upstream, squashing commits if required</description>
            <task id="GIT-001">
              <files>N/A (git operations)</files>
              <overview>Git preparation task description</overview>
              <dependencies>All previous phases</dependencies>
            </task>
          </phase>
          <phase name="final_verification" order="5" parallel_safe="false">
            <description>Running lint, test, build commands before PR</description>
            <task id="VER-001">
              <files>N/A (verification commands)</files>
              <overview>Run all verification commands</overview>
              <dependencies>All previous phases</dependencies>
            </task>
          </phase>
        </phased_tasks>
        <execute_handoff>
          <description>This section is parsed by /execute command to initialize task context</description>
          <decisions>
            <decision id="D-001">Design decision description affecting implementation</decision>
          </decisions>
          <references>
            <reference type="upstream_guidelines">Link or content of CONTRIBUTING.md requirements</reference>
            <reference type="pr_template">Structure and required sections from .github/PULL_REQUEST_TEMPLATE.md</reference>
            <reference type="pr_patterns">Title and description patterns learned from 10 sampled merged PRs</reference>
            <reference type="code_patterns">Relevant upstream code patterns to follow (specific file paths)</reference>
            <reference type="past_feedback">Patterns from past PR reviews to address</reference>
          </references>
          <deliverables>
            <deliverable task="CF-001">Expected output: fixed files passing lint</deliverable>
            <deliverable task="TU-001">Expected output: new/updated tests passing</deliverable>
            <deliverable task="DOC-001">Expected output: updated documentation</deliverable>
            <deliverable task="GIT-001">Expected output: clean commit history ready for PR</deliverable>
            <deliverable task="VER-001">Expected output: all verification commands pass</deliverable>
          </deliverables>
          <memory_hints>
            <hint>Check serena memory for: contribution patterns of this upstream repo</hint>
            <hint>Check serena memory for: past PR feedback patterns</hint>
          </memory_hints>
          <verification_criteria>
            <criterion task="CF-001">Lint passes with zero errors</criterion>
            <criterion task="TU-001">Test suite passes, coverage not decreased</criterion>
            <criterion task="VER-001">All verification_commands exit 0</criterion>
          </verification_criteria>
          <constraints>
            <constraint>Must maintain read-only until /execute is invoked</constraint>
            <constraint>All tasks must be completable without PR creation</constraint>
            <constraint>Tasks should be atomic and independently executable</constraint>
          </constraints>
        </execute_handoff>
      </task_breakdown>
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
      <action>Use PR template structure (if available) and patterns learned from 10 sampled merged PRs</action>
      <verification>Metadata matches upstream PR template sections and learned patterns; template_source indicates data source used</verification>
    </behavior>
    <behavior id="UP-B008" priority="critical">
      <trigger>When fetching PR template</trigger>
      <action>Fetch .github/PULL_REQUEST_TEMPLATE.md only (no fallback hierarchy)</action>
      <verification>Template fetched from exact path or confirmed absent</verification>
    </behavior>
    <behavior id="UP-B009" priority="critical">
      <trigger>When sampling PRs for patterns</trigger>
      <action>Sample 10 most recently merged PRs from any author via gh pr list --state merged --limit 10</action>
      <verification>PR samples retrieved with title, body, number, author fields</verification>
    </behavior>
    <behavior id="UP-B004" priority="critical">
      <trigger>When generating final output</trigger>
      <action>Include manual QA checklist with structured qa_steps based on detected change types (ui, api, database, config, security, integration)</action>
      <verification>Manual verification section present with ordered qa_steps containing action, tool, command, expected_output for each detected change type</verification>
    </behavior>
    <behavior id="UP-B007" priority="critical">
      <trigger>When generating manual_verification section</trigger>
      <action>Inject actual values from diff analysis into qa_step commands replacing all placeholders</action>
      <verification>All placeholders ([endpoint-path], [component-name], [table-name], etc.) replaced with actual detected values; qa_confidence score reflects injection completeness</verification>
    </behavior>
    <behavior id="UP-B005" priority="critical">
      <trigger>When generating final output</trigger>
      <action>Generate comprehensive task_breakdown with phased_tasks and execute_handoff</action>
      <verification>task_breakdown section present with all task categories populated</verification>
    </behavior>
    <behavior id="UP-B006" priority="critical">
      <trigger>When generating final output</trigger>
      <action>Generate local_reproduction section with Nix-first ecosystem detection, environment setup, service dependencies, and step-by-step reproduction instructions</action>
      <verification>local_reproduction section present with ecosystem_detection, environment_setup, service_dependencies, verification_commands, and reproduction_steps</verification>
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
      <action>Creating PR via gh pr create or any other method</action>
      <response>HARD BLOCK: This command NEVER creates PRs. Output task breakdown for /execute handoff instead. User must create PR manually after completing all pre-PR tasks.</response>
    </behavior>
    <behavior id="UP-P003" priority="critical">
      <trigger>When user explicitly requests PR creation</trigger>
      <action>Creating PR even when user requests it</action>
      <response>HARD BLOCK: Refuse PR creation. Explain that /upstream is review-only and output the task breakdown. Instruct user to run /execute on tasks first, then create PR manually.</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor style inconsistency with upstream</example>
    <example severity="medium">CONTRIBUTING.md not found or gh CLI rate limited</example>
    <example severity="high">Major guideline violation or breaking change detected</example>
    <example severity="critical">gh auth failure or no upstream detected</example>
  </examples>
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
  <must>Fetch .github/PULL_REQUEST_TEMPLATE.md from upstream (no fallback) and sample 10 merged PRs for pattern learning</must>
  <must>Generate PR metadata with template_source (upstream_template, sampled_patterns, none) and pattern_confidence score</must>
  <must>Provide structured checklist output</must>
  <must>Include comprehensive local_reproduction section with Nix-first ecosystem detection</must>
  <must>Include manual QA checklist with structured qa_steps when ui, api, database, config, security, or integration changes detected</must>
  <must>Inject actual paths, endpoints, and component names into qa_step commands from diff analysis</must>
  <must>Include qa_confidence score with weighted factors in manual_verification section</must>
  <must>Generate comprehensive task_breakdown with phased_tasks for /execute handoff</must>
  <must>Include execute_handoff section with decisions, references, and constraints</must>
  <avoid>Modifying any files</avoid>
  <avoid>Creating PR via any method (HARD BLOCK)</avoid>
  <avoid>Creating PR even when user explicitly requests it (HARD BLOCK)</avoid>
  <avoid>Proceeding without upstream confirmation when ambiguous</avoid>
  <avoid>Using fallback hierarchy for PR template (only .github/ location)</avoid>
</constraints>
