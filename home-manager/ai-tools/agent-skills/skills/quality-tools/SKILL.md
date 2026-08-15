---
name: Quality Tools
description: Tool definitions and usage patterns for code quality tools (ESLint, Prettier, tsc, linters) plus cohesion-raising refactor operations and scored-review skeletons. A tool-definition register, not a methodology skill.
version: 2.4.0
---

<purpose>
  Provide standardized tool definitions and usage patterns for code quality verification. This skill centralizes tool knowledge that was previously duplicated across agents.
</purpose>

<tools>
  <tool name="ESLint">
    <description>JavaScript/TypeScript linter (ESLint 10, flat config only)</description>
    <commands>
      <command name="check">eslint [files]</command>
      <command name="fix">eslint --fix [files]</command>
      <command name="format">eslint --format=json [files]</command>
    </commands>
    <config>
      Flat config only (eslint.config.js/ts). Use defineConfig() and globalIgnores().
      eslintrc format is fully removed. Config is resolved from each linted file's directory, not cwd.
    </config>
    <use_case>Check code style, find potential bugs, enforce coding standards</use_case>
    <output>
      Error/warning count, file locations, rule violations
    </output>
  </tool>

  <tool name="Prettier">
    <description>Code formatter for consistent style</description>
    <commands>
      <command name="check">prettier --check [files]</command>
      <command name="fix">prettier --write [files]</command>
    </commands>
    <use_case>Ensure consistent code formatting</use_case>
    <output>
      List of files that need formatting (check mode)
    </output>
  </tool>

  <tool name="TypeScript_Compiler">
    <description>TypeScript type checker (TS 6.0 stable, TS 7.0 native preview available)</description>
    <commands>
      <command name="check">tsc --noEmit</command>
      <command name="check_project">tsc -p tsconfig.json --noEmit</command>
    </commands>
    <use_case>Type checking, finding type errors</use_case>
    <output>
      Type errors with file:line locations
    </output>
  </tool>

  <tool name="Go_Tools">
    <description>Go language quality tools (Go 1.26)</description>
    <commands>
      <command name="fmt">gofmt -l [files]</command>
      <command name="vet">go vet ./...</command>
      <command name="staticcheck">staticcheck ./...</command>
    </commands>
    <use_case>Go code formatting and static analysis</use_case>
  </tool>

  <tool name="Rust_Tools">
    <description>Rust language quality tools (edition 2024)</description>
    <commands>
      <command name="fmt">cargo fmt --check</command>
      <command name="clippy">cargo clippy</command>
      <command name="check">cargo check</command>
    </commands>
    <use_case>Rust code formatting and linting</use_case>
  </tool>

  <tool name="Nix_Tools">
    <description>Nix language quality tools (nixfmt-rfc-style is the standard formatter)</description>
    <commands>
      <command name="fmt">nixfmt [files]</command>
      <command name="check">nix flake check</command>
      <command name="lint">statix check</command>
    </commands>
    <use_case>Nix code formatting and validation</use_case>
  </tool>

  <tool name="Biome">
    <description>Unified linter and formatter for JS/TS/JSON/CSS (alternative to ESLint+Prettier)</description>
    <commands>
      <command name="check">biome check [files]</command>
      <command name="fix">biome check --fix [files]</command>
      <command name="format">biome format [files]</command>
      <command name="lint">biome lint [files]</command>
    </commands>
    <config>biome.json configuration file. Supports linting + formatting in a single pass with 10-100x faster performance than ESLint+Prettier.</config>
    <use_case>All-in-one code quality for JS/TS projects; preferred when ESLint plugin ecosystem is not required</use_case>
    <output>
      Error/warning count, file locations, rule violations
    </output>
  </tool>

  <tool name="PHP_Tools">
    <description>PHP language quality tools (PHP 8.5+)</description>
    <commands>
      <command name="lint">phpstan analyse</command>
      <command name="fmt">php-cs-fixer fix [files]</command>
      <command name="test">pest --parallel</command>
    </commands>
    <use_case>PHP static analysis, formatting, and testing</use_case>
  </tool>

  <tool name="Haskell_Tools">
    <description>Haskell language quality tools (GHC 9.14)</description>
    <commands>
      <command name="fmt">fourmolu -i [files]</command>
      <command name="lint">hlint [files]</command>
      <command name="check">cabal build --ghc-options="-Wall -Werror"</command>
    </commands>
    <use_case>Haskell code formatting, linting, and type checking</use_case>
  </tool>

  <tool name="C_Cpp_Tools">
    <description>C/C++ quality tools (C23, C++26)</description>
    <commands>
      <command name="fmt">clang-format -i [files]</command>
      <command name="lint">clang-tidy [files]</command>
    </commands>
    <use_case>C/C++ formatting and static analysis</use_case>
  </tool>

  <tool name="Swift_Tools">
    <description>Swift language quality tools (Swift 6.3)</description>
    <commands>
      <command name="fmt">swift-format format -i [files]</command>
      <command name="lint">swiftlint [files]</command>
    </commands>
    <use_case>Swift code formatting and linting</use_case>
  </tool>

  <tool name="Python_Tools">
    <description>Python language quality tools (ruff is the dominant linter/formatter, replacing flake8, black, isort)</description>
    <commands>
      <command name="fmt">ruff format [files]</command>
      <command name="lint">ruff check [files]</command>
      <command name="fix">ruff check --fix [files]</command>
      <command name="type">mypy [files]</command>
    </commands>
    <use_case>Python code formatting, linting, and type checking</use_case>
  </tool>
</tools>

<concepts>
  <concept name="lint_categories">
    <description>Types of lint rules and their purposes</description>
    <example>
      Error prevention: Catch potential bugs
      Best practices: Enforce coding standards
      Style: Consistent formatting
      Security: Detect vulnerable patterns
    </example>
  </concept>

  <concept name="fix_safety">
    <description>Which fixes are safe to auto-apply</description>
    <example>
      Safe: Formatting, import sorting, simple style fixes
      Review needed: Complex refactors, logic changes
      Manual only: Security issues, breaking changes
    </example>
  </concept>

  <concept name="exit_codes">
    <description>Standard exit codes for quality tools</description>
    <example>
      0: Success, no issues
      1: Issues found
      2: Configuration or execution error
    </example>
  </concept>
</concepts>

<patterns>
  <pattern name="quality_check_workflow">
    <description>Standard workflow for quality verification</description>
    <example>
      1. Run type checker (tsc, mypy, etc.)
      2. Run linter (eslint, clippy, etc.)
      3. Run formatter check (prettier, ruff format, etc.)
      4. Report findings with locations
    </example>
  </pattern>

  <pattern name="auto_fix_workflow">
    <description>Workflow for automatic fixes</description>
    <example>
      1. Run formatter (prettier --write, ruff format, cargo fmt)
      2. Run linter with fix (eslint --fix, ruff check --fix)
      3. Verify with check commands
      4. Run tests to confirm no regressions
    </example>
  </pattern>

  <pattern name="incremental_check">
    <description>Check only changed files</description>
    <example>
      1. Get list of changed files (git diff --name-only)
      2. Filter by file type
      3. Run quality tools on filtered list
    </example>
  </pattern>
</patterns>

<refactoring_operations>
  <description>A catalog of language-neutral operations that raise cohesion and testability. Each is a bounded, behavior-preserving move; apply the smallest one that addresses the finding rather than a broad rewrite.</description>

  <operation name="view_data_extraction">
    <description>Move display-derivation logic (the branching that decides what to show) out of a view or component into a pure selector or "data" helper, leaving the view render-only.</description>
    <when>A component mixes decision logic with rendering, so it cannot be tested without rendering it.</when>
    <result>The pure helper is unit-testable in isolation; the view composes or renders frames without embedding decisions. A component is legitimately render-only only once such a pure selector exists.</result>
  </operation>

  <operation name="barrel_removal">
    <description>Replace a thin re-export module (a barrel or index) with direct imports from the concrete modules.</description>
    <when>A file exists only to re-export other modules and adds an indirection layer without any behavior.</when>
    <steps>
      <step>Point each consumer at the concrete module it actually needs.</step>
      <step>Once no consumer imports the barrel, delete it.</step>
    </steps>
    <caveats>
      <caveat>Confirm the file really is a barrel before deleting it. A package-root index that the package manifest names as the package's entry point is a config-bound public API, not a compatibility shim: retarget or remove the manifest export first, then delete the file.</caveat>
      <caveat>A file that defines anything of its own — contracts, ownership assignments, types — is not a barrel even when it looks like one. Split those definitions into dedicated modules and move consumers, rather than deleting the file outright.</caveat>
      <caveat>Keep an aggregation point that is the canonical public surface of a package until every caller has been migrated off it.</caveat>
    </caveats>
  </operation>

  <operation name="helper_split_wiring_from_implementation">
    <description>Separate wiring (state, transport, lifecycle) from implementation (the actual computation) by extracting the implementation into a focused helper module.</description>
    <when>A hook, handler, or service function grows because it both wires dependencies and performs complex logic.</when>
    <examples>
      <example>Extract complex callback logic from a hook into a helper, keeping only state wiring in the hook file.</example>
      <example>Keep transport-level handlers thin and move stream piping or payload-to-event mapping into a dedicated helper.</example>
      <example>Split a complex command into data-only spec helpers, a parse helper that returns explicit values, and a thin orchestrator that only validates and dispatches.</example>
    </examples>
    <verify>Run the targeted test file and the type or compile check for the touched package after each extraction; behavior must be unchanged.</verify>
  </operation>

  <operation name="static_data_logic_split">
    <description>Move inert values — constants, thresholds, marker strings, copy and template text, and narrow exported type contracts — out of a behavior module into a sibling data module, leaving the original module holding only executable behavior.</description>
    <when>A module mixes stable configuration values with the code that formats, parses, branches, or performs I/O.</when>
    <boundary_rule>If the value is a stable constant, a display limit or threshold, an immutable marker or copy string, or a lightweight exported contract (including a narrowing type alias), move it to the data module. If it formats, parses payloads, touches the filesystem or network, coordinates async work, or changes session state, it stays in the logic module.</boundary_rule>
    <result>The data module becomes the single place to change wording and limits, and the logic module becomes importable by tests without dragging static payloads along. This is a different axis from view_data_extraction, which moves display-decision branching: this one moves inert values and applies equally to server-side code that has no view at all.</result>
  </operation>

  <operation name="single_consumer_aggregator_inlining">
    <description>Inline a wrapper whose only job is to bundle already-local state and actions for exactly one caller.</description>
    <when>A composition module, factory, or wrapper forwards pieces the owning module already holds, and it has a single consumer.</when>
    <result>One indirection layer fewer with no behavior lost. Unlike barrel_removal the target composes rather than re-exports, so searching for pure re-export files will not find it.</result>
    <prohibition>Do not reintroduce a feature-root composition layer during a later split unless it carries logic beyond forwarding. This wrapper tends to come back, because adding one is the reflex when splitting a module — so the rule has to be stated as a standing prohibition, not only as a one-time cleanup.</prohibition>
  </operation>

  <operation name="over_abstraction_reversal">
    <description>Collapse a seam that does not pay for itself: remove a named helper and restore direct branching, or decline to unify two implementations that only look alike.</description>
    <when>A helper exists but relocates a step rather than owning a decision, or a proposed shared helper would span implementations whose semantics differ.</when>
    <tests>
      <test name="distinct_decision">Does the seam carry a decision of its own? A helper that only relocates a step costs a name, a call, and its own unit tests while making the control flow harder to follow at the call site.</test>
      <test name="semantics_not_shape">Do the candidates share semantics, or only structure? Two handlers with matching shape but differing validation rules, payload decoding, side effects, and limits are a recurring false positive for DRY; a common helper there reduces readability instead of raising it.</test>
    </tests>
    <result>Fewer helpers and fewer dedicated tests, with control flow visible where it happens. This direction of travel needs to be in the catalog: every other operation here extracts, so without a reversal an over-eager split has no documented remedy.</result>
  </operation>

  <stop_rules>
    <description>When to make no move at all. A catalog of extraction operations invites continuous splitting, so it needs criteria for declaring a module finished.</description>
    <rule>A module that is already thin orchestration over dedicated helpers is done. Extracting further adds indirection without reducing complexity.</rule>
    <rule>Target modules that hold concrete branching or mutable state. Do not sweep a subtree for candidates by structure alone.</rule>
    <rule>Split further only when a concrete bug or a genuinely new responsibility appears, not on the general principle that smaller is better.</rule>
    <rule>Apply coverage goals to the slices actually refactored rather than repo-wide as an undifferentiated target.</rule>
    <signals_of_overrun>A wrapper deleted, reintroduced, and deleted again; or a data module created to hold two constants. Both mean the catalog is being applied past the point where it pays.</signals_of_overrun>
  </stop_rules>

  <new_boundary_coverage>
    <rule>When an extraction creates a new boundary module, add a direct spec for that module. Relying on the public surface alone leaves the new seam exercised only incidentally, so coverage stays green while the boundary itself is untested.</rule>
  </new_boundary_coverage>
</refactoring_operations>

<scored_review>
  <description>Output skeleton for a multi-dimensional quality review (performance, documentation, or general design). It turns a diffuse "review" into a prioritized, actionable report.</description>
  <structure>
    <step order="1">Report each dimension separately as a one-line observable status — what was checked, what it returned, and what was not exercised — so weak areas stay visible rather than being averaged into one number.</step>
    <step order="2">Separate findings into Critical (must fix before release) and Quick Wins (high impact, low effort), each with an effort estimate.</step>
    <step order="3">Sequence remediation into phases (for example reliability first, then performance, then testing, then advanced), so the report reads as a rollout plan rather than a flat list.</step>
  </structure>
  <honesty>
    <rule>State the analysis's basis and its limits: an architectural or static review is not runtime measurement. Do not present estimated improvements or scores as measured results.</rule>
    <rule>Tag the review's basis with its evidence tier (verified, inferred, or assumed) and name what was not exercised (real workloads, low-resource systems, actual profiling).</rule>
    <rule>A score is valid only for the tree state it was computed against, so record what that state was. A stored scorecard outlives the code it scored and reads as current evidence to whoever finds it next.</rule>
    <rule>When an obsolete scorecard turns up, invalidate it explicitly rather than carrying it forward: state that it must not be used for current prioritization, and mark which of its items are resolved or superseded. Superseding a score requires re-measurement, not re-reading the old one — unbenchmarked estimates do not become evidence by ageing.</rule>
  </honesty>
  <label_integrity>
    <description>A check for the generated artifact itself, applicable to any report, dashboard, or evaluation table and not only to review output.</description>
    <rule>Every label must be derived from the same key it displays. A column headed with one qualifier while reading a differently-qualified source key produces a confident wrong number, which is worse than a missing one, because downstream decisions rely on it.</rule>
    <check>For each label in a report template, confirm the key it reads carries the same qualifier. This defect survives review because the code is locally correct and the label is locally reasonable — only the pairing is wrong.</check>
  </label_integrity>
</scored_review>

<policy_gates>
  <description>Project-local checks that enforce a rule the off-the-shelf tools above do not know about — typically finishing a mechanical migration or holding a layering boundary. Unlike the tool catalog, you author and maintain these, so their failure modes are yours too.</description>

  <pattern name="migration_enforcement_check">
    <description>A large mechanical migration does not finish when the last file is edited. It finishes when the old idiom becomes impossible to reintroduce.</description>
    <rule>Ship the check as a test inside the normal suite, not as a separate tool that has to be remembered.</rule>
    <rule>Scan emitted output — format strings and generated text — rather than whole-file text. A whole-file scan fails on comments and documentation, which is how a change that keeps runtime logic intact and merely renames surrounding prose ends up red.</rule>
    <rule>Make it table-driven, one call site per migrated file, so coverage gaps are visible by inspection instead of inferred.</rule>
    <rule>Ship a narrower variant for legitimate exceptions, so the escape hatch is explicit rather than achieved by weakening the rule for everyone.</rule>
  </pattern>

  <pattern name="regex_checker_false_positives">
    <description>Home-grown layering and purity checkers are almost always regex or grep over source text, so they match identifiers, comments, and string literals indistinguishably from real API references.</description>
    <rule>Treat a hit as evidence to investigate, not proof of a violation.</rule>
    <rule>Inside directories governed by such a check, avoid naming local identifiers after the forbidden APIs. A slightly different local name costs far less than a permanently noisy gate that reviewers learn to ignore.</rule>
    <note>A convention is not adopted until a machine gate enforces it (see workflow-patterns), and a gate is only worth having while its precision keeps it trusted. Those two pull in opposite directions when the check is textual.</note>
  </pattern>
</policy_gates>

<supply_chain>
  <description>Rules for external references a build resolves by name: CI action references, CDN asset URLs, container images, and any other dependency fetched at build or run time.</description>
  <rule priority="critical">Pin every external reference to an immutable identifier — a full commit hash or an exact version — and keep the human-readable release or tag in an adjacent comment. A floating reference (a moving tag, an `@latest` alias, an unversioned path) can change what the build produces without any repository diff, which is what makes it worth a standing rule rather than case-by-case judgement.</rule>
  <rule priority="high">Where the reference is data, assert the pinned form in a test — for example a test that matches asset URLs against an exact-version pattern. That turns an upgrade into a deliberate, reviewable edit instead of silent drift.</rule>
  <rule priority="medium">Keep automation credentials at the narrowest scope by default and widen only where a specific job demonstrates the need.</rule>
  <tooling>
    <category name="secret_scanning">Scan the working tree, not only history, so an unstaged credential is caught before it is committed (for example gitleaks).</category>
    <category name="workflow_linting">Validate CI workflow syntax and expressions statically (for example actionlint).</category>
    <category name="workflow_static_analysis">Audit workflows for supply-chain weaknesses specifically — unpinned references, over-broad permissions, untrusted input reaching a shell (for example zizmor).</category>
  </tooling>
</supply_chain>

<coverage_gates>
  <description>What a coverage percentage does and does not certify, for any tool that emits a per-file report and an aggregate total.</description>
  <rule priority="high">An aggregate percentage is computed over the files that appear in the report, so it says nothing about a file that was dropped from it. A production file that failed to instrument, or was never loaded by the run, contributes nothing to the denominator — and a hundred percent over nine of ten files reads exactly like a hundred percent over ten.</rule>
  <rule priority="high">Gate on a declared source manifest, not on the total alone. Compare the report's normalized source filenames against the list of files that are supposed to be covered, and reject the run when a declared file is missing from the report, when a row is malformed, or when a row's total is zero. A zero-total row means the file was seen but never executed, which is the same news as its absence and just as easy to overlook.</rule>
  <note>This is the coverage form of the empty-selector problem: the check passes because it found nothing to disagree with. See testing-patterns for the suite-level version.</note>
</coverage_gates>

<running_checks>
  <description>Mechanics that decide whether a check's output is usable at all, independent of which tool produced it.</description>
  <rule priority="high">Before inspecting diff output mechanically, neutralize any configured external diff driver. A repo-level or user-level `diff.external` / `diff.tool` setting (difftastic, delta, and similar) makes `git diff`, `git show`, and `git log -p` emit syntax-highlighted, structurally reformatted text rather than a parseable unified diff — so pass `--no-ext-diff`. The failure is silent: the command exits zero and the reader draws wrong conclusions from decorated output instead of hitting an error.</rule>
  <rule priority="medium">Check this before concluding a diff is empty or a change is missing. A configuration set globally for the machine applies to every repository, not just the one that documents it.</rule>
  <note>See git-ecosystem for the broader Git configuration and workflow surface. The rule appears here because it governs whether a verification step's output can be trusted.</note>
</running_checks>

<decision_tree name="tool_selection">
  <question>What type of quality check is needed?</question>
  <branch condition="Type errors">Use language-specific type checker (tsc, mypy, cargo check)</branch>
  <branch condition="Code style and bugs">Use linter (eslint, clippy, ruff, biome lint)</branch>
  <branch condition="Formatting only">Use formatter (prettier, ruff format, cargo fmt, biome format)</branch>
  <branch condition="Lint + format in one pass (JS/TS)">Use biome check (single tool for both)</branch>
  <branch condition="All of the above">Run in sequence: types → lint → format</branch>
</decision_tree>

<best_practices>
  <practice priority="critical">Run type checker before linter for faster feedback</practice>
  <practice priority="critical">Verify with tests after auto-fixes</practice>
  <practice priority="high">Use project-specific configuration when available</practice>
  <practice priority="high">Report all issues with file:line locations</practice>
  <practice priority="medium">Run incremental checks for large codebases</practice>
  <practice priority="medium">Separate formatting from logic changes in commits</practice>
  <practice priority="medium">Prefer the smallest cohesion-raising refactor operation (view-data extraction, barrel removal, helper split) over a broad rewrite, and verify behavior with targeted tests after each move (refactoring_operations)</practice>
  <practice priority="medium">For scored reviews, separate Critical from Quick Wins, phase the rollout, and state the analysis basis and its limits honestly (scored_review)</practice>
  <practice priority="high">Pin every external build reference to an immutable identifier, and assert the pin in a test where the reference is data (supply_chain)</practice>
  <practice priority="high">Pass --no-ext-diff whenever diff output will be read mechanically; a configured external diff driver fails silently rather than erroring (running_checks)</practice>
  <practice priority="medium">Before extracting further, apply the stop rules: a module that is already thin orchestration is done, and a seam that carries no decision should be collapsed rather than kept (refactoring_operations)</practice>
  <practice priority="medium">Finish a mechanical migration with a table-driven policy check scoped to emitted output, with an explicit narrower variant for exceptions (policy_gates)</practice>
  <practice priority="medium">Confirm every report label is derived from the key it displays, and invalidate obsolete scorecards instead of carrying them forward (scored_review)</practice>
</best_practices>

<anti_patterns>
  <avoid name="blind_auto_fix">
    <description>Auto-fixing without reviewing changes</description>
    <instead>Review auto-fix changes, run tests after</instead>
  </avoid>

  <avoid name="ignoring_warnings">
    <description>Only addressing errors, ignoring warnings</description>
    <instead>Address warnings that indicate potential issues</instead>
  </avoid>

  <avoid name="skipping_type_check">
    <description>Running linter without type checking first</description>
    <instead>Run type checker first for faster feedback loop</instead>
  </avoid>
</anti_patterns>

<constraints>
  <must>Run quality checks before marking implementation complete</must>
  <must>Report findings with file:line locations</must>
  <must>Use project configuration when available</must>
  <avoid>Auto-fixing without test verification</avoid>
  <avoid>Ignoring type errors</avoid>
  <avoid>Running only subset of quality tools</avoid>
</constraints>

<error_escalation>
  <examples>
    <example severity="low">Minor linting warning with no functional impact</example>
    <example severity="medium">Type error or test failure blocking CI</example>
    <example severity="high">Code quality gate failure preventing deployment</example>
    <example severity="critical">Security vulnerability detected by static analysis</example>
  </examples>
</error_escalation>

<related_skills>
  <skill name="testing-patterns">Run tests after quality fixes</skill>
  <skill name="execution-workflow">Integration with implementation workflow</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate code patterns and references in this skill domain</agent>
  <agent name="quality-assurance">Review implementation quality against this skill guidance</agent>
  <agent name="code-quality">Analyze code complexity and suggest refactoring improvements</agent>
</related_agents>
