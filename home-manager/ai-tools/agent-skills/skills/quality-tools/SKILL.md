---
name: Quality Tools
description: Tool definitions and usage patterns for code quality tools (ESLint, Prettier, tsc, linters). Agents reference this skill instead of inline tool definitions.
version: 2.0.0
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

<related_skills>
  <skill name="testing-patterns">Run tests after quality fixes</skill>
  <skill name="execution-workflow">Integration with implementation workflow</skill>
</related_skills>
