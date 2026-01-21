---
name: Quality Tools
description: Tool definitions and usage patterns for code quality tools (ESLint, Prettier, tsc, linters). Agents reference this skill instead of inline tool definitions.
version: 1.0.0
---

<purpose>
  Provide standardized tool definitions and usage patterns for code quality verification. This skill centralizes tool knowledge that was previously duplicated across agents.
</purpose>

<tools>
  <tool name="ESLint">
    <description>JavaScript/TypeScript linter for code quality and style</description>
    <commands>
      <command name="check">eslint [files]</command>
      <command name="fix">eslint --fix [files]</command>
      <command name="format">eslint --format=json [files]</command>
    </commands>
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
    <description>TypeScript type checker</description>
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
    <description>Go language quality tools</description>
    <commands>
      <command name="fmt">go fmt ./...</command>
      <command name="vet">go vet ./...</command>
      <command name="lint">golangci-lint run</command>
    </commands>
    <use_case>Go code formatting and static analysis</use_case>
  </tool>

  <tool name="Rust_Tools">
    <description>Rust language quality tools</description>
    <commands>
      <command name="fmt">cargo fmt --check</command>
      <command name="clippy">cargo clippy</command>
      <command name="check">cargo check</command>
    </commands>
    <use_case>Rust code formatting and linting</use_case>
  </tool>

  <tool name="Nix_Tools">
    <description>Nix language quality tools</description>
    <commands>
      <command name="fmt">nixfmt [files]</command>
      <command name="check">nix flake check</command>
      <command name="lint">statix check</command>
    </commands>
    <use_case>Nix code formatting and validation</use_case>
  </tool>

  <tool name="Python_Tools">
    <description>Python language quality tools</description>
    <commands>
      <command name="fmt">black [files]</command>
      <command name="lint">ruff check [files]</command>
      <command name="type">mypy [files]</command>
    </commands>
    <use_case>Python code formatting, linting, and type checking</use_case>
  </tool>
</tools>

<patterns>
  <pattern name="quality_check_workflow">
    <description>Standard workflow for quality verification</description>
    <example>
      1. Run type checker (tsc, mypy, etc.)
      2. Run linter (eslint, clippy, etc.)
      3. Run formatter check (prettier, black, etc.)
      4. Report findings with locations
    </example>
  </pattern>

  <pattern name="auto_fix_workflow">
    <description>Workflow for automatic fixes</description>
    <example>
      1. Run formatter (prettier --write, cargo fmt)
      2. Run linter with fix (eslint --fix)
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

<decision_tree name="tool_selection">
  <question>What type of quality check is needed?</question>
  <branch condition="Type errors">Use language-specific type checker (tsc, mypy, cargo check)</branch>
  <branch condition="Code style and bugs">Use linter (eslint, clippy, ruff)</branch>
  <branch condition="Formatting only">Use formatter (prettier, black, cargo fmt)</branch>
  <branch condition="All of the above">Run in sequence: types → lint → format</branch>
</decision_tree>

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
