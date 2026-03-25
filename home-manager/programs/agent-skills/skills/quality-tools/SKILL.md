---
name: quality-tools
description: "Use when running 'eslint', 'prettier', 'tsc', 'cargo clippy', 'go vet', 'nixfmt', 'ruff', 'black', 'mypy', or any code quality/linting workflow. Provides standardized tool definitions and usage patterns for multi-language code quality verification."
---

Centralized tool definitions and usage patterns for code quality verification across JavaScript/TypeScript, Go, Rust, Nix, and Python ecosystems. The agent should reference this skill instead of inline tool definitions.

## Tool Reference

### JavaScript/TypeScript

| Tool | Check Command | Fix Command | Purpose |
|------|--------------|-------------|---------|
| ESLint | `eslint [files]` | `eslint --fix [files]` | Linting, code quality, standards |
| Prettier | `prettier --check [files]` | `prettier --write [files]` | Consistent formatting |
| TypeScript | `tsc --noEmit` | N/A | Type checking |

### Go

| Tool | Command | Purpose |
|------|---------|---------|
| go fmt | `go fmt ./...` | Formatting |
| go vet | `go vet ./...` | Static analysis |
| golangci-lint | `golangci-lint run` | Comprehensive linting |

### Rust

| Tool | Command | Purpose |
|------|---------|---------|
| cargo fmt | `cargo fmt --check` | Formatting |
| cargo clippy | `cargo clippy` | Linting |
| cargo check | `cargo check` | Fast type/syntax check |

### Nix

| Tool | Command | Purpose |
|------|---------|---------|
| nixfmt | `nixfmt [files]` | Formatting |
| nix flake check | `nix flake check` | Flake validation |
| statix | `statix check` | Linting |

### Python

| Tool | Command | Purpose |
|------|---------|---------|
| black | `black [files]` | Formatting |
| ruff | `ruff check [files]` | Linting |
| mypy | `mypy [files]` | Type checking |

## Quality Check Workflow

1. Run the type checker first (`tsc --noEmit`, `mypy`, `cargo check`) for fastest feedback
2. Run the linter (`eslint`, `clippy`, `ruff`) to catch bugs and style issues
3. Run the formatter check (`prettier --check`, `black --check`, `cargo fmt --check`)
4. Report all findings with `file:line` locations

## Auto-Fix Workflow

1. Run the formatter (`prettier --write`, `cargo fmt`, `black`)
2. Run the linter with fix flag (`eslint --fix`, `ruff check --fix`)
3. Verify with check commands that no issues remain
4. Run tests to confirm no regressions were introduced

## Incremental Check (Large Codebases)

```bash
# Get changed files and filter by type
git diff --name-only | grep '\.ts$' | xargs eslint
git diff --name-only | grep '\.py$' | xargs ruff check
```

## Fix Safety Classification

| Safety Level | Examples | Action |
|-------------|----------|--------|
| Safe to auto-apply | Formatting, import sorting, simple style | Apply automatically |
| Review needed | Complex refactors, logic changes | Present diff to user |
| Manual only | Security issues, breaking changes | Flag and explain |

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success, no issues found |
| 1 | Issues found |
| 2 | Configuration or execution error |

## Best Practices

- The agent should run the type checker before the linter for faster feedback
- The agent should verify with tests after applying auto-fixes
- The agent should use project-specific configuration files when available
- The agent should report all issues with `file:line` locations
- The agent should run incremental checks for large codebases
- The agent should separate formatting changes from logic changes in commits

## Anti-Patterns

- **Blind auto-fix**: Always review auto-fix changes and run tests afterward
- **Ignoring warnings**: Address warnings that indicate potential issues, not only errors
- **Skipping type check**: Run the type checker first for a faster feedback loop
- **Running partial toolset**: Run all applicable quality tools (types, lint, format) in sequence
