---
name: rust-ecosystem
description: "Use when working with Rust projects, 'Cargo.toml', 'cargo build', 'cargo test', 'cargo clippy', 'rustfmt', ownership/borrowing issues, or Rust language patterns. Provides comprehensive Rust ecosystem patterns for language features, Cargo management, and toolchain configuration."
---

Comprehensive patterns for Rust language features, Cargo project management, toolchain configuration, and idiomatic Rust development practices.

## Core Concepts

- **Ownership**: Each value has one owner; when the owner goes out of scope, the value is dropped
- **Borrowing**: `&T` allows multiple immutable borrows; `&mut T` allows exactly one mutable borrow; cannot mix
- **Result/Option**: `Result<T, E>` for recoverable errors, `Option<T>` for optional values; use `?` for propagation
- **Traits**: Define behavior with traits; use `#[derive(...)]` for common implementations (Debug, Clone, PartialEq)

## Ownership and Borrowing

### Lifetimes

```rust
// Explicit lifetime annotation for complex cases
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```

The compiler infers lifetimes in common patterns (elision). Use `'static` for values that live the entire program.

## Common Traits

| Trait | Purpose | Derivable |
|-------|---------|-----------|
| Clone | Explicit duplication with `.clone()` | Yes |
| Copy | Implicit bitwise copy for simple types | Yes |
| Debug | Debug formatting with `{:?}` | Yes |
| Display | User-facing formatting with `{}` | No |
| Default | Default value construction | Yes |
| PartialEq/Eq | Equality comparison | Yes |
| Hash | HashMap/HashSet keys | Yes |
| From/Into | Type conversions | No |

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MyType {
    field1: String,
    field2: i32,
}
```

## Error Handling

### Custom Error Types

```rust
#[derive(Debug, thiserror::Error)]
enum MyError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Parse error: {msg}")]
    Parse { msg: String },
}

fn process_file(path: &str) -> Result<String, MyError> {
    let content = std::fs::read_to_string(path)?; // ? converts io::Error via From
    Ok(content)
}
```

Use `Result<T, E>` for recoverable errors callers should handle. Use `panic!` only for unrecoverable programming errors.

## Common Patterns

### Builder Pattern

```rust
MyStruct::builder()
    .field1(value1)
    .field2(value2)
    .build()
```

Use when the struct has many optional fields. Otherwise use a simple constructor or `Default` trait.

### Newtype Pattern

```rust
struct UserId(u64);
```

Provides type safety by wrapping primitive types.

## Cargo

### Project Structure

```
├── Cargo.toml
├── Cargo.lock
├── src/
│   ├── lib.rs          # Library crate root
│   ├── main.rs         # Binary crate root
│   └── bin/            # Additional binaries
├── tests/              # Integration tests
├── benches/            # Benchmarks
└── examples/           # Example code
```

### Cargo.toml

```toml
[package]
name = "my-crate"
version = "0.1.0"
edition = "2021"
rust-version = "1.83"

[dependencies]
serde = { version = "1.0", features = ["derive"] }

[dev-dependencies]
tokio-test = "0.4"

[profile.release]
lto = true
codegen-units = 1
panic = "abort"
strip = true
```

### Workspace Configuration

```toml
[workspace]
resolver = "2"
members = ["crate-a", "crate-b"]

[workspace.dependencies]
serde = "1.0"
tokio = { version = "1", features = ["full"] }
```

Member crates inherit with `version.workspace = true`, `serde.workspace = true`.

### Key Commands

| Command | Purpose |
|---------|---------|
| `cargo check` | Fast syntax/type check without codegen |
| `cargo build` | Compile the project |
| `cargo build --release` | Compile with optimizations |
| `cargo test` | Run all tests |
| `cargo clippy -- -D warnings` | Lint with warnings as errors |
| `cargo fmt` | Format code |
| `cargo doc --open` | Generate and open documentation |
| `cargo tree` | Display dependency tree |

## Toolchain Configuration

### Clippy

```toml
# In Cargo.toml
[lints.clippy]
pedantic = "warn"
nursery = "warn"
unwrap_used = "deny"
expect_used = "deny"
```

### Rustfmt

```toml
# rustfmt.toml
edition = "2021"
max_width = 100
use_small_heuristics = "Max"
imports_granularity = "Crate"
group_imports = "StdExternalCrate"
```

### cargo-nextest

```toml
# .config/nextest.toml
[profile.default]
retries = 2
slow-timeout = { period = "60s", terminate-after = 2 }
fail-fast = false
```

Additional tools: `cargo-audit` (security scanning), `cargo-deny` (license/security checks), `cargo-outdated` (dependency freshness), `cargo-watch` (auto-rebuild), `cargo-expand` (macro debugging).

## Workflow

1. **Analyze**: Check `Cargo.toml` for crate configuration, review existing patterns and traits, identify ownership/lifetime requirements
2. **Implement**: Design with ownership in mind, use `Result`/`Option` for error handling, follow Rust API guidelines
3. **Validate**: Run `cargo check` for quick validation, `cargo clippy` for lints, `cargo test` for testing

## Critical Rules

- The agent should run `cargo clippy` before committing and fix all warnings
- The agent should prefer safe Rust over `unsafe` blocks; document safety invariants when `unsafe` is needed
- The agent should use `Result` and `Option` types; never use `unwrap()` in library code
- The agent should prefer `&str` over `String` for function parameters
- The agent should write unit tests in the same file, integration tests in `tests/` directory

## Anti-Patterns

- **unwrap() in library code**: Use `?` or proper error handling instead
- **Clone abuse**: Prefer borrowing when possible
- **String for everything**: Use enums and newtypes for domain modeling
- **Arc<Mutex<T>> overuse**: Consider channels or ownership patterns first
