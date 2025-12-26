---
name: Rust Ecosystem
description: This skill should be used when working with Rust projects, "Cargo.toml", "rustc", "cargo build/test/run", "clippy", "rustfmt", or Rust language patterns. Provides comprehensive Rust ecosystem patterns and best practices.
version: 0.1.0
---

<purpose>
Provide comprehensive patterns for Rust language, Cargo project management, and toolchain configuration.
</purpose>

<rust_language>
<ownership_borrowing>
<concept name="ownership">
Each value has exactly one owner. When owner goes out of scope, value is dropped.
Use move semantics by default; explicit Clone when needed.
</concept>

<concept name="borrowing">
<rule name="immutable">&T allows multiple simultaneous borrows</rule>
<rule name="mutable">&mut T allows exactly one mutable borrow</rule>
<rule name="exclusivity">Cannot have &mut T while &T exists</rule>
</concept>

<concept name="lifetimes">
<pattern name="elision">Compiler infers lifetimes in common patterns</pattern>
<pattern name="explicit">fn foo&lt;'a&gt;(x: &'a str) -&gt; &'a str</pattern>
<pattern name="static">'static for values that live entire program</pattern>
</concept>
</ownership_borrowing>

<traits>
<common_traits>
<trait name="Clone">Explicit duplication with .clone()</trait>
<trait name="Copy">Implicit bitwise copy for simple types</trait>
<trait name="Debug">Debug formatting with {:?}</trait>
<trait name="Display">User-facing formatting with {}</trait>
<trait name="Default">Default value construction</trait>
<trait name="PartialEq/Eq">Equality comparison</trait>
<trait name="PartialOrd/Ord">Ordering comparison</trait>
<trait name="Hash">Hashing for HashMap/HashSet keys</trait>
<trait name="From/Into">Type conversions</trait>
<trait name="AsRef/AsMut">Cheap reference conversions</trait>
</common_traits>

<derive_pattern> #[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MyType { ... }
</derive_pattern>
</traits>

<error_handling>
<pattern name="Result">
<description>Recoverable errors with Result&lt;T, E&gt;</description>
<operators>? for early return on Err</operators>
<combinators>map, and_then, unwrap_or, unwrap_or_else</combinators>
</pattern>

<pattern name="Option">
<description>Optional values with Option&lt;T&gt;</description>
<operators>? for early return on None</operators>
<combinators>map, and_then, unwrap_or, unwrap_or_default</combinators>
</pattern>

<pattern name="custom_error">
<description>Define custom error types with thiserror or anyhow</description>
<example>
#[derive(Debug, thiserror::Error)]
enum MyError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Parse error: {msg}")]
    Parse { msg: String },
}
</example>
</pattern>
</error_handling>

<common_patterns>
<pattern name="builder">
<description>Fluent API for complex object construction</description>
<example>
MyStruct::builder()
.field1(value1)
.field2(value2)
.build()
</example>
</pattern>

<pattern name="newtype">
<description>Wrapper type for type safety</description>
<example>struct UserId(u64);</example>
</pattern>

<pattern name="type_state">
<description>Encode state in type system</description>
<use_case>Prevent invalid state transitions at compile time</use_case>
</pattern>
</common_patterns>

<anti_patterns>
<avoid name="unwrap_in_library">Use ? or proper error handling instead</avoid>
<avoid name="clone_abuse">Prefer borrowing when possible</avoid>
<avoid name="string_for_everything">Use enums, newtypes for domain modeling</avoid>
<avoid name="arc_mutex_overuse">Consider channels or ownership patterns first</avoid>
</anti_patterns>
</rust_language>

<cargo>
<project_structure>
<standard_layout>
.
├── Cargo.lock
├── Cargo.toml
├── src/
│   ├── lib.rs          # Library crate root
│   ├── main.rs         # Binary crate root
│   └── bin/            # Additional binaries
├── tests/              # Integration tests
├── benches/            # Benchmarks
└── examples/           # Example code
</standard_layout>

<module_organization>
<pattern name="mod_rs">src/module/mod.rs with submodules</pattern>
<pattern name="file_module">src/module.rs (preferred for simple modules)</pattern>
</module_organization>
</project_structure>

<cargo_toml>
<basic_structure>
[package]
name = "my-crate"
version = "0.1.0"
edition = "2021"
rust-version = "1.70"

[dependencies]
serde = { version = "1.0", features = ["derive"] }

[dev-dependencies]
tokio-test = "0.4"

[build-dependencies]
cc = "1.0"
</basic_structure>

<feature_flags>
[features]
default = ["std"]
std = []
async = ["tokio"]
full = ["std", "async"]
</feature_flags>

<profile_optimization>
[profile.release]
lto = true
codegen-units = 1
panic = "abort"
strip = true

[profile.dev]
opt-level = 0
debug = true
</profile_optimization>
</cargo_toml>

<workspace>
<root_cargo_toml>
[workspace]
resolver = "2"
members = ["crate-a", "crate-b"]

[workspace.package]
version = "0.1.0"
edition = "2021"
license = "MIT"

[workspace.dependencies]
serde = "1.0"
tokio = { version = "1", features = ["full"] }
</root_cargo_toml>

<member_inheritance>
[package]
name = "crate-a"
version.workspace = true
edition.workspace = true

[dependencies]
serde.workspace = true
</member_inheritance>
</workspace>

<commands>
<command name="cargo build">Compile the project</command>
<command name="cargo build --release">Compile with optimizations</command>
<command name="cargo run">Build and run binary</command>
<command name="cargo test">Run all tests</command>
<command name="cargo check">Fast syntax/type check without codegen</command>
<command name="cargo doc --open">Generate and open documentation</command>
<command name="cargo update">Update dependencies</command>
<command name="cargo tree">Display dependency tree</command>
</commands>
</cargo>

<toolchain>
<clippy>
<description>Rust linter for catching common mistakes and improving code</description>
<usage>cargo clippy -- -D warnings</usage>

<configuration>
# In Cargo.toml
[lints.clippy]
pedantic = "warn"
nursery = "warn"
unwrap_used = "deny"
expect_used = "deny"

# Or in clippy.toml

msrv = "1.70"
cognitive-complexity-threshold = 25
</configuration>

<common_lints>
<lint name="clippy::unwrap_used">Prefer ? or proper error handling</lint>
<lint name="clippy::expect_used">Prefer ? or proper error handling</lint>
<lint name="clippy::pedantic">Stricter lints for cleaner code</lint>
<lint name="clippy::nursery">Experimental but useful lints</lint>
</common_lints>
</clippy>

<rustfmt>
<description>Automatic code formatter</description>
<usage>cargo fmt</usage>

<configuration>
# rustfmt.toml
edition = "2021"
max_width = 100
use_small_heuristics = "Max"
imports_granularity = "Crate"
group_imports = "StdExternalCrate"
reorder_imports = true
</configuration>
</rustfmt>

<cargo_nextest>
<description>Next-generation test runner with better output and parallelism</description>
<usage>cargo nextest run</usage>

<features>
<feature>Parallel test execution</feature>
<feature>Better failure output</feature>
<feature>JUnit XML output for CI</feature>
<feature>Test retries</feature>
</features>

<configuration>
# .config/nextest.toml
[profile.default]
retries = 2
slow-timeout = { period = "60s", terminate-after = 2 }
fail-fast = false
</configuration>
</cargo_nextest>

<other_tools>
<tool name="cargo-audit">Security vulnerability scanning</tool>
<tool name="cargo-deny">Dependency license and security checks</tool>
<tool name="cargo-outdated">Check for outdated dependencies</tool>
<tool name="cargo-watch">Auto-rebuild on file changes</tool>
<tool name="cargo-expand">Macro expansion debugging</tool>
</other_tools>
</toolchain>

<context7_integration>
<description>Use Context7 MCP for up-to-date Rust documentation</description>

<rust_libraries>
<library name="The Rust Book" id="/rust-lang/book" />
<library name="Cargo" id="/rust-lang/cargo.git" />
<library name="Rust Clippy" id="/rust-lang/rust-clippy" />
<library name="Rustfmt" id="/rust-lang/rustfmt" />
<library name="Rust Reference" id="/rust-lang/reference.git" />
<library name="Rust by Example" id="/rust-lang/rust-by-example.git" />
<library name="cargo-nextest" id="/websites/nexte_st" />
</rust_libraries>

<usage_patterns>
<pattern name="language_reference">
<step>resolve-library-id libraryName="rust lang"</step>
<step>get-library-docs context7CompatibleLibraryID="/rust-lang/book" topic="ownership"</step>
</pattern>

<pattern name="cargo_configuration">
<step>get-library-docs context7CompatibleLibraryID="/rust-lang/cargo.git" topic="workspace"</step>
</pattern>

<pattern name="clippy_lints">
<step>get-library-docs context7CompatibleLibraryID="/rust-lang/rust-clippy" topic="lints configuration"</step>
</pattern>
</usage_patterns>
</context7_integration>

<best_practices>
<practice>Use cargo check for fast iteration during development</practice>
<practice>Run cargo clippy before committing</practice>
<practice>Format with cargo fmt for consistent style</practice>
<practice>Use workspace for multi-crate projects</practice>
<practice>Prefer &str over String for function parameters</practice>
<practice>Use impl Trait for return types when possible</practice>
<practice>Document public API with /// doc comments</practice>
<practice>Write unit tests alongside code in same file</practice>
<practice>Use integration tests in tests/ for API testing</practice>
<practice>Set rust-version in Cargo.toml for MSRV</practice>
</best_practices>
