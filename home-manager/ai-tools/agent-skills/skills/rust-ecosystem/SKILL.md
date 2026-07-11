---
name: Rust Ecosystem
description: This skill should be used when working with Rust projects, "Cargo.toml", "rustc", "cargo build/test/run", "clippy", "rustfmt", or Rust language patterns. Provides comprehensive Rust ecosystem patterns and best practices.
version: 2.1.0
---

<purpose>
  Provide comprehensive patterns for Rust language, Cargo project management, and toolchain configuration.
</purpose>

<tools>
  <tool>Read - Analyze Cargo.toml and Rust source files</tool>
  <tool>Edit - Modify Rust code and Cargo configuration</tool>
  <tool>Bash - Run cargo build, cargo test, cargo clippy commands</tool>
  <tool>mcp__plugin_claude-code-home-manager_context7__query-docs - Fetch latest Rust documentation</tool>
</tools>

<concepts>
  <concept name="ownership">Each value has one owner; when owner goes out of scope, value is dropped</concept>
  <concept name="borrowing">Immutable (&amp;T) allows multiple borrows; mutable (&amp;mut T) allows exactly one; cannot mix</concept>
  <concept name="result_option">Result for recoverable errors (Ok/Err), Option for optional values (Some/None); use ? for propagation</concept>
  <concept name="traits">Define behavior with traits; use derive for common implementations (Debug, Clone, PartialEq)</concept>
</concepts>

<rust_language>
  <ownership_borrowing>
    <concept name="ownership">
      <description>Each value has exactly one owner. When owner goes out of scope, value is dropped.</description>
      <use>Use move semantics by default; explicit Clone when needed</use>
    </concept>

    <concept name="borrowing">
      <description>Immutable and mutable references with strict rules</description>
      <rules priority="critical">
        <rule>&amp;T allows multiple simultaneous borrows</rule>
        <rule>&amp;mut T allows exactly one mutable borrow</rule>
        <rule>Cannot have &amp;mut T while &amp;T exists</rule>
      </rules>
    </concept>

    <concept name="lifetimes">
      <description>Lifetime annotations for reference validity</description>
      <pattern name="elision">
        <description>Compiler infers lifetimes in common patterns</description>
      </pattern>
      <pattern name="explicit">
        <description>Explicit lifetime annotations for complex cases</description>
        <example>
          fn foo&lt;'a>(x: &amp;'a str) -> &amp;'a str {
            x
          }
        </example>
      </pattern>
      <pattern name="static">
        <description>'static for values that live entire program</description>
      </pattern>
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

    <pattern name="derive">
      <description>Automatically implement common traits</description>
      <example>
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        struct MyType {
          field1: String,
          field2: i32,
        }
      </example>
    </pattern>
  </traits>

  <error_handling>
    <pattern name="Result">
      <description>Recoverable errors with Result with T and E type parameters</description>
      <operators>? for early return on Err</operators>
      <combinators>map, and_then, unwrap_or, unwrap_or_else</combinators>
      <decision_tree name="when_to_use">
        <question>Is this a recoverable error that callers should handle?</question>
        <if_yes>Return Result type with appropriate error variant</if_yes>
        <if_no>Use panic only for unrecoverable programming errors</if_no>
      </decision_tree>
    </pattern>

    <pattern name="Option">
      <description>Optional values with Option with T type parameter</description>
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
      <note>Use thiserror 2.0+ which provides improved error messages and automatic From implementations.</note>
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
      <decision_tree name="when_to_use">
        <question>Does the struct have many optional fields or complex construction logic?</question>
        <if_yes>Implement builder pattern for ergonomic construction</if_yes>
        <if_no>Use simple constructor function or Default trait</if_no>
      </decision_tree>
    </pattern>

    <pattern name="newtype">
      <description>Wrapper type for type safety</description>
      <example>
        struct UserId(u64);
      </example>
    </pattern>

    <pattern name="type_state">
      <description>Encode state in type system</description>
      <use_case>Prevent invalid state transitions at compile time</use_case>
    </pattern>
  </common_patterns>

  <trust_boundary_types>
    <principle>
      A trust boundary is any point where data crosses from an unvalidated source (config files, external processes, network, shell) into code that acts on it. The general rule: never let a raw String or Map travel to the final action site (process spawn, shell emission, SQL) still typed as raw text. Wrap validated values in newtypes so the type system, not developer discipline, enforces that only validated data reaches the boundary.
    </principle>

    <pattern name="validated_newtype_over_raw_string">
      <description>Public/config model fields that carry constrained values should use validated newtypes instead of raw String / HashMap&lt;String, String&gt;, so direct Rust construction cannot bypass the serde/runtime validators.</description>
      <rule>Do not add infallible From&lt;&amp;str&gt; or From&lt;String&gt; for a validated newtype — that reintroduces unchecked construction and defeats the boundary. Provide only a fallible constructor (TryFrom / new -&gt; Result).</rule>
      <example>
        // Raw form: any String can be constructed, validation is optional and easy to skip.
        struct Config { name: String }

        // Validated form: the only way to build a Name is through validation.
        struct Name(String);
        impl Name {
          fn new(s: &amp;str) -&gt; Result&lt;Self, NameError&gt; { /* enforce invariants */ }
        }
        struct Config { name: Name }
      </example>
    </pattern>

    <pattern name="single_source_for_schema_and_validator">
      <description>Derive the published schema and the runtime validator from the same constant. If a JSON Schema advertises an inclusive maximum while the runtime constructor enforces an exclusive one, a documented-valid config can be rejected at runtime (or worse, the reverse).</description>
      <rule>Keep schema generation, serde deserialization, and direct constructors on one typed boundary. Publish numeric bounds (e.g. exclusiveMaximum) from the same crate-level constant the constructor checks, not from a separately written field-level annotation.</rule>
    </pattern>

    <pattern name="runtime_validate_mirrors_serde">
      <description>When a type is constructed both via serde (deserialization) and directly by API callers, the direct-construction validate() path is a second trust boundary. It must mirror the serde validators exactly — same limits, same shared constants (e.g. a single MAX_DEPTH used by validator, compiler, and executor).</description>
      <rule>A separate model-side limit that disagrees with the downstream limit lets a "valid" value fail later during generation or execution. Route both paths through one constant.</rule>
    </pattern>

    <pattern name="convert_at_entry_require_at_boundary">
      <description>Keep serde-facing models ergonomic (plain String), but convert to the validated newtype immediately after validation, and require the newtype at every downstream API. Tests and real execution then share one un-bypassable boundary.</description>
      <rule>Downstream backend/executor signatures should take &amp;ValidatedName, not &amp;str, so no call site can smuggle an unvalidated value to the action point.</rule>
    </pattern>

    <pattern name="distinct_types_for_distinct_semantics">
      <description>When two values are both "strings" but have different execution semantics, give them distinct output types so call sites cannot mix them. A concrete argument value and an already-quoted shell word are not interchangeable even though both are text.</description>
      <rule>Cross the boundary explicitly (as_str() / into_inner()) so the conversion is visible and intentional. This also applies to identity handles parsed from external process stdout — parse into a validated Id type, then require that Id for follow-up operations.</rule>
    </pattern>

    <pattern name="reject_non_utf8_before_validation">
      <description>Bytes read from external processes are not guaranteed UTF-8. Reject non-UTF-8 before constructing an execution handle; lossy conversion (from_utf8_lossy / to_string_lossy) is acceptable only for user-facing diagnostics, never for values that will be fed back into execution.</description>
    </pattern>

    <pattern name="deterministic_ordering">
      <description>For maps whose iteration order feeds generated output (emitted options, template expansion, argv), use BTreeMap rather than HashMap at the public model type so ordering is deterministic at the type level. Downstream code then iterates directly instead of re-sorting at each call site.</description>
    </pattern>

    <pattern name="propagate_dont_silently_skip">
      <description>A loader that reads and validates a directory of config files is a trust boundary. It must not silently skip malformed files or directory-entry errors, because that makes validation/listing output disagree with actual on-disk state and can hide broken or unsafe definitions.</description>
      <rule>Ignore only explicitly out-of-scope inputs (e.g. unsupported extensions). Propagate parse/validation errors with the file path in the error chain, and propagate read_dir entry errors with directory context. Sort successful results for deterministic output.</rule>
    </pattern>
  </trust_boundary_types>

  <polymorphism>
    <principle>
      Choose the dispatch mechanism from whether the set of implementers is open or closed. A closed, compile-time-known set can use static dispatch with no vtable or heap allocation; an open/plugin set needs dynamic dispatch.
    </principle>

    <pattern name="trait_plus_enum_dispatch">
      <description>For a closed set of implementers, combine a trait with an enum whose variants are the implementers, annotated with #[enum_dispatch]. This gives zero-cost (static) polymorphism — the enum forwards each trait method to the active variant with no Box&lt;dyn Trait&gt; indirection — while keeping one exhaustive registration point.</description>
      <example>
        #[enum_dispatch]
        trait Fetcher {
          async fn get_info(&amp;self, cl: &amp;Client) -&gt; PackageInfo;
        }

        #[enum_dispatch(Fetcher)]
        enum FetcherDispatch {
          FromGit(FromGit),
          FromRegistry(FromRegistry),
        }
      </example>
      <decision_tree name="when_to_use">
        <question>Is the set of implementers closed and known at compile time?</question>
        <if_yes>trait + enum_dispatch — static dispatch, exhaustive matching, no allocation</if_yes>
        <if_no>Box&lt;dyn Trait&gt; — open set, plugin boundaries, or heterogeneous collections whose members are decided at runtime</if_no>
      </decision_tree>
    </pattern>

    <pattern name="extension_procedure">
      <description>Formalize adding a new implementer so the enum stays the single source of truth.</description>
      <step order="1">Create the implementation module and implement the trait for the new type.</step>
      <step order="2">Add a variant to the dispatch enum (the enum carries the #[enum_dispatch(Trait)] attribute).</step>
      <step order="3">Wire construction/selection (CLI variant, factory, or detection logic) to produce the new variant.</step>
      <note>Because dispatch is exhaustive over the enum, the compiler flags every match that must handle the new variant — the type system drives completeness.</note>
    </pattern>
  </polymorphism>

  <edition_2024_features>
    <feature name="async_closures">
      <description>Rust now supports async closures like async || {} which return futures when called (Edition 2024).</description>
      <example>
        let closure = async || {
          do_async_work().await
        };
        let result = closure().await;
      </example>
    </feature>
    <feature name="rpit_lifetime_capture">
      <description>impl Trait return types now capture all in-scope lifetimes by default. Use use&lt;..&gt; to explicitly specify captured parameters.</description>
    </feature>
    <feature name="diagnostic_do_not_recommend">
      <description>#[diagnostic::do_not_recommend] attribute lets crate authors control which trait implementations are suggested in compiler diagnostics.</description>
    </feature>
    <feature name="temporary_lifetime_changes">
      <description>Changed scope of temporaries for if let expressions and tail expressions in blocks (Edition 2024).</description>
    </feature>
    <feature name="async_fn_in_traits">
      <description>async fn in trait definitions and implementations is stable since Rust 1.75. No longer requires the async-trait crate for most use cases.</description>
      <example>
        trait DataStore {
          async fn fetch(&amp;self, id: u64) -&gt; Result&lt;Data, Error&gt;;
        }

        impl DataStore for PostgresStore {
          async fn fetch(&amp;self, id: u64) -&gt; Result&lt;Data, Error&gt; {
            sqlx::query_as("SELECT * FROM data WHERE id = $1")
              .bind(id)
              .fetch_one(&amp;self.pool)
              .await
          }
        }
      </example>
      <note>For public traits in libraries, consider using trait_variant::make to provide Send bounds.</note>
    </feature>
    <feature name="let_chains">
      <description>Chain let patterns in if and while conditions (stable in edition 2024).</description>
      <example>
        if let Some(user) = get_user(id)
          &amp;&amp; let Some(email) = user.email
          &amp;&amp; email.ends_with("@company.com")
        {
          send_internal_notification(email);
        }
      </example>
    </feature>
  </edition_2024_features>

  <anti_patterns>
    <avoid name="unwrap_in_library">
      <description>Using unwrap() in library code</description>
      <instead>Use ? or proper error handling instead</instead>
    </avoid>

    <avoid name="clone_abuse">
      <description>Cloning values unnecessarily</description>
      <instead>Prefer borrowing when possible</instead>
    </avoid>

    <avoid name="string_for_everything">
      <description>Using String for all domain values</description>
      <instead>Use enums, newtypes for domain modeling</instead>
    </avoid>

    <avoid name="arc_mutex_overuse">
      <description>Defaulting to Arc with Mutex with T for concurrency</description>
      <instead>Consider channels or ownership patterns first</instead>
    </avoid>
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
      <pattern name="mod_rs">
        <description>src/module/mod.rs with submodules</description>
      </pattern>
      <pattern name="file_module">
        <description>src/module.rs (preferred for simple modules)</description>
      </pattern>
    </module_organization>
  </project_structure>

  <cargo_toml>
    <basic_structure>
      [package]
      name = "my-crate"
      version = "0.1.0"
      edition = "2024" # Current edition (stable since Rust 1.85)
      rust-version = "1.85" # Minimum supported version for edition 2024

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
      resolver = "3"  # Default for edition 2024
      members = ["crate-a", "crate-b"]

      [workspace.package]
      version = "0.1.0"
      edition = "2024"
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

    <decision_tree name="when_to_use">
      <question>Do you have multiple related crates in one repository?</question>
      <if_yes>Use workspace to share dependencies and build configuration</if_yes>
      <if_no>Single crate project without workspace structure</if_no>
    </decision_tree>
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
      <file_reference>In Cargo.toml</file_reference>
      [lints.clippy]
      pedantic = "warn"
      nursery = "warn"
      unwrap_used = "deny"
      expect_used = "deny"

      <file_reference>Or in clippy.toml</file_reference>

      msrv = "1.94"
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
      <file_reference>rustfmt.toml</file_reference>
      edition = "2024"
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
      <file_reference>.config/nextest.toml</file_reference>
      [profile.default]
      retries = 2
      slow-timeout = { period = "60s", terminate-after = 2 }
      fail-fast = false
    </configuration>
  </cargo_nextest>

  <other_tools>
    <tool name="cargo-audit">
      <description>Security vulnerability scanning</description>
    </tool>
    <tool name="cargo-deny">
      <description>Dependency license and security checks</description>
    </tool>
    <tool name="cargo-outdated">
      <description>Check for outdated dependencies</description>
    </tool>
    <tool name="cargo-watch">
      <description>Auto-rebuild on file changes</description>
    </tool>
    <tool name="cargo-expand">
      <description>Macro expansion debugging</description>
    </tool>
    <tool name="cargo-binstall">
      <description>Install pre-built binaries from crates.io (faster than cargo install)</description>
    </tool>
    <tool name="cargo-vet">
      <description>Supply chain security — audit third-party crate reviews</description>
    </tool>
    <tool name="cargo-mutants">
      <description>Mutation testing to verify test effectiveness</description>
    </tool>
  </other_tools>
</toolchain>

<release_profile_and_test_gating>
  <principle>
    Assertions and tests behave differently under the release profile than under dev. debug_assert! and any debug-only invariant check compile out when built with --release, so tests that expect a debug_assert to fire will not observe it in an optimized build. Packaging pipelines (distro packages, sandboxed builds) commonly build with --release and no network, so the release build is the one that actually ships — design tests to stay green there.
  </principle>

  <pattern name="debug_assert_stripped_in_release">
    <description>debug_assert! is a no-op in release builds. It is fine as an internal sanity check, but it is not a runtime safety boundary — the real boundary must be a normal check that returns a Result (or panics unconditionally), so the guarantee survives optimization.</description>
    <rule>Gate #[should_panic] tests that exercise debug_assert! behavior behind #[cfg(debug_assertions)], otherwise they fail (no panic) under a release test run.</rule>
    <example>
      #[cfg(debug_assertions)]
      #[test]
      #[should_panic]
      fn rejects_invalid_in_debug() { /* triggers a debug_assert! path */ }
    </example>
  </pattern>

  <pattern name="sandbox_build_test_gating">
    <description>Packaged/sandboxed builds typically run --release with no network access and a minimal toolchain (no git, no external CLIs). Tests that need network, git, or external tools will fail there if run unconditionally.</description>
    <rule>Mark network/IO/external-tool tests with #[ignore] or put them behind a feature flag so the default (and packaged) test run passes in the sandbox. Reserve real external backends for opt-in test profiles; use recording/mock backends by default.</rule>
  </pattern>
</release_profile_and_test_gating>

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
      <step order="1">
  <action>resolve-library-id libraryName="rust lang"</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
      <step order="1">
  <action>get-library-docs context7CompatibleLibraryID="/rust-lang/book" topic="ownership"</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    </pattern>

    <pattern name="cargo_configuration">
      <step order="1">
  <action>get-library-docs context7CompatibleLibraryID="/rust-lang/cargo.git" topic="workspace"</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    </pattern>

    <pattern name="clippy_lints">
      <step order="1">
  <action>get-library-docs context7CompatibleLibraryID="/rust-lang/rust-clippy" topic="lints configuration"</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    </pattern>
  </usage_patterns>
</context7_integration>

<best_practices>
  <practice priority="critical">Use cargo check for fast iteration during development</practice>
  <practice priority="critical">Run cargo clippy before committing</practice>
  <practice priority="critical">Format with cargo fmt for consistent style</practice>
  <practice priority="high">Use workspace for multi-crate projects</practice>
  <practice priority="high">Prefer &amp;str over String for function parameters</practice>
  <practice priority="high">Use impl Trait for return types when possible</practice>
  <practice priority="medium">Document public API with /// doc comments</practice>
  <practice priority="medium">Write unit tests alongside code in same file</practice>
  <practice priority="medium">Use integration tests in tests/ for API testing</practice>
  <practice priority="medium">Set rust-version in Cargo.toml for MSRV</practice>
</best_practices>

<rules priority="critical">
  <rule>Run cargo clippy before committing; fix all warnings</rule>
  <rule>Prefer safe Rust over unsafe blocks; document safety invariants when unsafe is needed</rule>
  <rule>Use Result and Option types; never unwrap() in library code</rule>
</rules>

<rules priority="standard">
  <rule>Use cargo fmt for consistent formatting</rule>
  <rule>Prefer &amp;str over String for function parameters</rule>
  <rule>Write unit tests in same file, integration tests in tests/ directory</rule>
  <rule>Use cargo check for fast iteration during development</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Understand Rust code requirements</objective>
    <step order="1">
  <action>1. Check Cargo.toml for crate configuration</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Review existing patterns and traits</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Identify ownership and lifetime requirements</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="implement">
    <objective>Write safe, idiomatic Rust code</objective>
    <step order="1">
  <action>1. Design with ownership in mind</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Use Result/Option for error handling</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Follow Rust API guidelines</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="validate">
    <objective>Verify Rust code correctness</objective>
    <step order="1">
  <action>1. Run cargo check for quick validation</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Run cargo clippy for lints</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Run cargo test for testing</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Clippy warning about style</example>
    <example severity="medium">Borrow checker error</example>
    <example severity="high">Breaking change in public API</example>
    <example severity="critical">Unsafe code without proper justification</example>
  </examples>
</error_escalation>

<constraints>
  <must>Prefer safe Rust over unsafe blocks</must>
  <must>Use Result and Option for error handling</must>
  <must>Follow Rust API guidelines for public APIs</must>
  <avoid>Using unwrap() in library code</avoid>
  <avoid>Unnecessary Clone implementations</avoid>
  <avoid>Unsafe code without safety documentation</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Navigate trait implementations and module hierarchies</skill>
  <skill name="context7-usage">Fetch Rust book, cargo, and clippy documentation</skill>
  <skill name="investigation-patterns">Debug borrow checker errors, lifetime issues, and performance bottlenecks</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate code patterns and references in this skill domain</agent>
  <agent name="quality-assurance">Review implementation quality against this skill guidance</agent>
  <agent name="code-quality">Analyze code complexity and suggest refactoring improvements</agent>
</related_agents>
