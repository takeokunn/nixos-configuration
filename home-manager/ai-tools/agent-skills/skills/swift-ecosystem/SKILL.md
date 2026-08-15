---
name: swift-ecosystem
description: Use for Swift projects, covering Package.swift, swift build/test/run, swiftc, SwiftLint, SwiftFormat, and cross-platform CLI or library development patterns.
version: 3.0.0
---

Patterns for Swift language hazards, Swift Package Manager, and toolchain configuration for cross-platform
CLI and library development. Assumes familiarity with structs/enums, optionals, and protocols; focuses on
where those constructs fail silently or trap at runtime.

## Type System

- **Never force-unwrap (`!`) or force-try (`try!`) in library code.** Both convert a recoverable absence or
  error into an unconditional crash (`EXC_BAD_INSTRUCTION`) with no call-site indication that failure was
  possible. Use `if let`/`guard let`/`??` for optionals, `do-catch` or propagate `throws` for errors.
- `~Copyable` types (Swift 6+) give unique ownership for resources like file handles and locks. Mark methods
  `borrowing` (read-only) or `consuming` (takes ownership) — mismatched ownership annotations are a compile
  error, not a runtime trap, so the type system catches double-release/use-after-consume at build time.
- Typed throws (Swift 6+): `func parse(_ input: String) throws(ParseError) -> Config` narrows the `catch`
  clause's error type from `any Error` to `ParseError`, so an exhaustive `switch` inside `catch` is checked
  by the compiler instead of trusting the caller to know every case.

## Concurrency and Swift 6 Migration

**Mechanism**: every actor method call that crosses an isolation boundary is an implicit suspension point
(`await`), even when no I/O happens, because the actor's executor serializes access to its state one call at
a time.

- Use actors instead of locks for shared mutable state; the compiler enforces the isolation, a lock does not.
- `@MainActor` isolates a type or function to the main thread. It is easy to over-apply to a whole class and
  silently serialize unrelated background work onto the main thread — scope it to the properties/methods
  that actually touch UI state, not the type as a whole.
- **A type crossing an isolation boundary must be `Sendable`.** Value types with `Sendable` members are
  implicitly `Sendable`. Reference types need explicit conformance, and the compiler cannot verify that a
  `final class: Sendable` isn't mutated from two actors — that guarantee is only as good as the immutability
  (or internal synchronization) you actually wrote.
- `withThrowingTaskGroup` guarantees child tasks are cancelled if the parent scope exits, including via a
  thrown error. A detached `Task {}` spawned inside that scope does not get this guarantee — it outlives the
  group and keeps running after the parent has already unwound.
- **Migrate incrementally.** Enable `-strict-concurrency=complete` (or `.enableUpcomingFeature("StrictConcurrency")`
  per target) while still in Swift 5 language mode, fix the diagnostics it surfaces, then flip
  `.swiftLanguageMode(.v6)`. Flipping to v6 mode directly on an existing codebase surfaces every data-race
  diagnostic at once with no incremental path back to green.

```swift
// Package.swift target settings for incremental migration, then full v6 mode
swiftSettings: [.enableUpcomingFeature("StrictConcurrency")]
// swiftSettings: [.swiftLanguageMode(.v6)]
```

## Memory Management (ARC)

**Mechanism**: ARC reclaims an object only when its retain count reaches zero. A strong reference cycle
(A retains B, B retains A) never reaches zero — it leaks silently, with no crash and no compiler warning.

- Use `[weak self]` in any escaping closure where `self`'s lifetime is not guaranteed to outlive the closure
  (network callbacks, stored completion handlers, delegates held elsewhere).
- Use `unowned` only when you can prove the referenced object outlives the reference. A dangling `unowned`
  access traps at runtime (`EXC_BAD_INSTRUCTION`); a dangling `weak` access degrades to `nil` instead. Pick
  `unowned` deliberately for the trap, not as a shorthand for `weak`.
- Declare delegate properties `weak var delegate: SomeDelegate?` — a strong delegate reference is the most
  common retain cycle in reference-type-heavy code, because the cycle is invisible until memory profiling.

## Error Handling

- `throws`/`do-catch` for recoverable, synchronous errors.
- `Result<Success, Failure>` when the error needs to be stored or passed rather than propagated immediately
  (e.g. captured in a closure for later inspection).
- See typed throws above for Swift 6's narrowed `catch`.

## Testing

swift-testing (stable since Swift 6.0) is preferred for new code. It does not yet support performance or UI
testing — keep XCTest for those.

```swift
import Testing

@Test("Descriptive name")
func subtraction() throws {
  let result = try compute()
  #expect(result > 0)
}

@Test(arguments: [1, 2, 3])
func multipleInputs(value: Int) {
  #expect(value > 0)
}
```

XCTest migration: `XCTAssert*` becomes `#expect(...)`, `XCTAssertEqual(a, b)` becomes `#expect(a == b)`, and
an `XCTestCase` subclass becomes free `@Test` functions grouped under `@Suite`.

## Swift Package Manager

```
Package.swift
Package.resolved
Sources/MyLibrary/MyLibrary.swift
Sources/MyCLI/main.swift
Tests/MyLibraryTests/MyLibraryTests.swift
```

Dependency pinning forms: `.package(url:, exact:)`, `.package(url:, from:)` (floating minor/patch),
`.package(url:, "1.0.0"..<"2.0.0")`, `.package(url:, branch:)`/`revision:`, or `.package(path:)` for a local
sibling package. For cross-platform (macOS + Linux) targets, omit the `platforms:` array entirely — declaring
it locks the package to the listed platforms even when nothing in it is platform-specific.

```
swift build                       # compile
swift build -c release            # compile with optimizations
swift run                         # build and run executable
swift test                        # run all tests
swift test --filter MyTest        # run one test
swift package resolve             # resolve dependencies to Package.resolved
swift package update              # update dependencies within version constraints
swift package show-dependencies   # print dependency tree
swift package clean               # remove build artifacts
```

## Toolchain

**swiftlint** (`swiftlint lint`) — configure via `.swiftlint.yml`. `force_unwrapping`, `force_try`, and
`force_cast` are opt-in rules worth enabling explicitly; they catch exactly the crash-prone patterns above at
lint time instead of at runtime.

```yaml
opt_in_rules:
  - force_unwrapping
  - empty_count
  - closure_spacing
included: [Sources, Tests]
excluded: [.build, Packages]
line_length: { warning: 120, error: 200 }
```

**swiftformat** (`swiftformat .`) — configure via `.swiftformat`.

```
--swiftversion 6.3
--indent 4
--self remove
--wraparguments before-first
--maxwidth 120
```

**sourcekit-lsp** ships with the toolchain; the VS Code Swift extension picks it up automatically once you
run `swift build` to generate the index — no separate LSP install step.

**swift-format** is Apple's official alternative formatter, built into Xcode 16. **swift-docc**
(`swift package generate-documentation`) compiles `///` doc comments into DocC documentation.

## Notable Swift 6.3 changes

- **Swift SDK for Android** is stable — existing packages can add Android support, and Swift code can be
  called from Kotlin/Java via Swift Java / Swift Java JNI Core.
- **`@c` attribute** exposes a Swift function or enum to C code within the same project without a bridging
  header: `@c func myFunction() -> Int32 { 42 }`.
- **Swift Build in SPM** is opt-in, unifying the build backend across platforms.

## Related

- [context7-usage](../context7-usage/SKILL.md) — fetch current Swift language and library documentation
  (swift-argument-parser, swift-log, Vapor, GRDB, etc.) instead of relying on training-data API shapes.
- [serena-usage](../serena-usage/SKILL.md) — navigate protocol conformances and module structure by symbol
  rather than by text search.
- [investigation-patterns](../investigation-patterns/SKILL.md) — trace type errors, optional-handling bugs,
  and concurrency/data-race issues to their cause.
