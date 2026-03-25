---
name: swift-ecosystem
description: "Use when working with Swift projects, Package.swift, swift build, swift test, swift run, swiftc, SwiftLint, SwiftFormat, or Swift language patterns. Provides comprehensive Swift ecosystem patterns for cross-platform CLI and library development."
---

Comprehensive patterns for Swift language, Swift Package Manager, and toolchain configuration for cross-platform CLI and library development. The agent should prefer value types, use async/await for concurrency, and follow Swift API design guidelines.

## Critical Rules

- Run `swiftlint` before committing; fix all warnings
- Never use force unwrap (`!`) in library code
- Use `async/await` over completion handlers for new code
- Always use `[weak self]` in escaping closures unless self lifetime is guaranteed

## Workflow

1. **Analyze** — Check Package.swift for configuration, review existing patterns and protocols, identify concurrency requirements
2. **Implement** — Design with value types and protocols, use optionals and error handling correctly, follow Swift API design guidelines
3. **Validate** — Run `swift build` for compilation, `swiftlint` for style/safety, `swift test` for testing

## Type System

### Value vs Reference Types

The agent should prefer structs for data models without identity. Use classes only when reference semantics or inheritance is required. Use enums with associated values for algebraic data types.

```swift
// Prefer structs for data (value type, copy semantics)
struct User: Identifiable, Timestamped {
    let id: String
    let createdAt: Date
    let name: String
}

// Enums with associated values
enum ParseError: Error {
    case invalidFormat
    case missingField(String)
}
```

### Optionals

```swift
// Safe unwrapping patterns
if let value = optionalValue { use(value) }           // if-let binding
guard let value = optionalValue else { return }        // guard-let for early exit
let value = optionalValue ?? defaultValue              // nil-coalescing
let result = object?.property?.method()                // optional chaining
```

Never use `!` to force unwrap. Never use `try!` in production code.

### Protocols and Generics

```swift
// Protocol-oriented programming: composition over inheritance
protocol Describable {
    var description: String { get }
}

extension Describable {
    var description: String { String(describing: self) }
}

// Generic constraints
func process<T: Codable & Sendable>(_ item: T) -> Data {
    try! JSONEncoder().encode(item)
}
```

### Noncopyable Types (Swift 6+)

```swift
struct UniqueResource: ~Copyable {
    consuming func release() { /* takes ownership */ }
}
func process(borrowing data: SomeData) { /* read-only */ }
```

Use for resources requiring unique ownership (file handles, locks). Mark methods with `borrowing`/`consuming` for explicit ownership.

## Error Handling

| Situation | Pattern |
|-----------|---------|
| Synchronous with recoverable errors | `throws` with `do-catch` |
| Need to store or pass the error | `Result` type |
| Simple absence without error details | `Optional` |
| Swift 6+ with specific error type | Typed `throws(ErrorType)` |

```swift
// Typed throws (Swift 6+)
func parse(_ input: String) throws(ParseError) -> Config {
    guard !input.isEmpty else { throw ParseError.invalidFormat }
    // parsing logic
}
```

## Concurrency

```swift
// async/await (Swift 5.5+)
func fetchData() async throws -> Data {
    let (data, _) = try await URLSession.shared.data(from: url)
    return data
}

// Actors for thread-safe mutable state
actor Counter {
    private var value = 0
    func increment() { value += 1 }
    func getValue() -> Int { value }
}

// Task groups for parallel execution
func processAll(_ items: [Item]) async throws -> [Result] {
    try await withThrowingTaskGroup(of: Result.self) { group in
        for item in items { group.addTask { try await process(item) } }
        return try await group.reduce(into: []) { $0.append($1) }
    }
}
```

Mark types as `Sendable` when they cross concurrency boundaries. Use actors for shared mutable state instead of locks.

## Memory Management

```swift
// Prevent retain cycles in closures
service.fetch { [weak self] result in
    self?.handleResult(result)
}
```

Use `[weak self]` in escaping closures. Use `unowned` only when the referenced object is guaranteed to outlive the closure.

## Common Patterns

### Builder Pattern

```swift
struct RequestBuilder {
    private var method: String = "GET"
    private var headers: [String: String] = [:]

    func method(_ m: String) -> RequestBuilder {
        var copy = self; copy.method = m; return copy
    }
    func header(_ key: String, _ value: String) -> RequestBuilder {
        var copy = self; copy.headers[key] = value; return copy
    }
    func build() -> Request { Request(method: method, headers: headers) }
}
```

### Property Wrappers

```swift
@propertyWrapper
struct Clamped<Value: Comparable> {
    private var value: Value
    private let range: ClosedRange<Value>
    var wrappedValue: Value {
        get { value }
        set { value = min(max(newValue, range.lowerBound), range.upperBound) }
    }
    init(wrappedValue: Value, _ range: ClosedRange<Value>) {
        self.range = range
        self.value = min(max(wrappedValue, range.lowerBound), range.upperBound)
    }
}
```

## Swift Package Manager

### Project Structure

```
.
├── Package.swift
├── Sources/
│   ├── MyLibrary/
│   │   └── MyLibrary.swift
│   └── MyCLI/
│       └── main.swift
├── Tests/
│   └── MyLibraryTests/
│       └── MyLibraryTests.swift
└── Plugins/
```

### Package.swift Configuration

```swift
// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "MyPackage",
    products: [
        .library(name: "MyLibrary", targets: ["MyLibrary"]),
        .executable(name: "my-cli", targets: ["MyCLI"])
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-argument-parser", from: "1.5.0"),
        .package(url: "https://github.com/apple/swift-log", from: "1.6.0")
    ],
    targets: [
        .target(name: "MyLibrary", dependencies: [
            .product(name: "Logging", package: "swift-log")
        ]),
        .executableTarget(name: "MyCLI", dependencies: [
            "MyLibrary",
            .product(name: "ArgumentParser", package: "swift-argument-parser")
        ]),
        .testTarget(name: "MyLibraryTests", dependencies: ["MyLibrary"])
    ]
)
```

For cross-platform (macOS + Linux), omit the `platforms` array entirely. Only specify platforms when using platform-specific APIs.

### Key Commands

| Command | Purpose |
|---------|---------|
| `swift build` | Compile the package |
| `swift build -c release` | Compile with optimizations |
| `swift test` | Run all tests |
| `swift test --filter MyTest` | Run specific test |
| `swift package resolve` | Resolve dependencies |
| `swift package update` | Update dependencies |

## Toolchain

### SwiftLint

```yaml
# .swiftlint.yml
disabled_rules:
  - trailing_whitespace
opt_in_rules:
  - empty_count
  - force_unwrapping
included:
  - Sources
  - Tests
excluded:
  - .build
line_length:
  warning: 120
  error: 200
```

### SwiftFormat

```
# .swiftformat
--swiftversion 6.0
--indent 4
--maxwidth 120
--wraparguments before-first
--importgrouping testable-last
```

## Testing

### Swift Testing Framework (Swift 6+)

```swift
import Testing

@Test func addition() { #expect(1 + 1 == 2) }

@Test("Descriptive name")
func subtraction() throws {
    let result = try compute()
    #expect(result > 0)
}

@Test(arguments: [1, 2, 3])
func multipleInputs(value: Int) { #expect(value > 0) }

@Suite("Calculator Tests")
struct CalculatorTests {
    @Test func add() { #expect(1 + 1 == 2) }
    @Test func multiply() { #expect(2 * 3 == 6) }
}
```

Use swift-testing for new test code. For existing XCTest code, migrate by replacing `XCTAssert` with `#expect` and `XCTestCase` classes with `@Test` functions.

## Anti-Patterns to Avoid

- **Force unwrap (`!`)** — use if-let, guard-let, or nil-coalescing
- **Force try (`try!`)** — use do-catch or propagate with throws
- **Class for data** — use struct for value semantics
- **`Any`/`AnyObject` overuse** — use generics or protocols with associated types
- **Retain cycles** — use `[weak self]` or `[unowned self]` capture lists

## Error Escalation

- **Low:** SwiftLint warning about style — fix warning, maintain idiomatic code
- **Medium:** Type safety error or optional handling issue — redesign with proper types
- **High:** Breaking change in public API — stop, present migration options
- **Critical:** Force unwrap or unsafe code in library — block operation, require safe alternatives
