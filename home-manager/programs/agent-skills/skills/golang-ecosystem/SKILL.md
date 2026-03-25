---
name: golang-ecosystem
description: "Use when the agent needs to 'write go', 'golang', 'go.mod', 'go module', 'go test', 'go build', or work with Go language development. Provides idiomatic Go patterns for error handling, interfaces, concurrency, modules, testing, and project structure."
---

Comprehensive patterns for Go language development, modules, testing, and idiomatic coding practices.

## Core Concepts

- **Errors are values**: Handle explicitly with `if err != nil`; never ignore returned errors
- **Interfaces**: Accept interfaces, return concrete types; define where used, not implemented
- **Concurrency**: Goroutines for concurrency, channels for communication, `context.Context` for cancellation
- **Zero values**: Design types so zero value is useful (`var buf bytes.Buffer` is ready to use)

## Development Workflow

1. **Analyze**: Check `go.mod` for module/dependencies, review existing patterns, identify interfaces
2. **Implement**: Follow Go naming conventions, use interfaces for abstraction, handle errors explicitly
3. **Validate**: Run `go build`, then `go vet ./...`, then `go test -race ./...`

## Naming Conventions

| Convention | Rule | Example |
|-----------|------|---------|
| Packages | Lowercase, single-word, no underscores | `package httputil` |
| Exported | PascalCase | `func ReadFile()`, `type Handler` |
| Unexported | camelCase | `func parseConfig()`, `var maxRetries` |
| Interfaces | Single-method: method + "er" | `Reader`, `Writer`, `Handler` |
| Acronyms | All uppercase | `ServeHTTP()`, `HTTPClient`, `userID` |
| Getters | No "Get" prefix | `func (u *User) Name() string` |

## Error Handling

```go
// Basic check with context wrapping
result, err := doSomething()
if err != nil {
    return fmt.Errorf("processing user %s: %w", userID, err)
}

// Sentinel errors
var ErrNotFound = errors.New("not found")

// Error inspection
if errors.Is(err, ErrNotFound) { /* handle */ }
var valErr *ValidationError
if errors.As(err, &valErr) { log.Printf("field: %s", valErr.Field) }

// Go 1.20+ multiple errors
err := errors.Join(err1, err2, err3)
```

**Decision**: Use `%w` when callers need `errors.Is`/`errors.As`; use `%v` otherwise.

## Interfaces

```go
// Small, focused interfaces (1-3 methods)
type Handler interface {
    Handle(ctx context.Context, req Request) (Response, error)
}

// Composition
type ReadWriteCloser interface {
    io.Reader
    io.Writer
    io.Closer
}
```

**Common**: `io.Reader`, `io.Writer`, `io.Closer`, `error`, `fmt.Stringer`

## Concurrency

```go
// WaitGroup for fan-out
var wg sync.WaitGroup
for _, item := range items {
    wg.Add(1)
    go func(item Item) {
        defer wg.Done()
        process(item)
    }(item)
}
wg.Wait()

// Context for cancellation
ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
defer cancel()
select {
case result := <-doWork(ctx):
    return result, nil
case <-ctx.Done():
    return nil, ctx.Err()
}
```

**Sync primitives**: `sync.Mutex`, `sync.RWMutex`, `sync.Once`, `sync.WaitGroup`, `sync.Map`

## Modules

```
module github.com/user/project
go 1.23
toolchain go1.23.0

require (
    github.com/pkg/errors v0.9.1
)
```

**Key commands**: `go mod init`, `go mod tidy`, `go get pkg@version`, `go mod vendor`, `go mod verify`

**Versioning**: v0/v1 no path suffix; v2+ include version in import (`import "github.com/user/project/v2"`)

## Testing

```go
// Table-driven tests
func TestAdd(t *testing.T) {
    tests := []struct {
        name     string
        a, b     int
        expected int
    }{
        {"positive", 1, 2, 3},
        {"negative", -1, -2, -3},
        {"zero", 0, 0, 0},
    }
    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            if got := Add(tt.a, tt.b); got != tt.expected {
                t.Errorf("Add(%d, %d) = %d, want %d", tt.a, tt.b, got, tt.expected)
            }
        })
    }
}

// Test helpers
func setupTestDB(t *testing.T) *DB {
    t.Helper()
    db := NewDB()
    t.Cleanup(func() { db.Close() })
    return db
}
```

**Commands**: `go test ./...`, `go test -v`, `go test -run TestName`, `go test -cover`, `go test -race`, `go test -bench .`

## Project Structure

```
cmd/myapp/main.go    # Minimal main; call into internal packages
internal/            # Private packages, not importable externally
pkg/                 # Public library code (optional)
api/                 # API definitions (OpenAPI, protobuf)
testdata/            # Test fixtures (ignored by go build)
```

## Build Commands

| Command | Purpose |
|---------|---------|
| `go build` | Compile package |
| `go build -o name` | Build with custom binary name |
| `go run main.go` | Compile and execute |
| `go fmt ./...` | Format all code |
| `go vet ./...` | Static analysis |
| `go generate` | Run code generators |
| `GOOS=linux GOARCH=amd64 go build` | Cross-compile |

## Anti-Patterns

- **init() overuse**: Prefer explicit initialization with parameters
- **Global state**: Pass dependencies via function params or struct fields
- **Interface pollution**: Define interfaces only when needed for decoupling
- **Naked returns**: Use explicit returns in long functions
- **panic for errors**: Return errors as values; handle explicitly
- **Goroutine leak**: Use `context.Context` or done channels for cancellation
- **Data races**: Use sync primitives; always run tests with `-race`

## Critical Rules

- Handle all errors explicitly; never ignore returned errors
- Run `go vet` and `go test` before committing
- Use `context.Context` for cancellation and timeouts in concurrent code
- Use `gofmt`/`goimports` for consistent formatting

## Context7 Integration

Go documentation: `/golang/website` (trust 8.3), `/golang/tools` (trust 8.3)

```
get-library-docs context7CompatibleLibraryID="/golang/website" topic="go.mod modules"
```

## Related Skills

- `serena-usage`: Navigate Go packages and symbol definitions
- `investigation-patterns`: Debug goroutine leaks, race conditions, performance issues
