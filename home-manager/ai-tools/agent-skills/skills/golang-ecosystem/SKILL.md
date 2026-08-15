---
name: golang-ecosystem
description: Use for Go, covering go.mod, go modules, go test, go build, and Go language development patterns.
version: 3.0.0
---

Patterns for Go module management, testing, concurrency, and error handling that go beyond what a
competent Go developer already knows — silent failure modes, capture traps, and exact toolchain
invocations.

## Naming and formatting

Exported identifiers use PascalCase, unexported use camelCase; acronyms stay uppercase (`ServeHTTP`,
`HTTPClient`, `userID`). Single-method interfaces take the method name plus "er" (`Reader`, `Closer`,
`Stringer`). Getters drop the `Get` prefix: `func (u *User) Name() string`, not `GetName()`. Run
gofmt/goimports rather than debating formatting by hand.

## Error handling

**Errors are values inspected at each call site, not exceptions** — a swallowed `if err != nil` is the
single most common Go defect. Wrap with `%w` only when a caller needs `errors.Is`/`errors.As` to unwrap
it; use `%v` when the error is purely for a human and wrapping would leak an implementation detail into
the caller's error-matching surface.

```go
if err != nil {
    return fmt.Errorf("processing user %s: %w", userID, err)
}
```

Sentinel errors (`var ErrNotFound = errors.New(...)`) and custom error types implementing `Error() string`
compose with `errors.Is`/`errors.As`:

```go
if errors.Is(err, ErrNotFound) { ... }

var valErr *ValidationError
if errors.As(err, &valErr) {
    log.Printf("field: %s", valErr.Field)
}
```

`errors.Join(err1, err2, err3)` (Go 1.20+) combines multiple errors without picking one to lose.

Never panic for a recoverable error — panic unwinds past deferred cleanup in ways callers do not expect
and turns a local failure into a process-wide one.

## Interfaces and nil

Accept interfaces, return concrete types; define the interface at the consumer, not the implementer, so
the producer package stays decoupled from every caller's abstraction. Keep interfaces to 1-3 methods —
a wide interface forces every implementer to satisfy methods it does not need.

**A nil concrete type wrapped in an interface is a non-nil interface.** `var p *MyError; var err error = p;
err != nil` is true even though `p` is nil, because the interface value carries a non-nil type descriptor
alongside the nil pointer. This breaks `if err != nil` checks when a function returns a typed nil pointer
through an `error`-typed return — always return a bare `nil`, not a nil-valued typed variable, when there
is no error.

## Loop variables and closures

**Go 1.22 changed loop-variable semantics**: each iteration now gets a fresh variable, so `go func() {
use(item) }()` inside `for _, item := range items` is safe without the pre-1.22 `item := item` shadow or
passing `item` as a parameter. On Go <1.22 (check `go.mod`'s `go` directive before assuming this is safe),
every closure captures the same variable and all goroutines can observe the final iteration's value:

```go
// pre-1.22: bug — every goroutine may print the same, final i
for i, item := range items {
    go func() { process(item) }()
}
// pre-1.22 fix: pass as a parameter to bind per-iteration
for i, item := range items {
    go func(item Item) { process(item) }(item)
}
```

This same trap applies to `defer` inside a loop and to slices of function values built in a loop body.

## Concurrency

`context.Context` is the cancellation and timeout primitive; a goroutine with no way to observe `ctx.Done()`
or a done channel **leaks for the life of the process** because nothing ever unblocks it:

```go
ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
defer cancel()

select {
case result := <-doWork(ctx):
    return result, nil
case <-ctx.Done():
    return nil, ctx.Err()
}
```

A `select` with no `default` blocks until one case is ready; adding `default` makes it non-blocking and
is the difference between "wait for a channel" and "poll a channel." Closing a channel signals "no more
values" to every receiver — closing a channel with active senders, or closing it twice, panics; only the
sender should close, and only once.

`sync.WaitGroup.Add` must happen before the goroutine starts (not inside it) or `Wait` can return before
all goroutines have been counted. `sync.Once` runs its function exactly once even under concurrent callers.
Run tests with `-race` — a data race that a bare `go test` never catches will surface intermittently in
production instead.

## Module resolution

`go.mod` pins the module path, Go version, and dependencies; the `go` directive sets the language version
the compiler accepts, and `toolchain` pins the exact toolchain binary used to build:

```
module github.com/user/project

go 1.26
toolchain go1.26.0

require (
    github.com/pkg/errors v0.9.1
)
```

v0/v1 modules import with no suffix; **v2 and above require the major version in the import path**
(`import "github.com/user/project/v2"`) — omitting it is a common source of "module not found" errors
after a v2 tag. `replace` overrides resolution for local development (`replace github.com/user/lib =>
../lib`) but must not ship in a published module's go.mod, since it silently redirects every consumer.

`go mod tidy` adds missing and removes unused requires — run it after any import change, since a stale
go.mod/go.sum pair fails `go mod verify` in CI even when the code compiles locally. `go mod vendor`
copies dependencies into `vendor/`; once a vendor directory exists, `go build` uses it automatically and
ignores the module cache, which is a frequent source of "I updated go.mod but the old code still runs."

`internal/` packages are import-blocked from outside their parent module at compile time, not by
convention — the compiler enforces it.

## Testing

Table-driven tests are the idiomatic structure for multiple input/output cases:

```go
func TestAdd(t *testing.T) {
    tests := []struct {
        name     string
        a, b     int
        expected int
    }{
        {"positive", 1, 2, 3},
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
```

`t.Helper()` in a test helper attributes failures to the caller's line, not the helper's; `t.Cleanup()`
runs teardown even if the test fails partway, unlike a bare deferred close that a `t.Fatal` can skip past.
`testdata/` is excluded from `go build` automatically — anything under it is safe for fixtures without a
build tag.

Exact invocations:
- `go test ./...` — recurse all packages; a bare `go test` only covers the current directory and silently
  skips the rest, which is easy to mistake for "the suite passed."
- `go test -run TestName` — run only matching tests, by regex against the test name.
- `go test -race` — enables the race detector; several times slower, but the only way to catch a data race
  reliably.
- `go test -coverprofile=c.out` then `go tool cover -html=c.out` — coverage percentage alone hides which
  lines are untested; the HTML view does not.
- `go vet ./...` — catches suspicious constructs (format-string/argument mismatches, unreachable code,
  lock-copying) that compile cleanly but are wrong.

## Modern stdlib (verify version against the target go.mod before relying on these)

- `log/slog` (1.21+): structured logging, preferred over `log` for new code — `slog.Info("user logged in",
  "user_id", userID)`.
- `slices` / `maps` (1.21+): `slices.Sort`, `slices.Contains`, `slices.Index`, `maps.Keys` replace
  hand-rolled loops and `sort.Slice`.
- `cmp` (1.22+): `cmp.Or(userInput, envVar, "default")` for first-non-zero-value defaulting; `cmp.Compare`
  for ordered comparison.
- `net/http` method/path patterns (1.22+): `mux.HandleFunc("GET /api/users/{id}", getUser)` with
  `r.PathValue("id")` reduces the need for a third-party router.
- `tool` directive in go.mod (1.24+): declares tool dependencies, replacing the `tools.go` blank-import
  hack.
- Go 1.26: Green Tea GC is default; `new(MyStruct{Field: "value"})` allows an initial value; generic types
  may self-reference in their own type parameter list; cgo call overhead is reduced roughly 30%.

## Build and lint

`go build -o name` sets the output binary name; `GOOS=linux GOARCH=amd64 go build` cross-compiles without
a toolchain switch. `go generate` executes `//go:generate` directives but does not run automatically —
nothing detects a stale generated file for you. `golangci-lint run` aggregates linters beyond `go vet`;
treat a lint failure as informative rather than blocking unless the project's CI already gates on it.

## Anti-patterns

- **Package-level mutable state** creates hidden cross-call dependencies and is a race condition waiting on
  concurrent access — pass dependencies explicitly instead.
- **Interface pollution**: defining an interface before a second implementation or a decoupling need exists
  adds indirection with no payoff; wait for the second caller.
- **Naked returns** in anything longer than a few lines force the reader to scroll back to the signature to
  know what is returned — use explicit `return` values.
- **`interface{}`/`any` overuse** discards the compiler's type checking and pushes the failure to a runtime
  type assertion; prefer concrete types or a small interface.
- **`init()` overuse** makes initialization order implicit and hard to test in isolation; prefer an
  explicit constructor function that takes parameters.

## Related

- [serena-usage](../serena-usage/SKILL.md) — navigate Go packages and symbol definitions by symbol rather
  than by grep, especially across a large module.
- [context7-usage](../context7-usage/SKILL.md) — pull current stdlib and toolchain documentation instead
  of relying on a possibly stale training-data snapshot of Go's fast-moving standard library.
- [investigation-patterns](../investigation-patterns/SKILL.md) — bisecting a goroutine leak, a data race,
  or a performance regression once `-race` or profiling has located a symptom but not its cause.
