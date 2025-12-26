---
name: Go Ecosystem
description: This skill should be used when the user asks to "write go", "golang", "go.mod", "go module", "go test", "go build", or works with Go language development. Provides comprehensive Go ecosystem patterns and best practices.
version: 0.1.0
---

<purpose>
Provide comprehensive patterns for Go language development, modules, testing, and idiomatic coding practices.
</purpose>

<go_language>
<naming_conventions>
<convention name="packages">
Lowercase, single-word names. No underscores or mixedCaps.
<example>package httputil</example>
<avoid>package http_util, package httpUtil</avoid>
</convention>

<convention name="exported">
PascalCase for exported (public) identifiers.
<example>func ReadFile(), type Handler, var MaxRetries</example>
</convention>

<convention name="unexported">
camelCase for unexported (private) identifiers.
<example>func parseConfig(), type handler, var maxRetries</example>
</convention>

<convention name="interfaces">
Single-method interfaces: method name + "er" suffix.
<example>Reader, Writer, Closer, Stringer, Handler</example>
</convention>

<convention name="acronyms">
Keep acronyms uppercase: URL, HTTP, ID, API.
<example>func ServeHTTP(), type HTTPClient, var userID</example>
</convention>

<convention name="getters">
No "Get" prefix for getters.
<example>func (u *User) Name() string // not GetName()</example>
</convention>
</naming_conventions>

<formatting>
<rule>Use gofmt/goimports - no manual formatting debates</rule>
<rule>Tabs for indentation, spaces for alignment</rule>
<rule>No semicolons except in for loops and multi-statement lines</rule>
<rule>Opening brace on same line as declaration</rule>
</formatting>

<type_system>
<pattern name="zero_values">
Zero values are meaningful: 0, "", nil, false.
<example>var buf bytes.Buffer // ready to use, no initialization needed</example>
</pattern>

<pattern name="type_assertion">
<safe>value, ok := x.(Type)</safe>
<unsafe>value := x.(Type) // panics if wrong type</unsafe>
</pattern>

<pattern name="type_switch">
switch v := x.(type) {
case string: // v is string
case int:    // v is int
default:     // v is interface{}
}
</pattern>
</type_system>
</go_language>

<error_handling>
<principles>
<principle>Errors are values, not exceptions</principle>
<principle>Handle errors explicitly at each call site</principle>
<principle>Return errors, don't panic</principle>
<principle>Add context when propagating errors</principle>
</principles>

<patterns>
<pattern name="basic_check">
<example>
result, err := doSomething()
if err != nil {
    return fmt.Errorf("failed to do something: %w", err)
}
</example>
</pattern>

<pattern name="wrap_with_context">
<description>Use %w verb to wrap errors for later inspection</description>
<example>
if err != nil {
    return fmt.Errorf("processing user %s: %w", userID, err)
}
</example>
</pattern>

<pattern name="sentinel_errors">
<description>Define package-level error variables</description>
<example>
var ErrNotFound = errors.New("not found")
var ErrInvalidInput = errors.New("invalid input")
</example>
</pattern>

<pattern name="custom_error_type">
<example>
type ValidationError struct {
    Field   string
    Message string
}

func (e \*ValidationError) Error() string {
return fmt.Sprintf("validation failed for %s: %s", e.Field, e.Message)
}
</example>
</pattern>

<pattern name="error_inspection">
<example>
// Check for specific error
if errors.Is(err, ErrNotFound) { ... }

// Extract custom error type
var valErr \*ValidationError
if errors.As(err, &valErr) {
log.Printf("field: %s", valErr.Field)
}
</example>
</pattern>

<pattern name="multiple_errors">
<description>Go 1.20+ errors.Join</description>
<example>
err := errors.Join(err1, err2, err3)
</example>
</pattern>
</patterns>

<anti*patterns>
<avoid>if err != nil { return err } // loses context</avoid>
<avoid>panic() for recoverable errors</avoid>
<avoid>* = riskyOperation() // ignoring errors</avoid>
</anti_patterns>
</error_handling>

<interfaces>
<principles>
<principle>Accept interfaces, return concrete types</principle>
<principle>Keep interfaces small (1-3 methods)</principle>
<principle>Define interfaces where they are used, not implemented</principle>
<principle>Implicit satisfaction - no "implements" keyword</principle>
</principles>

<common_interfaces>
<interface name="io.Reader">Read(p []byte) (n int, err error)</interface>
<interface name="io.Writer">Write(p []byte) (n int, err error)</interface>
<interface name="io.Closer">Close() error</interface>
<interface name="error">Error() string</interface>
<interface name="fmt.Stringer">String() string</interface>
</common_interfaces>

<patterns>
<pattern name="interface_definition">
<example>
type Handler interface {
    Handle(ctx context.Context, req Request) (Response, error)
}
</example>
</pattern>

<pattern name="interface_composition">
<example>
type ReadWriteCloser interface {
    io.Reader
    io.Writer
    io.Closer
}
</example>
</pattern>

<pattern name="empty_interface">
<description>interface{} or any (Go 1.18+) accepts all types</description>
<warning>Avoid when possible - loses type safety</warning>
</pattern>
</patterns>
</interfaces>

<modules>
<go_mod_structure>
<example>
module github.com/user/project

go 1.21

require (
github.com/pkg/errors v0.9.1
golang.org/x/sync v0.3.0
)

require (
golang.org/x/sys v0.10.0 // indirect
)
</example>
</go_mod_structure>

<commands>
<command name="go mod init">Initialize new module</command>
<command name="go mod tidy">Add missing, remove unused dependencies</command>
<command name="go get pkg@version">Add or update dependency</command>
<command name="go mod download">Download dependencies to cache</command>
<command name="go mod vendor">Create vendor directory</command>
<command name="go mod verify">Verify dependencies</command>
</commands>

<versioning>
<pattern name="semantic_import">v0.x.x and v1.x.x: no path suffix</pattern>
<pattern name="v2_plus">v2+: include version in import path</pattern>
<example>import "github.com/user/project/v2"</example>
</versioning>

<replace_directive>
<description>Override module location for local development</description>
<example>
replace github.com/user/lib => ../lib
replace github.com/user/lib v1.0.0 => ./local-lib
</example>
</replace_directive>
</modules>

<project_structure>
<standard_layout>
<directory name="cmd/">Main applications (cmd/myapp/main.go)</directory>
<directory name="internal/">Private packages, not importable externally</directory>
<directory name="pkg/">Public library code (optional, controversial)</directory>
<directory name="api/">API definitions (OpenAPI, protobuf)</directory>
<directory name="configs/">Configuration files</directory>
<directory name="scripts/">Build/install scripts</directory>
<directory name="testdata/">Test fixtures</directory>
</standard_layout>

<patterns>
<pattern name="main_package">
cmd/myapp/main.go should be minimal - call into internal packages
</pattern>

<pattern name="internal_protection">
internal/ packages cannot be imported from outside parent module
</pattern>

<pattern name="one_package_per_directory">
Each directory = one package (except _test packages)
</pattern>
</patterns>
</project_structure>

<testing>
<file_naming>
<pattern>foo.go → foo_test.go</pattern>
<pattern>Test functions: func TestXxx(t *testing.T)</pattern>
<pattern>Benchmark functions: func BenchmarkXxx(b *testing.B)</pattern>
<pattern>Example functions: func ExampleXxx()</pattern>
</file_naming>

<patterns>
<pattern name="table_driven_tests">
<example>
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
</example>
</pattern>

<pattern name="test_helpers">
<example>
func setupTestDB(t *testing.T) *DB {
    t.Helper()
    db := NewDB()
    t.Cleanup(func() { db.Close() })
    return db
}
</example>
</pattern>

<pattern name="testdata_directory">
testdata/ is ignored by go build, used for test fixtures
</pattern>
</patterns>

<commands>
<command name="go test">Run tests in current package</command>
<command name="go test ./...">Run all tests recursively</command>
<command name="go test -v">Verbose output</command>
<command name="go test -run TestName">Run specific test</command>
<command name="go test -cover">Show coverage percentage</command>
<command name="go test -coverprofile=c.out">Generate coverage profile</command>
<command name="go test -bench=.">Run benchmarks</command>
<command name="go test -race">Enable race detector</command>
</commands>
</testing>

<concurrency>
<goroutines>
<pattern name="launch">
go func() {
    // concurrent work
}()
</pattern>

<pattern name="with_waitgroup">
<example>
var wg sync.WaitGroup
for _, item := range items {
    wg.Add(1)
    go func(item Item) {
        defer wg.Done()
        process(item)
    }(item)
}
wg.Wait()
</example>
</pattern>
</goroutines>

<channels>
<pattern name="unbuffered">ch := make(chan int) // synchronous</pattern>
<pattern name="buffered">ch := make(chan int, 10) // async up to buffer size</pattern>
<pattern name="receive_only">func consumer(ch <-chan int)</pattern>
<pattern name="send_only">func producer(ch chan<- int)</pattern>

<pattern name="select">
<example>
select {
case msg := <-ch1:
    handle(msg)
case ch2 <- value:
    // sent
case <-ctx.Done():
    return ctx.Err()
default:
    // non-blocking
}
</example>
</pattern>

<pattern name="close_channel">
close(ch) // signal no more values
for v := range ch { } // receive until closed
</pattern>
</channels>

<context>
<description>Use context.Context for cancellation and timeouts</description>
<example>
ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
defer cancel()

select {
case result := <-doWork(ctx):
return result, nil
case <-ctx.Done():
return nil, ctx.Err()
}
</example>
</context>

<sync_package>
<type name="sync.Mutex">Mutual exclusion lock</type>
<type name="sync.RWMutex">Read-write lock</type>
<type name="sync.Once">Execute exactly once</type>
<type name="sync.WaitGroup">Wait for goroutine completion</type>
<type name="sync.Map">Concurrent map (specialized use cases)</type>
</sync_package>
</concurrency>

<anti_patterns>
<avoid name="init_overuse">Prefer explicit initialization over init()</avoid>
<avoid name="global_state">Minimize package-level variables</avoid>
<avoid name="interface_pollution">Don't define interfaces prematurely</avoid>
<avoid name="naked_returns">Avoid in functions longer than a few lines</avoid>
<avoid name="panic_for_errors">Use panic only for unrecoverable situations</avoid>
<avoid name="goroutine_leak">Always ensure goroutines can exit</avoid>
<avoid name="data_race">Use -race flag during development</avoid>
<avoid name="empty_interface_overuse">Prefer concrete types or small interfaces</avoid>
</anti_patterns>

<build_commands>
<command name="go build">Compile package</command>
<command name="go build -o name">Specify output name</command>
<command name="go install">Compile and install to GOPATH/bin</command>
<command name="go run main.go">Compile and run</command>
<command name="go fmt ./...">Format all code</command>
<command name="go vet ./...">Static analysis</command>
<command name="go generate">Run code generators</command>
<command name="GOOS=linux GOARCH=amd64 go build">Cross-compile</command>
</build_commands>
