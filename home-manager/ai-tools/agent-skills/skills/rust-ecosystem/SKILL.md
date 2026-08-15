---
name: rust-ecosystem
description: Use when working with Rust projects — Cargo.toml, rustc, cargo build/test/clippy/rustfmt, borrow-checker errors, lock-registry design, checked/saturating arithmetic on untrusted sizes, or Rust language patterns.
version: 3.0.0
---

Non-obvious Rust patterns and toolchain hazards: borrow-checker refactors, trust-boundary type
design, lock-registry concurrency, and the debug/release divergence that makes a test suite lie
about what ships.

## Ownership and borrowing

**Split borrows by naming the fields, not by extracting a method.** "Extract a method" is the
reflex refactor, and in Rust it routinely fails with E0502 the moment the extracted body writes
one field while the caller still holds a read of another. A method taking `&mut self` borrows the
whole struct; the borrow checker only reasons about disjoint fields when those fields are named at
the call site, and a method signature hides them.

- When an extraction hits E0502, give the helper exactly the fields it needs —
  `fn encode_row(pool: &mut EncodePool, line: &Line)` — instead of `&mut self`. The immutable
  borrow of one field and the mutable borrow of the other stay disjoint, and the call site keeps
  ownership of the composition.
- A module-level free function taking two `&mut` fields is sometimes the forced, correct design
  rather than a smell. Note at the definition site that the shape is borrow-checker-driven, so a
  later refactor does not "tidy" it back into a method and reintroduce the conflict.

```rust
// E0502: &mut self borrows the whole struct while self.screen is still borrowed.
let line = self.screen.get_line(row);
self.encode_row(line);

// Compiles: disjoint field borrows, named at the call site.
let line = self.screen.get_line(row);
encode_row(&mut self.pool, line);
```

## Type design

**Prefer named fields over positional tuples for anything that isn't a wire format.** A type alias
over a tuple makes a positional shape look intentional while leaving it entirely unchecked. In
`type EncodedLine = (usize, String, Vec<(usize, usize, u32, u32, u64, u32)>, Vec<usize>)`, every
same-typed field is silently swappable and the compiler accepts any permutation — the alias
supplies a name for the aggregate but no name for any of its parts, which is exactly where the
ordering knowledge was needed. A cache key of three bare `u64`s has the same defect: transposing
two of them still typechecks and produces a hash that matches the wrong row.

- Positional shape belongs only at a serialization boundary, where an external wire format
  dictates it. Inside the call graph, use a struct with named fields, or a newtype per component,
  so an ordering mistake is unrepresentable rather than merely unlikely.
- Keep tuple destructuring localized inside the codec functions that own the wire format. The
  external shape is preserved unchanged; no ordinary function ever receives a bare tuple.

### Trust boundaries

A trust boundary is any point where data crosses from an unvalidated source (config files,
external processes, network, shell) into code that acts on it. The general rule: never let a raw
`String` or `Map` travel to the final action site (process spawn, shell emission, SQL) still typed
as raw text. Wrap validated values in newtypes so the type system, not developer discipline,
enforces that only validated data reaches the boundary. (The language-neutral rules — enforce
limits before allocating, validate before applying normalizing coercion, pin an identity rather
than re-opening a path, never interpolate untrusted data into output — live in the
[trust-boundaries](../trust-boundaries/SKILL.md) skill; what follows is the Rust expression of
them.)

**Validated newtype over raw string.** Public/config model fields that carry constrained values
should use validated newtypes instead of raw `String` / `HashMap<String, String>`, so direct Rust
construction cannot bypass the serde/runtime validators. Do not add an infallible `From<&str>` or
`From<String>` for a validated newtype — that reintroduces unchecked construction and defeats the
boundary. Provide only a fallible constructor (`TryFrom` / `new -> Result`).

```rust
// Raw form: any String can be constructed, validation is optional and easy to skip.
struct Config { name: String }

// Validated form: the only way to build a Name is through validation.
struct Name(String);
impl Name {
  fn new(s: &str) -> Result<Self, NameError> { /* enforce invariants */ }
}
struct Config { name: Name }
```

**Single source for schema and validator.** Derive the published schema and the runtime validator
from the same constant. If a JSON Schema advertises an inclusive maximum while the runtime
constructor enforces an exclusive one, a documented-valid config can be rejected at runtime (or
worse, the reverse). Publish numeric bounds (e.g. `exclusiveMaximum`) from the same crate-level
constant the constructor checks, not from a separately written field-level annotation.

**Runtime validate mirrors serde.** When a type is constructed both via serde (deserialization) and
directly by API callers, the direct-construction `validate()` path is a second trust boundary. It
must mirror the serde validators exactly — same limits, same shared constants (e.g. a single
`MAX_DEPTH` used by validator, compiler, and executor). A separate model-side limit that disagrees
with the downstream limit lets a "valid" value fail later during generation or execution. Route
both paths through one constant.

**Convert at entry, require at boundary.** Keep serde-facing models ergonomic (plain `String`), but
convert to the validated newtype immediately after validation, and require the newtype at every
downstream API. Downstream backend/executor signatures should take `&ValidatedName`, not `&str`, so
no call site can smuggle an unvalidated value to the action point.

**Distinct types for distinct semantics.** When two values are both "strings" but have different
execution semantics, give them distinct output types so call sites cannot mix them. A concrete
argument value and an already-quoted shell word are not interchangeable even though both are text.
Cross the boundary explicitly (`as_str()` / `into_inner()`) so the conversion is visible and
intentional. This also applies to identity handles parsed from external process stdout — parse into
a validated `Id` type, then require that `Id` for follow-up operations.

**Reject non-UTF-8 before validation.** Bytes read from external processes are not guaranteed
UTF-8. Reject non-UTF-8 before constructing an execution handle; lossy conversion
(`from_utf8_lossy` / `to_string_lossy`) is acceptable only for user-facing diagnostics, never for
values that will be fed back into execution.

**Sanitize config values that get rendered to a terminal.** Configuration files and environment
variables become untrusted input the moment their values are rendered rather than merely compared.
A colour or style string, a set of hint-label characters, a prompt loaded from a file — each
reaches the terminal as bytes, and a terminal interprets the escape sequences it receives
regardless of who authored them. A config value is therefore an injection surface with the same
shape as untrusted network data: it can move the cursor, clear the screen, or drive a clipboard
write.

- Filter with an allowlist of exactly one escape family, never a denylist of known-bad ones. For
  style values, decode escapes only into ANSI SGR CSI sequences terminated by `m`; drop OSC, cursor
  movement, clear-screen, embedded printable text, and every malformed sequence. A denylist cannot
  enumerate the terminal's dialect, and each terminal has a slightly different one.
- Strip Unicode control characters, not only the ASCII C0 range, from any value that becomes an
  on-screen glyph. Preserve ordinary space where it is a legitimate input character (a search
  term); discard whitespace only where the value is a set of distinct single-character labels.
- Filtering can empty the set. After discarding unsafe characters and deduplicating, assert that
  enough usable values remain — at least two distinct labels, for instance — and fall back to the
  built-in default rather than proceeding with a degenerate set. Silently operating on one label,
  or none, is a worse outcome than ignoring the configuration.
- Reject a repeated singleton CLI option instead of letting the last occurrence win. Last-wins
  override lets a later argument quietly replace a value the caller believes is in effect, and
  nothing on the failure path is observable.

**Bound latency, not only length.** A read capped at N bytes bounds how much you accept, not how
long you wait. Opening a FIFO blocks inside `open()` before a single byte is read, so a size limit
gives no protection at all to a startup path or a request handler pointed at a path that an
attacker — or an ordinary mistake — controls. Length and latency are separate budgets and each
needs its own enforcement.

- On Unix, open with `O_NONBLOCK` and then interrogate the descriptor you actually got, via
  `fstat`, rejecting anything that is not a regular file. Checking the opened descriptor rather
  than stat-ing the path and then opening it also closes the path-replacement race between those
  two calls: you are asking about the object you hold, not the name you looked up.
- Stream the read with a cap of `limit + 1` bytes, not `limit`. Reading exactly `limit` cannot
  distinguish "the input is exactly at the limit" from "the input was truncated here"; the one
  extra byte is what makes over-limit detectable rather than inferred.
- Validate UTF-8 at the same boundary and reject on failure. A read that respected both budgets but
  yielded invalid bytes is still invalid input, and deferring the check just moves the failure
  somewhere with less context.

**Deterministic ordering.** For maps whose iteration order feeds generated output (emitted options,
template expansion, argv), use `BTreeMap` rather than `HashMap` at the public model type so
ordering is deterministic at the type level. Downstream code then iterates directly instead of
re-sorting at each call site.

**Propagate, don't silently skip.** A loader that reads and validates a directory of config files
is a trust boundary. It must not silently skip malformed files or directory-entry errors, because
that makes validation/listing output disagree with actual on-disk state and can hide broken or
unsafe definitions. Ignore only explicitly out-of-scope inputs (e.g. unsupported extensions).
Propagate parse/validation errors with the file path in the error chain, and propagate `read_dir`
entry errors with directory context. Sort successful results for deterministic output.

## Polymorphism

Choose the dispatch mechanism from whether the set of implementers is open or closed. A closed,
compile-time-known set can use static dispatch with no vtable or heap allocation; an open/plugin
set needs dynamic dispatch.

For a closed set, combine a trait with an enum whose variants are the implementers, annotated with
`#[enum_dispatch]`. This gives zero-cost (static) polymorphism — the enum forwards each trait
method to the active variant with no `Box<dyn Trait>` indirection — while keeping one exhaustive
registration point. Use `Box<dyn Trait>` instead for an open set: plugin boundaries or heterogeneous
collections whose members are decided at runtime.

```rust
#[enum_dispatch]
trait Fetcher {
  async fn get_info(&self, cl: &Client) -> PackageInfo;
}

#[enum_dispatch(Fetcher)]
enum FetcherDispatch {
  FromGit(FromGit),
  FromRegistry(FromRegistry),
}
```

To add a new implementer and keep the enum the single source of truth: create the implementation
module and implement the trait for the new type; add a variant to the dispatch enum (which carries
the `#[enum_dispatch(Trait)]` attribute); wire construction/selection (CLI variant, factory, or
detection logic) to produce the new variant. Because dispatch is exhaustive over the enum, the
compiler flags every match that must handle the new variant — the type system drives completeness.

## Concurrency

A single lock around a collection of independently-usable resources converts concurrent work into
serial work. When each entry can do meaningful work without touching its siblings, the collection
lock should protect only the collection's shape, and each entry should carry its own lock.

**Registry of locks.** Holding the guard of a `Mutex<HashMap<K, V>>` for the duration of the work
done on one entry serializes every entry against every other. Polling, parsing, rendering, and
transcoding on one session then block all other sessions even though they share no state — the map
was never the contended resource, it was just the thing in the way. The fix is to make the map hold
handles rather than values.

- Use `Mutex<HashMap<K, Arc<Mutex<V>>>>`. Hold the map lock only long enough to clone, insert,
  remove, or snapshot an `Arc`; release it; then lock the selected entry. All real work happens
  under the entry lock and none of it under the map lock.
- Fix the lock hierarchy in one direction and never invert it: never reacquire the map lock while
  holding an entry guard. This ordering is what keeps the design deadlock-free, and because it is
  invisible in the type system it has to be stated at the registry definition where a future caller
  will read it.
- Removal from the map is the linearization point for shutdown. Once an entry is out of the map no
  new work can acquire it, and in-flight holders drain naturally through their `Arc` without needing
  a separate quiescence flag.
- When pruning against a snapshot taken earlier, compare with `Arc::ptr_eq` before removing rather
  than trusting the key. Between the snapshot and the prune the map lock was released, so an entry
  may have been removed and a fresh one inserted under the same key; key equality alone would drop
  the replacement.

**Asymmetric poison recovery.** A poisoned mutex records that some thread panicked while holding it
— it does not establish that the data is unusable. How to respond depends on the signature of the
API doing the locking, and treating every case identically makes one of the two cases lie.

- A fallible single-resource API (returning `Result`) should map both map-level and entry-level
  poison to an error. The caller asked about one resource whose invariants may genuinely be broken,
  and refusing is the honest answer.
- An infallible enumerating API (list, count, snapshot) must recover a poisoned entry lock with
  `PoisonError::into_inner()` instead of skipping the entry. Omitting it reports a live resource as
  absent, and the caller has no way to distinguish "no such resource" from "that resource panicked
  once" — the enumeration becomes wrong rather than merely degraded.

## Language-feature traps

Return-position `impl Trait` now captures all in-scope lifetimes by default — a change from prior
editions. Use `use<..>` to explicitly specify captured parameters when you need the narrower
behavior.

`async fn` in traits is stable and no longer requires the `async-trait` crate for most cases, but
the default does not add `Send` bounds. For public traits in libraries, use `trait_variant::make` to
provide `Send` bounds, or callers on multi-threaded executors will hit an unhelpful "future is not
`Send`" error far from the trait definition.

## Toolchain

**`#[expect]` over `#[allow]` for suppressions you intend to fix.** Both silence a lint, but they
age in opposite ways. `#[expect]` additionally asserts that the lint would have fired; once the
underlying code is fixed the annotation itself warns via `unfulfilled_lint_expectations`, which is
the property that makes it self-cleaning. `#[allow]` is silent forever, so it accumulates and
outlives whatever justified it. (This is the attribute `#[expect(...)]`, entirely unrelated to the
`clippy::expect_used` lint, which is about `Option::expect`.)

- When `unfulfilled_lint_expectations` fires, delete the annotation — the lint is telling you the
  suppression is now dead. Re-broadening it to `#[allow]` to quiet the build discards exactly the
  signal you configured it to produce.
- Do not attach `#[expect]` to a macro that expands at many call sites. The expectation can be
  fulfilled at some expansion sites and unfulfilled at others, leaving the annotation simultaneously
  necessary and stale with no edit that satisfies every site. Move it down onto the specific
  generated function, or the specific call site, that actually needs it.

Non-default lint groups and MSRV live in `clippy.toml`, separate from `Cargo.toml`:

```
msrv = "1.94"
cognitive-complexity-threshold = 25
```

**Dead code at registration boundaries.** rustc computes reachability over the Rust call graph. A
function whose only caller is an attribute macro's generated registration path — FFI entry points,
`#[no_mangle]` exports, `#[wasm_bindgen]` or `#[pyfunction]` surfaces, `inventory`- and
`linkme`-style distributed registries — has no in-language caller at all, so `cargo check` and
`cargo test` report it as `dead_code`. The warning is an accurate statement about the reference
graph and says nothing about whether the function is used.

- Treat `dead_code` on an export or registration surface as boundary noise first, before treating it
  as a finding. Confirm the registration path — does the macro emit a static registration, an
  exported symbol, or a table entry that the host resolves at runtime? Deleting on the strength of
  the warning removes a live entry point, and the breakage surfaces only when the host loads the
  artifact, far from the edit.
- Suppress it narrowly at the boundary function or type rather than crate-wide, so genuinely dead
  code elsewhere still surfaces. A crate-level allow buys silence at the cost of the lint's entire
  value.
- [investigation-patterns](../investigation-patterns/SKILL.md) covers dead-code removal discipline
  generally; its rules assume the true callers are findable within the program, which is exactly the
  assumption a registration macro breaks.

`cargo nextest run` accepts retry and timeout configuration that `cargo test` has no equivalent for:

```
[profile.default]
retries = 2
slow-timeout = { period = "60s", terminate-after = 2 }
fail-fast = false
```

## Release profile and test gating

Assertions and tests behave differently under the release profile than under dev. `debug_assert!`
and any debug-only invariant check compile out when built with `--release`, so tests that expect a
`debug_assert` to fire will not observe it in an optimized build. Packaging pipelines (distro
packages, sandboxed builds) commonly build with `--release` and no network, so the release build is
the one that actually ships — design tests to stay green there.

**`debug_assert!` is stripped in release.** It is fine as an internal sanity check, but it is not a
runtime safety boundary — the real boundary must be a normal check that returns a `Result` (or
panics unconditionally), so the guarantee survives optimization. Gate `#[should_panic]` tests that
exercise `debug_assert!` behavior behind `#[cfg(debug_assertions)]`, otherwise they fail (no panic)
under a release test run.

```rust
#[cfg(debug_assertions)]
#[test]
#[should_panic]
fn rejects_invalid_in_debug() { /* triggers a debug_assert! path */ }
```

**Checked or saturating arithmetic on untrusted sizes.** Arithmetic on sizes derived from untrusted
input — pane dimensions, match counts, display widths, byte lengths — must not use plain `+`, `-`,
or `*`. The reason is the same debug/release divergence as above, read from the other direction: an
overflowing expression panics in a debug build and silently wraps to a wrong value in release, so
the failure mode differs between the build you test and the build that ships. Both halves are bad,
and neither is visible from the other profile.

- Choose the operation by whether failure carries information. Use `checked_*` (yielding `Option`
  or `Result`) where an out-of-range value means something has actually gone wrong and the caller
  must decide; use `saturating_*` where clamping to the bound is the correct answer and continuing
  is safe, as with a rendering coordinate that should stop at the screen edge.
- Never reach for `wrapping_*` to silence an overflow warning. Wrapping is correct for hashes and
  deliberate modular arithmetic and for nothing else — on a size it converts a loud debug panic into
  a silent release-only wrong answer, which is precisely the outcome the other two operations exist
  to prevent.
- Keep boundary tests at zero, at radix powers, at `usize::MAX`, and near coordinate limits. These
  are the inputs that discriminate between the two build profiles; without them the debug build
  stays green and the release wrap ships undetected.

**Sandbox build test gating.** Packaged/sandboxed builds typically run `--release` with no network
access and a minimal toolchain (no git, no external CLIs). Tests that need network, git, or external
tools will fail there if run unconditionally. Mark network/IO/external-tool tests with `#[ignore]`
or put them behind a feature flag so the default (and packaged) test run passes in the sandbox.
Reserve real external backends for opt-in test profiles; use recording/mock backends by default.

## Context7 library IDs

| Library | ID |
|---|---|
| The Rust Book | `/rust-lang/book` |
| Cargo | `/rust-lang/cargo.git` |
| Rust Clippy | `/rust-lang/rust-clippy` |
| Rustfmt | `/rust-lang/rustfmt` |
| Rust Reference | `/rust-lang/reference.git` |
| Rust by Example | `/rust-lang/rust-by-example.git` |
| cargo-nextest | `/websites/nexte_st` |

## Related

- [serena-usage](../serena-usage/SKILL.md) — navigate trait implementations and module hierarchies
- [context7-usage](../context7-usage/SKILL.md) — fetch current Rust book, cargo, and clippy documentation
- [investigation-patterns](../investigation-patterns/SKILL.md) — debug borrow-checker errors, lifetime issues, and performance bottlenecks
- [trust-boundaries](../trust-boundaries/SKILL.md) — language-neutral rules behind the trust-boundary patterns above: limits before allocation, validation before normalizing coercion, pinning identity instead of re-opening a path, and never interpolating untrusted data into output
