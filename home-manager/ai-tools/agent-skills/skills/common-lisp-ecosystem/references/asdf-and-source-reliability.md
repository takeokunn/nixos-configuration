## ASDF parallel execution

Concurrent CLI/test invocations that each call `asdf:load-system` can race on an inherited default FASL
cache and fail with "Failed to find the TRUENAME of ...fasl". Initialize output translations in the
launcher, before `load-system`, to a private directory unique to each invocation. A path based only on
the user and implementation still collides between concurrent runs. Keep this initialization in the
packaged launcher so every subcommand inherits it, and verify that simultaneous invocations resolve
their FASL outputs to different directories.

## Constant reload safety

ANSI leaves the consequences undefined if a constant is redefined to a value not `eql` to its current
value; SBCL enforces this by signalling `SB-EXT:DEFCONSTANT-UNEQL`. Because `eql` is identity-based for
compound objects, re-loading a file that `defconstant`s a vector, list, or string literal fails even
when the contents are visually identical, since each load builds a fresh object. Reserve `defconstant`
for scalars and objects with stable `eql` identity. For tables, vectors, quoted lists, string defaults,
and any compound literal that must survive repeated load/compile cycles, use `defparameter` (or
`defvar`). `alexandria:define-constant` with `:test #'equal` is the portable alternative when a genuine
constant is required. (The `eql` redefinition rule is ANSI; the `DEFCONSTANT-UNEQL` condition name is
SBCL-specific.)

```lisp
;; unsafe on reload: each load builds a fresh vector, not eql to the prior one
(defconstant +md5-table+ #(1 2 3 4))    ; => SB-EXT:DEFCONSTANT-UNEQL on reload

;; reload-safe: mutable-binding forms rebind without an eql check
(defparameter +md5-table+ #(1 2 3 4))

;; genuine constant with structural identity: alexandria:define-constant
(alexandria:define-constant +md5-table+ #(1 2 3 4) :test #'equalp)
```

## Read-time evaluation and load order

Read-time evaluation with `#.` is legitimate for handing a literal to a macro that needs it at
expansion time, but its cost is under-appreciated: it converts what looks like an ordinary data
reference into a dependency one phase earlier than compile time, earlier than every intuition about
ordering.

**`#.` creates a read-time dependency.** `#.+some-table+` is evaluated while the file is being read.
The defining unit must therefore be fully loaded before the referencing file is *read*, not before it
is compiled, not before the form runs. In an ASDF system this makes an ordinary-looking constant
reference into a hard `:depends-on` edge, and getting it wrong surfaces as an unbound-variable error
during load rather than as a dependency error. Declare the component dependency explicitly whenever a
file uses `#.` against a constant defined elsewhere.

**`#.` cannot see later forms in its own file.** No matter how far apart the forms are, `#.` can never
reference a value defined later in the same file, because the read of the referencing form happens
before the defining form has been evaluated, a common self-inflicted version of the previous trap.
Prefer a plain symbol reference for plist and table constants unless the value is genuinely required at
read time and guaranteed to exist then; the plain reference is resolved at run time, costs nothing
here, and removes the ordering constraint entirely.

```lisp
;; read-time: requires the defining unit to be loaded before this file is READ
(define-strategy foo :parameters #.+foo-parameters+)

;; run-time reference: no read-time ordering constraint at all
(define-strategy foo :parameters +foo-parameters+)
```

## Source file decomposition constraints

The counterpart to the "shrink the compile unit" advice in Compile/load hang triage below. Splitting
a large file is often right, but the split points are constrained by the grammar, not by taste, and a
split that fights those constraints costs more than the file it replaced.

**Every fragment must read to completion alone.** In a language whose unit of loading is the file, each
fragment must be independently readable: it must contain only complete top-level forms. A single
`defun` continued across a file boundary does not work, and a fragment with one trailing unclosed
parenthesis surfaces as a reader end-of-file, not a helpful structural message. Verify each fragment
boundary by actually reading or loading the fragment: a whitespace-and-conflict-marker diff check does
not detect an unclosed form, and the resulting failure is reported against the fragment that follows,
not the one that is broken.

**Retreat when boundaries are not stable.** Split only where fragment boundaries are genuinely stable.
If achieving a split requires duplicating loader scaffolding across fragments, or cutting through a
form, the file wants one cohesive data fragment plus a thin loader rather than N fragments. The signal
that a decomposition is wrong is mechanical rather than aesthetic: repeated loader text and forms that
resist separation both mean the chosen seams are not real seams in the code's structure.

## Atomic output and temporary files

Writing a file that readers may observe concurrently, and the temporary-file lifecycle that supports
it. The general discipline: identify the commit point (the single irreversible step that makes the new
state visible) and keep everything before it undoable while treating everything after it as best-effort.
Write content first and publish the pointer to it last, since a crash between the two leaves the prior,
complete state reachable rather than a pointer to nothing; a failed attempt must clean up only what it
created, never the last-good state a concurrent reader still depends on. What follows is the CL/POSIX
mechanics that implement that discipline.

**Publish by rename within the target directory.** Create the temporary file in the target's own
parent directory, write it, close and flush the stream, and only then publish it with a rename that
overwrites the target. Renaming across filesystems is not atomic and may not even be a rename; keeping
the temporary beside the target guarantees both files are on one filesystem so the POSIX rename is a
single atomic replacement. `uiop:rename-file-overwriting-target` is the portable form of the publish
step. The ordering matters as much as the call: a rename issued before the stream is closed can publish
a partially flushed file.

**Failure deletes only the temporary.** If writing or renaming fails, cleanup deletes the temporary
file and leaves any existing target untouched: a cleanup path that removes the target as well converts
a failed update into data loss, since the previous good version is exactly what the caller still needs.
Test this invariant by file name or truename, never by raw pathname equality: on macOS the `/tmp` path
a test wrote to canonicalizes to `/private/tmp`, so a pathname-equality assertion fails on a correct
implementation, a routine source of platform-only flaky filesystem tests.

**Retry only on a collision result.** With `:if-exists nil`, `open` returns `nil` for an existing file;
let other open failures propagate. Bound retries with an explicit attempt count. Do not use a subsequent
`probe-file` to reinterpret an error: that introduces a race and can misclassify permission failures.
See [CLHS OPEN](https://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm).

```lisp
(or (open candidate :direction :output :if-exists nil :if-does-not-exist :create)
    :retry)
```

## Numeric frontend correctness

Rules for writing or testing a numeric front end (a parser, a serializer, a converter). Both exist
because the obvious reference point is the host implementation, and the host implementation is not a
specification.

**The host reader is not a floating-point oracle.** Do not validate a float parser by requiring
identity with the implementation's own reader. A reader can be off by one unit in the last place on
subnormals and other hard cases, so a differential test using it as the oracle reports failures where
the implementation under test is the more accurate of the two. Use an exact rational-to-binary64
computation as the reference. Build the oracle from
exact arithmetic: parse the decimal into an exact rational, round to nearest with ties to even against
the binary64 grid, compare bit patterns. When two implementations disagree, decide the winner by exact
rational distance rather than by which one is the host.

**Enforce exponent bounds before constructing anything.** Check the exponent against its maximum
before any coercion, `(expt 10 n)`, ratio construction, or decimal conversion. Implementations disagree
about whether numeric overflow signals at all (one may signal where another returns positive infinity),
so overflow detection must never be implemented by catching a condition the implementation might not
raise. Worse, a token like an exponent of a billion can exhaust storage during the construction that
was supposed to reveal the overflow. Order every numeric front end the same way: validate the textual
exponent range, then build. The same reasoning covers ratio serialization, where denominator
factorization or zero padding must be bounded before it runs rather than after it produces a value.

## Derived state and cache coherence

Three linked invariants for any structure that carries derived state (an index, a compiled plan, a
memoized signature) alongside the data it is derived from.

**Every mutator must go through the rebuild.** A derived index is only as coherent as the least
disciplined mutator. Public mutators that write the underlying collection directly (bypassing the
setter that rebuilds the index) leave newly added entries unusable and removed entries still live.
Route every mutation through the canonical setter, or make each mutator rebuild explicitly. Write the
regression test against behavior, not representation: exercise the operation that consumes the index
immediately after an add and after a remove. A test that inspects the underlying list passes on exactly
the broken code this rule describes, because the list is correct and the index is not.

**A revision counter is only valid if nothing leaks.** A revision counter bumped by the container's own
mutators is a valid cache key only when every path that can invalidate the cache goes through the
container. If the public API hands out the mutable node and edge objects it owns, a caller can mutate
one directly and the counter never moves. An O(1) revision fast path therefore requires an ownership
design (back-references that make every element setter notify its owning containers), not just a
counter. Before adopting a revision-counter cache, enumerate what the public API returns; if any
returned object is both mutable and part of the cached computation, the counter is unsound and the
honest choices are to return copies, add the back-reference notification, or keep validating
structurally.

**Validity checks must not use normalizing accessors.** Write the cache-validity check against the
internal raw representation, not the public getters. Public getters commonly normalize on every call
(a fresh `mapcar`, a fresh hash table), so a validity check built on them allocates on the hot path every
time it runs and defeats the cache it was added to protect. This is a performance bug that looks like
correctness care: the check is right, it is the accessor choice that turns a steady-state O(1) hit into
per-element allocation.

**Never retain caller-owned mutable strings as hash keys.** Common Lisp strings are mutable and `equal`
hashes on content, so retaining a caller-owned string as an `equal` hash key is a latent orphaning bug:
if the caller destructively modifies that string it still owns, the entry becomes unreachable. There is
no error: the lookup simply misses, and the entry leaks for the life of the table. Copy at
key-construction time. Build keys from copied signature strings rather than from the caller's node
names or port names, and rebuild them when the existing invalidation detects a change.

```lisp
;; orphaning: the caller still owns NAME and may destructively modify it
(setf (gethash name table) value)

;; safe: the table owns its key
(setf (gethash (copy-seq name) table) value)
```

## Test suite architecture

- **Zero runtime deps, test-only framework.** Keep the main system's runtime dependencies at zero (or
  minimal) and concentrate test-only dependencies (e.g. FiveAM) in a separate `proj/test` system.
  Runtime source then loads in a plain SBCL image, while the canonical verification path is the one
  that pulls the test framework, commonly a pinned dev shell where the framework is provisioned.
- **Stratified suites.** Stratify the test system into explicit tiers (unit, integration, e2e, perf)
  as separate components, and keep property-based test support in its own support file. This lets a
  fast tier run in isolation and keeps slow/perf tiers opt-in.
- **Layered component decomposition.** For a component that both defines a surface syntax and executes
  it, separate the specification/description layer, the parsing layer, and the orchestration layer into
  distinct units. Beyond clarity, this bounds each compile unit and lets every layer be loaded and
  tested independently.

## Macro hygiene: gensym capture and single evaluation

A `defmacro` that references a variable name the caller might also use captures it silently: the expansion
compiles, and the caller's binding is shadowed with no diagnostic at either site. Every symbol the macro
introduces that the caller did not write must be `gensym`'d; a symbol
intentionally exposed to caller code (anaphora) should be documented as such at the definition site rather
than left to look like an accident.

For macros promising ordinary function-call semantics, evaluate each argument form once and preserve
left-to-right evaluation. Control-flow macros instead follow their documented conditional or repeated
evaluation contract. A function-like macro that evaluates `(incf counter)` twice, or evaluates argument B
before argument A, silently breaks any caller relying on ordinary function-call semantics. Bind each
argument exactly once, in the order it appears, via gensym'd let-bindings before referencing it;
`alexandria:once-only` does this correctly and should be preferred over hand-rolling it inline, since
hand-rolled once-only is itself a common source of the bug it exists to prevent.

```lisp
(defmacro my-max2 (a b)
  (alexandria:once-only (a b)
    `(if (> ,a ,b) ,a ,b)))
```

Verify with `macroexpand-1` against a call site that uses a mutating or side-effecting argument form; a
correct macro's expansion evaluates that form exactly once.

