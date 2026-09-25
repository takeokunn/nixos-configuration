---
name: common-lisp-ecosystem
description: Use for Common Lisp, SBCL, or Coalton, covering CLOS, ASDF, package hygiene, condition design, format-string injection, Unicode predicates, reader macros, macro hygiene, atomic file publishing, and hash-table key safety. Also SBCL execution and debugging (--script, REPL/SLY, backtraces, save-lisp-and-die, profiling, compile/load hangs, terminating a stuck SBCL, sb-thread/sb-cover hazards).
metadata:
  version: "4.0.0"
---

Silent failure modes in Common Lisp, CLOS, ASDF, and package systems: cases where correct-looking
code loads and runs without error but does the wrong thing. Ordinary CLOS, condition, and ASDF usage
is assumed knowledge; this file exists for the traps that pass a naive review. The SBCL operations part
near the end covers running, debugging, profiling, and shipping SBCL programs.

## CLOS: the construction boundary is the class, not the constructor

Exporting a class exports `make-instance` on it. Any validation or invariant that lives only in a
convenience constructor like `make-foo` is advisory: a caller who writes `(make-instance 'foo :slot
...)` bypasses all of it and gets an object the rest of the system assumes cannot exist. The
convenience constructor reads like the API, so reviewers check it and stop; nothing in `make-foo`
hints that a second, unvalidated construction path is exported alongside it.

Enforce invariants in an `initialize-instance :after` (or applicable `shared-initialize`) method when
direct construction must preserve them. A slot `:initform` supplies a default but does not validate
supplied values. Keeping the class package-internal narrows the supported API; it is not an access-control
boundary. Document any unchecked constructor as an internal convention, not a security guarantee.

```lisp
;; invariant enforced at the construction boundary, not in a helper
(defmethod initialize-instance :after ((c connection) &key)
  (unless (slot-boundp c 'endpoint)
    (error "connection: endpoint is required"))
  (check-type (slot-value c 'timeout) (integer 1 3600)))

;; a trusted fast path must not be an initarg; keep it internal
(defvar *trusted-construction* nil)   ; bound only by package-internal callers
```

## Conditions: format-string injection and unbounded payloads

**Never pass input-derived data as a FORMAT control string.** FORMAT is a full language (`~R`, `~V`,
deeply nested `~{~}`, and the recursive `~?` directive), so a control string under attacker influence
is CL's format-string vulnerability, with resource exhaustion and information disclosure available
directly from the directive set. Error helpers are where this is introduced, because `(error msg)`
reads so naturally.

```lisp
;; unsafe: MSG is interpreted as a control string
(defun fail (msg) (error msg))

;; safe: MSG is data
(defun fail (msg) (error "~A" msg))
```

Write `(error 'my-error :detail msg)` or `(error "~A" msg)` instead, and audit for any call (`error`,
`warn`, `cerror`, `format` itself) whose control string is a variable rather than a literal.

Sanitizing report *output* is not enough: a public condition that retains an unbounded payload keeps
it alive for the lifetime of the condition object, and anything that later prints, logs, or serializes
the condition re-materializes it. Bound and sanitize at initialization, in the slot, not at render
time; validate limits against fixed hard maxima before allocating any buffer, and truncate during
rendering rather than building an unbounded intermediate string and cutting it afterwards. The general
rule (enforce limits before allocation, not at emission) belongs to
[trust-boundaries](../trust-boundaries/SKILL.md); the shape it takes here is that the intermediate is
usually a `with-output-to-string` whose size nobody bounded:

```lisp
;; wrong shape: the blowup happens before the limit is consulted
(let ((s (with-output-to-string (o) (print-object huge o))))
  (subseq s 0 (min +max+ (length s))))

;; right shape: the limit bounds the work, not just the result
(with-output-to-string (o)
  (print-object-bounded huge o :limit +max+))
```

## Input validation traps

Two CL-specific ways a validation boundary silently stops validating.

**Standard character predicates are Unicode-aware.** `digit-char-p` is not an ASCII test: on
implementations with full Unicode support it accepts decimal digits from any script (fullwidth,
Arabic-Indic, Devanagari, others) and returns their numeric weight. `alpha-char-p` and `alphanumericp`
widen the same way. Any grammar defined over U+0030–U+0039 that reaches for `digit-char-p` has quietly
widened its accepted language. The widening is invisible in review because `digit-char-p` is exactly
what the spec prose seems to ask for, and every ASCII test case passes; it surfaces as a downstream
conversion failure, or as two components disagreeing about whether a token was a number. Define
ASCII-only predicates once and use them at every stage of the grammar: start detection, digit
consumption, numeric conversion, stream framing. A single stage still using the standard predicate
reintroduces the disagreement. Keep fullwidth, Arabic-Indic, and Devanagari digits as standing
regression inputs.

```lisp
(declaim (inline ascii-digit-p))
(defun ascii-digit-p (ch)
  (and (char<= #\0 ch #\9) (- (char-code ch) (char-code #\0))))

;; (digit-char-p #\３) => 3   on a Unicode-capable implementation
;; (ascii-digit-p  #\３) => NIL
```

The standard permits but does not require non-ASCII digit recognition; the major implementations do
it. Write the ASCII predicate rather than testing which behavior your implementation has.

**Validate before you normalize.** When a guard exists to reject input class A, and a normalizing
coercion maps A into B, running the coercion first makes the guard unreachable: it stays in the
source, passes review, and is a no-op. The observed case: a recursive directory delete whose
`:validate` option could not reject a bare file pathname, because `ensure-directory-pathname` had
already folded the file name into directory form before the guard ran. "Normalize, then validate" is
the safest-sounding possible ordering and is exactly backwards. Assert on the raw argument in the
first form of the function body. Watch every `uiop` pathname coercion
(`ensure-directory-pathname`, `ensure-pathname`, `parse-namestring` with defaults); each is lossy about
precisely the distinction a guard is usually there to enforce.

These instantiate general rules owned by [trust-boundaries](../trust-boundaries/SKILL.md): limits
enforced before allocation, raw input validated before a normalizing coercion.

## Package hygiene: stub packages contaminate a shared image

A verification script that defines a partial stub package before loading selected real sources
permanently occupies that name in the image's global package namespace; `defpackage` is not scoped
and does not unwind. If a later path in the same image evaluates the canonical definition, its
`:import-from` fails on symbols the stub never exported, and the error points at the canonical file,
which is innocent. Run stub-defining harnesses in their own process, never sharing an image with
canonical loads. When a definition fails that has no business failing, establish first whether the
image is fresh: a polluted image produces errors that indict entirely unrelated code. This is the
concrete reason behind the fresh-process-per-unit rule in the SBCL headless verification harness below.

## Definition reachability verification

A whole class of "loads fine, explodes at call time" failures comes from confusing three different
things: a symbol existing, a symbol being exported, and a symbol having a binding. Package loading
cannot detect the gap, and neither can a structural parenthesis check.

**`:export` interns a symbol, it does not bind it.** It establishes no function binding, no value, no
class. A package whose exports name functions that no longer exist loads without complaint; the
failure appears later as an undefined-function call, typically at test-image startup, and reads like a
load-order problem rather than the deletion it actually is. When deleting or replacing a module, audit
the retained exports with `fboundp` (and `boundp` / `find-class` for the other namespaces) as an
explicit step, and search for every remaining top-level caller. Encode the audit as a test over the
package's documented callable exports so the next deletion is caught mechanically. The following wider
scan produces review candidates, not failures: exported type names and declaration identifiers can lack
all three bindings.

```lisp
(loop for sym being the external-symbols of (find-package :my-project)
      unless (or (fboundp sym) (boundp sym) (find-class sym nil))
        collect sym)
```

**Balance is not nesting correctness.** A structural checker proves the parentheses balance, not that
the nesting is what the author meant. A misplaced closing parenthesis can nest two `defun`s inside a
third, or produce `(defparameter (defparameter *table* ...))`, and the file still reads as valid Lisp.
The exported symbols then exist but are not fbound, because the definitions never became top-level
forms. Passing a structural check feels like proof, which is exactly what makes this dangerous: the
tooling's green result is used as evidence for a property it never examined. Use the full ladder and stop treating any
single rung as sufficient: balance check → top-level form outline (does each expected definition
appear at depth zero?) → `fboundp` on the expected exports → an actual system load. Structural repair
tooling should be verified at the outline and `fboundp` rungs, not just the balance rung.

## ASDF path resolution

Resolving repository-relative files (fixtures, READMEs, data, sibling test fragments) correctly under
both fresh-process ASDF loads and direct source loads. The core hazard: when ASDF loads a compiled
FASL, `*load-truename*` points into the FASL output cache, not the source tree, so `merge-pathnames`
against it resolves under the cache and fails.

**Resolve from the system, not `*load-truename*`.** Use `asdf:system-relative-pathname` or
`asdf:system-source-directory`, not `*load-truename*` / `merge-pathnames`. In a fresh test process
`*load-truename*` may even be unbound inside a test file.

```lisp
;; anchored to the system's source directory
(asdf:system-relative-pathname :my-project "tests/fixtures/data.txt")

;; fragile under FASL loads: *load-truename* points into the cache
;; (merge-pathnames "fixtures/data.txt" *load-truename*)
```

Require `:asdf` at compile/load/execute time; resolve the base directory from the system when it is
registered; fall back to `*compile-file-truename*` / `*load-truename*` / `*load-pathname*` only for
direct script/source loads that run outside ASDF. This applies to any split test loader that calls
`load` on sibling fragments.

**Initialize the source registry first.** A fresh or inherited ASDF session must have its source
registry pointed at the project root before `asdf:load-system`; loading the `.asd` file alone is not
sufficient and can stall inside `find-system`/`load-system` discovery. Treat clean
`CL_SOURCE_REGISTRY` execution as a required smoke path, run from a child process. This applies to a
launcher that owns its process: a test runner, a CLI entry point, a coverage script. It does not apply
to a bootstrap fragment that a caller loads into an already-configured image:
`asdf:initialize-source-registry` *replaces* the caller's configuration rather than extending it, so a
bootstrap that calls it silently discards whatever the caller set up. A library-side bootstrap should
add paths with `(pushnew path asdf:*central-registry* :test #'equal)` and leave the source registry
alone. Decide which of the two you are writing before choosing the call.

**Register directories, not trees.** Prefer `:directory` entries naming precise project roots over a
broad `:tree` rooted at a parent checkout. Recursive discovery traverses everything under the root,
including unrelated build outputs and, in store-backed environments, root-level result symlinks that
lead into an immutable store closure. The symptom is not an error; it is a launcher that appears to
hang, or a bootstrap that exceeds its command timeout. Enumerate the sibling project roots the build
actually needs and register each as a `:directory`. Where a checkout can be a linked worktree rather
than the primary one, detect that case and derive sibling dependency paths from the owning repository
root, since the worktree directory does not contain them.

```lisp
;; precise: name the roots, do not sweep a parent directory
(asdf:initialize-source-registry
  `(:source-registry
    (:directory ,(merge-pathnames "proj/"     workspace-root))
    (:directory ,(merge-pathnames "proj-dep/" workspace-root))
    :ignore-inherited-configuration))

;; risky: traverses build trees and store symlinks under WORKSPACE-ROOT
;; (:tree ,workspace-root)
```

If a stall survives switching to `:directory`, the traversal source may be the implementation's own
wrapping registry rather than your configuration; see ASDF plan-layer hang triage below.

## ASDF system definition pitfalls

Recurring traps when defining a library system plus its test system in a `.asd` file:

- **Conditional test-system definition.** Guarding the test-system definition with `(unless
  (asdf:find-system "proj/test" nil) ...)` makes `asdf:test-system` recurse into the same `.asd` load
  path and can surface as a circular dependency during system discovery. Define the library system and
  the test system unconditionally; let ASDF handle repeated loads/redefinitions of the `.asd` file.
- **Operation symbol package.** Qualify operations as `asdf:test-op` when forms may be read outside
  `ASDF-USER`. Ordinary `.asd` files use `ASDF-USER`, where inherited ASDF symbols already resolve.
- **Component pathnames.** Relative directory components such as `:file "src/foo"` are supported;
  use `:module` when several components share a directory, not as a workaround for a raw checkout.
- **Canonical system defined inside an alias-named `.asd`.** Defining the canonical test system inside
  an alias-named file (e.g. `proj-test.asd`), so that loading the library does not let ASDF discover
  it, triggers an ASDF warning and a fresh-registry smoke gap. Keep the canonical `proj/test` system in
  the primary `proj.asd`; let the alias-named `.asd` define only a thin compatibility alias depending
  on `proj/test`. In a fresh registry, load the alias system explicitly before asserting the canonical
  one is reachable.

```lisp
;; proj.asd: both systems defined unconditionally; module carries the pathname;
;; the operation class is qualified as asdf:test-op and runs the framework directly.
(defsystem "proj"
  :components ((:module "src" :pathname "src"
                :components ((:file "package")
                             (:file "core" :depends-on ("package"))))))

(defsystem "proj/test"
  :depends-on ("proj" "fiveam")
  :components ((:module "tests" :pathname "tests"
                :components ((:file "suite"))))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :fiveam '#:run!
               (uiop:find-symbol* '#:proj-suite :proj/test))))
```

## Dependency change surface

Swapping, removing, or externalizing a dependency is not a code change with follow-up chores. It is
one atomic edit across a fixed set of surfaces, and a partial application leaves the system unloadable:
ASDF still names components that no longer exist, so the next fresh load fails for everyone. Check:

- The `.asd` build manifest: `:depends-on` of the library system and the test system, and `:components`
  entries for any deleted files.
- Package definitions: `:import-from` clauses, `:export` lists, local-nicknames referencing the
  departing package.
- The dependency lockfile and any pinned revision, so the removed input stops being fetched.
- The development shell and source-registry configuration that made the dependency discoverable at all.
- CI runner scripts and coverage configuration that load or enumerate the affected systems.
- Call sites and test helpers, including helpers that only construct fixtures, which are easy to miss
  because they compile until the package disappears.
- README and changelog claims. A removed dependency that documentation still advertises is a claim the
  code no longer supports.

Land the whole surface list as a single review and commit unit. Deferring the manifest, the lockfile,
or the source-registry entry to a follow-up commit produces an intermediate state in which the system
cannot load, which blocks everyone who pulls between the two commits and makes bisection over that
range useless. Verify the change in a fresh registry and a fresh image, not in the session where you
made it: a warm image already has the departing package loaded and will happily resolve symbols that
no longer have a source.

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

## SBCL operations

Operational guidance for running, debugging, profiling, and shipping SBCL programs: invocation modes, ASDF
loading, the specific ways a compile or load can hang silently, subprocess/thread contracts that differ from
the textbook expectation, and coverage-measurement biases.

### Invocation modes

- **REPL** (`sbcl`, `sbcl --noinform`): interactive exploration. Reproduce a failure here first, then
  minimize the input.
- **Script** (`sbcl --script tools/task.lisp`): batch/CI execution. Design explicit exit codes; wrap
  top-level failures with `handler-case` + `sb-ext:exit`.
- **Load/eval**: one-liner load-and-run for CI or local automation:
  ```bash
  sbcl --non-interactive \
    --eval '(require :asdf)' \
    --eval '(asdf:load-system :my-app)' \
    --eval '(my-app:main)'
  ```
  Prefer `--non-interactive` in CI so a prompt cannot hang the job. Move complex startup logic into a
  `--script` file for maintainability.
- **Core control** (`sbcl --core my.core`, `--disable-debugger --non-interactive`): do not disable the
  debugger while actively investigating a single failure; reserve it for the batch harness described below.

### ASDF workflow

```lisp
(require :asdf)
(asdf:load-system :my-app)
```
Validate `load-system` success before deeper debugging, and read the *first* ASDF failure carefully:
chasing secondary errors wastes time. For tests:
```bash
sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:test-system :my-app/test)'
```
Prefer Qlot for dependency reproducibility (`qlot install`, then `qlot exec sbcl ...`) to cut local-vs-CI
drift.

### Debugging workflow

Reproduce → observe → hypothesize → fix and verify:

1. **Reproduce**: fix the execution mode first, then strip inputs/environment to a minimal failing case.
2. **Observe**: inspect the debugger backtrace and stack frames, `describe`/`inspect` the problem objects,
   and `trace` the call path:
   ```lisp
   (trace my-app::parse-input)
   (untrace my-app::parse-input)
   (describe some-object)
   (inspect some-object)
   ```
3. **Hypothesize**: define an observable signal per hypothesis and use step/break/log checks to prove or
   reject it one at a time.
4. **Fix and verify**: re-run the *same* reproduction command after the fix, and add a test that preserves
   the failure case.

Use `restart-case` to keep diagnosing while preserving continuity, instead of a bare `handler-case` that
swallows the condition:
```lisp
(restart-case
    (dangerous-op x)
  (use-default () :report "fallback value" 0)
  (retry () :report "retry operation" (dangerous-op x)))
```

### Compile/load hang triage

A distinct failure class: SBCL stops making progress (no error, no backtrace, no output) inside
`compile-file`, `load`, or `asdf:load-system`, rather than signalling. These are compile-unit and load-order
phenomena, not ordinary runtime bugs: the same forms often compile and load fine in isolation but stall once
combined in one file or one image. Diagnose structurally; prefer decomposition over per-form workarounds.

**Shrink the compile unit.** Treat the compile unit (the file handed to `compile-file`, or a single ASDF
component) as the primary variable. Splitting a stalling file into smaller, serially-loaded files is the
durable fix; per-form workarounds are stopgaps. Many stalls come from compile-time interaction between
top-level forms in the same unit (macro-generation feeding a later macro invocation, large constant folding,
definition ordering), not from any single form, so reducing the unit removes the interaction.

**Keep top-level forms boring.** Define top-level helpers with plain `defun` rather than a top-level
`(setf (symbol-function 'name) (lambda ...))` or an eager `(compile nil (lambda ...))` at registration time.
Keep constant-heavy work inside runtime helper functions instead of thin top-level wrappers that invite
constant folding of large literals. Both patterns have been observed to trigger compile/eval stalls on SBCL
2.6.0 (macOS/Nix) where the equivalent plain `defun`, or a non-constant construction path, loads normally:
treat the specific triggers as version-scoped, the general "keep top-level forms simple and side-effect-light"
guidance as dialect-stable.
```lisp
;; risky at top level: symbol-function assignment of a full lambda body
(setf (symbol-function '%encode) (lambda (s) #| large body |#))
;; safe: plain defun
(defun %encode (s) #| large body |#)

;; risky: a thin wrapper that folds a large constant vector at compile time
(defun tokens () #(#| hundreds of literal specs |#))
;; safe: build the vector at runtime through a non-constant argument path
(defun tokens (specs) (build-token-vector specs))

;; drop unneeded generated copiers that enlarge a defstruct-heavy compile unit
(defstruct (node (:copier nil)) a b c)
```

**Watch macro expansion size.** A macro whose expansion grows combinatorially with its arguments can make
`macroexpand`/compile appear hung. Observed case: a keyword-wrapper macro emitting one direct-call branch per
`&key`-presence subset produced on the order of 2^N branches for N keys: a wrapper with ~18 keys generated
hundreds of thousands of branches. The load stall was macroexpansion blow-up, not the wrapped function. Emit a
linear runtime construction instead, and add a macroexpansion-size regression test for high-arity call sites.

**Load order is a variable.** A file that compiles alone can stall when compiled after another file has been
loaded into the same image. Observed with definition-heavy files (many `defstruct` forms) that compiled fine
in a fresh image but stalled once an earlier file had been loaded first, evidence the trigger is cross-unit
state, not the file's own source. When a stall appears only in-sequence, re-verify each unit in a fresh image.

Other version-scoped observed triggers (SBCL 2.6.0), to test as hypotheses rather than trust as guarantees:
- A run of many top-level `defstruct` forms in one compile unit; one more struct crosses a threshold and
  `compile-file` stalls. Mitigate by splitting structs across serially-loaded files and adding `(:copier nil)`.
- Predicates branching on implementation Unicode category/width tables via `member`/`case` under a
  bootstrap-loaded image: bind the return value and compare with explicit `eq`/`or` checks instead.
- Forcing `sb-ext:*evaluator-mode*` to `:interpret` across a whole file to dodge a compile stall frequently
  just relocates the stall to a later file or to execution time, a diagnostic, not a fix, unless paired with
  structural decomposition and fresh verification.
- A large `defun` whose small helper is a candidate for open-coding: adding `(declaim (notinline %helper))`
  has let a runner load past the stalling boundary: this localizes the cause (inlining) but shrinking the
  compile unit is still the durable answer.
- Unresolved forward references in a definition-heavy file: a top-level
  `(declaim (ftype function ...))` for the remaining forward references has cleared a load stall that no
  per-form change reached.

**In-image timeouts do not guard this.** `sb-ext:with-timeout` does not reliably interrupt the compiler, so an
in-image timeout is not a valid guard against a compile or load stall: the timeout simply never fires and the
session hangs exactly as it would have without it. This is why the subprocess harness below is mandatory, not
merely convenient: a stall has to be bounded from outside the image by a process-level timeout with a kill
grace, because the process being bounded may be in a state where nothing inside it can run.

### ASDF plan-layer hang triage

The sibling failure family: a stall inside `asdf:load-system` that never reaches your code at all. ASDF's
operation/plan layer (system-definition discovery, source-registry flattening, plan computation) runs
*before* the first form of the target system is compiled, and it can hang there. Rule the environment out
before spending time bisecting project sources, since every technique above assumes the stall is in a compile
unit you own.

**Use `(asdf:load-system "asdf")` as a control.** ASDF registers itself as a system, so loading it exercises
the same find-system/operate machinery with none of your project's code in it. If `(require :asdf)` succeeds
but `(asdf:load-system "asdf")` never returns, the fault is environmental and no amount of file-level
bisection will find it. Probe layers cheapest-first, each in a fresh timeout-bounded child process, and stop
at the first one that hangs:
`(require :asdf)` → `(asdf:load-system "asdf")` → `(asdf:find-system "proj" nil)` →
`(asdf:load-asd #p"/abs/path/proj.asd")` → `(asdf:operate 'asdf:load-op "proj")`.
A plain `(load "src/file.lisp")` that returns promptly while `find-system` hangs is direct evidence the stall
is in discovery, not the source. Observed on Darwin/Nix with ASDF 3.3.7: `require` returned, then
`load-system`, `load-asd`, `find-system`, and `operate` all hung after system-definition discovery, while a
direct `load` registered the same system immediately: without the control experiment this reads as "our
project hangs on load."

**`:ignore-inherited-configuration` does not disable the wrapper.** It suppresses inherited user and system
source-registry configuration, but not the implementation's *wrapping* source registry. SBCL's wrapping
configuration recursively registers the implementation directory, so a blocked descriptor somewhere under the
SBCL contrib tree can stall registry flattening even when your own configuration is fully explicit. When
registry flattening is the suspect, inspect the stalled process's open descriptors (`lsof`/`fs_usage` on
Darwin, `/proc/PID/fd` on Linux) rather than re-reading your configuration: a descriptor pinned inside the
implementation's own contrib directory confirms the wrapper, not your project, is the traversal source.
Narrowing a `:directory` instead of a `:tree` does not help either, since the wrapper is added independently
of your entries. (Mechanism observed with ASDF 3.3.7 on a store-backed SBCL; confirm by descriptor inspection
rather than treating it as universal.)

**Interrupt-disabled regions need SIGKILL.** A stall can sit inside a Lisp interrupt-disabled region, where
SIGALRM and SIGTERM are deferred indefinitely. An in-image timeout, a handler-based deadline, and a TERM-only
external watchdog all fail silently against it: the deadline "fires" and nothing happens. Every watchdog over
an ASDF load must escalate to SIGKILL after a grace period, and must report which signal actually ended the
child; a process that survived TERM and needed KILL is itself evidence about where it was stuck.

### Headless verification harness

A sound, non-interactive harness is a prerequisite for diagnosing the stalls above: if the timeout mechanism
is unsound, a stalled form and a stalled harness are indistinguishable, producing false positives.

**Real subprocess timeout.** The timeout must run in a parent process that keeps the ability to kill the
child. Use an established timeout utility with a kill grace, as below, rather than an unverified fork/alarm
wrapper. Verify normal completion, signal termination, and timeout exit statuses against known controls.
Before group signalling, establish that the group belongs to this run; never infer ownership from a PID alone.

**Kill the process group, not just the wrapper's PID.** A child that has called `setpgid`/`setpgrp` is
orphaned (not reaped) if only the parent is killed, and keeps holding resources. Put the child in its own
group and send TERM/KILL to the group, or let the wrapper live to its deadline and reap the child.

**Deterministic child flags.** Launch every verification child with a fixed, minimal, non-interactive flag set:
- `--disable-debugger`: never enter the interactive debugger in automation. This does not contradict the
  root-cause rule above: disable it in the batch harness, keep it enabled while investigating a single failure
  interactively.
- `--no-sysinit --no-userinit`: ignore site/user init files so the child does not inherit local state.
- Exit with a fully qualified `(sb-ext:exit ...)`/`(sb-ext:quit)`; an unqualified `(quit)` can become unsafe
  after package changes during ASDF loading.
```bash
sbcl --no-sysinit --no-userinit --disable-debugger \
     --eval '(require :asdf)' \
     --load run-one-unit.lisp \
     --eval '(sb-ext:exit :code 0)'
```

**Fresh process per unit.** Use isolated processes to diagnose cross-unit state or load-order failures.
Passing isolated units does not establish that the whole-suite execution contract works; preserve and rerun
the failing combined case. Keep diagnostic isolation complete, including the bootstrap compilation step.

**Isolate the FASL cache.** Give each run a private, initialized output-translations/cache root before
`asdf:load-system`, for the race described in ASDF parallel execution above. Initialize output translations in
the launcher itself with a run-owned directory inside the authorized worktree; do not repurpose `HOME` or clear
another run's cache.

**Bound timeout with a kill grace.** When using coreutils `timeout(1)`, always pass a kill grace:
`timeout -k 10s <limit>s <command>`. Do not use `--foreground` for noninteractive test jobs:
[GNU Coreutils](https://www.gnu.org/s/coreutils/manual/html_node/timeout-invocation.html) documents that its
children are not timed out in that mode. Descendants that escape the managed process group still need
separate, ownership-checked cleanup. Plain `timeout` sends only TERM, and SBCL can remain alive
after its initial termination signal, so a nominally bounded run leaks past the job budget and the escaped
child keeps holding the FASL cache and any ports it opened, same root cause as the interrupt-disabled-region
issue above: the first signal is a request, not a guarantee. Set the grace long enough for an orderly exit
(a few seconds is usually ample) but budget the outer CI step timeout against `limit + grace`, not `limit`.
```bash
# bounded: TERM at the limit, KILL 10s later if the child is still alive
timeout -k 10s 300s \
  sbcl --no-sysinit --no-userinit --disable-debugger --script run-tests.lisp
```

**Timeout threshold vs. contention.** Distinguish a genuine per-file stall from ambient machine contention.
When many SBCL sessions run concurrently, baseline load latency can exceed a low per-file timeout and report
every file as a timeout. Reproduce alone and measure an unchanged control before attributing blame to a
single file. Do not raise an acceptance timeout merely to make the check pass; document a demonstrated
harness defect before changing its limit.

### Form bisect and package preflight

**Bisect by form, not by line.** When narrowing which top-level form stalls compile/load, slice by complete
top-level forms, never by raw line ranges: a line-range slice can cut through the middle of a form and
produce malformed Lisp that fails to read, masquerading as the original stall (e.g. `INPUT-ERROR-IN-LOAD`).
Use a read/eval form-trace: read one top-level form at a time, log its head before evaluating and log
completion after, and stop on the first form that logs a head but never completes.
```lisp
;; streaming form-trace: reader sees each in-package before it reads the next form,
;; and the last "head:" without a matching "done:" names the stalling form.
(with-open-file (in path)
  (loop for form = (read in nil :eof)
        until (eq form :eof)
        for head = (and (consp form) (car form))
        do (format *error-output* "~&head: ~S~%" head)
           (finish-output *error-output*)
           (eval form)
           (format *error-output* "~&done: ~S~%" head)))
```

**Reader intern timing and package preflight.** The reader interns every symbol in the current package at
read time, before an `in-package` in the same batch takes effect. Reading a whole file (or a whole `--eval`)
into a list of forms first, then evaluating, interns later symbols in the wrong package and can make
package-local functions look undefined, a false failure unrelated to the code under test. Keep package
creation, package switch, and definitions as separate top-level evaluations (or stream forms so the reader
sees `in-package` before it reads later forms). When a child process receives a test/symbol name via
environment variable or argument, read or resolve it in the target package, not in `CL-USER`; otherwise it
interns into `COMMON-LISP-USER` and dispatch can miss or hang at the boundary.

**Minimal reproducer hygiene.** Before trusting a "hang", rule out defects in the reproducer itself: an
unbalanced paren in a probe loader can leave a form open so later `defun`s never become top-level, and a
package-mismatched read can fake a missing-symbol error. A malformed harness produces false hangs.

### Subprocess process-group contract

What `sb-ext:run-program` actually guarantees about the child's process group, and why "I can kill the whole
subprocess tree" is silently false for one specific input mode. This matters for any library that spawns a
pipeline and promises cancellation or cleanup: the promise holds for most call sites and breaks for one, so it
passes casual testing.

**Inherited stdin suppresses the child process group.** `run-program` only puts the child in its own process
group when the child's input descriptor is a real (nonnegative) descriptor. With `:input t` (inherited
stdin), SBCL prepares the descriptor as -1 and the forked child calls `tcsetpgrp` instead of creating a new
group, so the child stays in the caller's process group. Every other supported input mode (`nil`, a stream, a
pathname, `:stream`) takes the nonnegative path and does create the group: `setpgid(0, getpid())` on Darwin,
`setpgrp()` on Linux. The dangerous half is not that the group is missing, but that a later kill of "the
child's group" then targets the caller's own group: a cancellation routine written against the common case
will signal the Lisp process itself the first time someone passes `:input t`. Do not infer the process group
from the spawn arguments: verify it after spawn (compare `sb-posix:getpgid` of the child pid against the pid
itself), store the verified pgid in an opaque handle, and route public signal APIs through that handle rather
than a caller-supplied pid. If verification fails, degrade to single-process signalling and say so in the
handle rather than pretending group cancellation is available. (Descriptor/syscall details observed on POSIX
SBCL 2.6.x; the verify-then-record remedy is portable regardless of how a given release wires the modes.)

**A saved pgid expires with its leader.** A saved pgid is only authorization to signal while the group leader
is alive. Once the leader has been reaped, the kernel is free to reuse that pid and pgid, so a later
`kill(-pgid, signal)` can land on an unrelated process group. Public group-signal entry points must reject a
handle whose leader has already reached a terminal state, rather than "cleaning up anyway": best-effort
cleanup paths that fire after reaping are exactly where reuse bites, so gate them on the same check.

**Distinguish ESRCH from EPERM.** Cleanup code must distinguish the two failure modes of a group signal:
ESRCH means no such group: the target is genuinely gone and cleanup succeeded; EPERM means the group exists
but is not signalable by this process: the target is still running and cleanup failed. Collapsing both into
"kill failed, ignore" silently converts a leaked process tree into a clean shutdown report. Return ESRCH as
success from a reaper, and escalate EPERM as a real error carrying the pgid. This is POSIX-general and applies
equally to a shell wrapper checking `kill`'s exit status.

### Threading contracts

In-process concurrency contracts that differ from the textbook expectation, plus the lock discipline that
keeps a worker pool from deadlocking on its own error path.

**`condition-wait` with `:timeout` may return without the mutex.** `sb-thread:condition-wait` with `:timeout`
may return without having reacquired the mutex, when reacquisition itself cannot complete before the deadline
expires. This violates the usual condition-variable contract (that the wait always returns holding the
lock) and the damage surfaces later: exiting the surrounding `sb-thread:with-mutex` signals a mutex ownership
error at a frame that has nothing to do with the timeout. Nobody reads "not the owner of the mutex" at a
`with-mutex` exit as "a `condition-wait` timeout three lines up returned early", so the investigation starts in
the wrong place. Do not use `:timeout` to implement blocking semantics. Implement a blocking operation as a
timeout-free predicate loop (wait, re-test the predicate, wait again) and make every state change that can
satisfy the predicate signal the condition variable explicitly, including the non-obvious ones: a dispatcher
freeing capacity must wake blocked producers, and a cancellation that changes the predicate must wake them
too, or the loop sleeps through the event it was waiting for.
```lisp
;; blocking enqueue without :timeout; the predicate loop is the contract
(sb-thread:with-mutex (lock)
  (loop until (or cancelled (< count capacity))
        do (sb-thread:condition-wait space-available lock))
  (unless cancelled (push item queue) (incf count)))

;; every predicate-changing site must wake the waiters, including cancellation
(sb-thread:with-mutex (lock)
  (setf cancelled t)
  (sb-thread:condition-broadcast space-available))
```

**Never call a user callback under the state lock.** Update the shared state while holding its mutex, release
the mutex, and only then invoke the user callback; if the callback's failure must be recorded, reacquire the
mutex after it unwinds. Invoking a callback under the state lock hands arbitrary user code the power to block
all state synchronization, and (the failure people actually hit) deadlocks on a recursive lock attempt when
the callback signals and the handler tries to record the condition in the same state. The deadlock arrives
through the error-recording path, not the happy path: every test with a well-behaved callback passes, and the
first callback that signals hangs the pool: that asymmetry is why this survives review. Apply the same rule
to any outward call from under a lock: joining a dispatcher thread, calling a logging hook, signalling a
condition whose handler is user-supplied. The invariant is "no lock is held across a call whose implementation
the module does not own."
```lisp
;; state mutation under the lock; the callback strictly outside it
(let ((snapshot nil))
  (sb-thread:with-mutex (task-lock)
    (setf (task-state task) :finished)
    (setf snapshot (task-result task)))
  (handler-case (funcall (task-callback task) snapshot)
    (error (c)
      (sb-thread:with-mutex (task-lock)
        (setf (task-callback-error task) c)))))
```

### Coverage measurement bias

**sb-cover under-attributes definition-heavy files.** `sb-cover` reports low expression coverage for files
dominated by top-level defining forms and metadata side effects (`defpackage`, `define-condition`, top-level
documentation/table assignments), even when the runtime contracts they establish are fully tested: these
forms are counted as expressions but are not all attributed as executed by ordinary test runs. Separate
genuine runtime gaps from instrumentation bias by comparing a low-coverage file against its shape:
definition-heavy files may warrant a few explicit contract tests but need not reach 100%; logic-heavy files
are the higher-value target for additional tests or refactoring. Do not distort public API design solely to
satisfy sb-cover on top-level metadata; prefer explicit tests plus a documented exception. sb-cover does not
clean its own HTML output directory, so clear the stale report after splitting or renaming source files before
reading a new one.

**Coverage instrumentation is process-global.** SB-COVER counters live in process-global mutable state.
Running the suite across concurrent workers in one image produces nondeterministic per-file undercounts even
while every test still passes, so the coverage number moves run to run for reasons unrelated to the tests.
Run coverage single-worker even when the ordinary suite runs in parallel: treat it as a distinct execution
mode with its own runner settings, not the normal run with a flag added.

**Load instrumented sources through the build system.** After resetting SB-COVER, load the system under
measurement through `(asdf:load-system :proj :force t)`. Manually compiling and loading copied sources
detaches the counters from the source identity SB-COVER reports against, and the affected files come back as
a confident 0% instead of an error, which reads as "untested" and sends people to write tests for code that
is already covered. The distinguishing symptom is that the 0% files are exactly the ones the runner handled
specially: a copy step, a staging directory, a hand-rolled compile loop.

**Gate coverage against a source manifest.** An aggregate percentage is computed over the files that appear
in the report, so it says nothing about files that never made it in: a report showing 100% across nine files
when the system has twelve is still 100%. The gate must compare normalized report source filenames against a
declared manifest of production components and reject the run when a row is missing, malformed, or has a zero
total, before it accepts the percentage at all. Derive the manifest from the ASDF component list rather than a
hand-maintained second list, so a newly added component is covered by the gate on the commit that adds it.
Normalize both sides (truename, case, store-path prefixes) before comparing, or the check fails open on path
formatting alone. This manifest rule is language-neutral and applies to any coverage or lint report consumed
as a gate; the SB-COVER specifics above are what make it easy to lose rows here.

### Performance profiling

```lisp
;; start here before reaching for a profiler
(time (my-app:run-once input))

;; deterministic, call-site granularity
(require :sb-profile)
(sb-profile:profile my-app::hot-fn my-app::other-hot-fn)
(my-app:run-benchmark)
(sb-profile:report)
(sb-profile:unprofile)

;; statistical, lower overhead, broad trends
(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 3000 :report :flat)
  (my-app:run-benchmark))

;; apply optimization declarations locally, and verify impact; avoid safety 0
;; without hard evidence and strong tests
(defun hot (x y)
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (type fixnum x y))
  (+ x y))
```
These are tool invocations: how to obtain a number from SBCL. They do not tell you whether the number means
anything. Paired A/B protocols, warmup and full-GC discipline, measuring the noise floor before claiming a
delta, gating on a confidence interval rather than a point estimate, and proving you are measuring your
working tree rather than a pre-registered store build all belong to
[performance-benchmarking](../performance-benchmarking/SKILL.md): consult it before reporting any
before/after comparison.

### Build and release

```lisp
(defun main ()
  (handler-case
      (progn
        (my-app:run)
        (sb-ext:exit :code 0))
    (error (e)
      (format *error-output* "fatal: ~a~%" e)
      (sb-ext:exit :code 1))))

(sb-ext:save-lisp-and-die "my-app"
  :toplevel #'main
  :executable t
  :compression t)
```
Always define explicit process exit codes, and validate ASDF load and tests before generating the image.

### Ecosystem integration

- **SLY**: prefer SLY over SLIME in this environment, assume sly/sly-asdf/sly-macrostep workflows for Emacs
  integration, and give SLY-compatible guidance when explaining editor actions.
- **Nix**: `nix shell nixpkgs#sbcl` for reproducible execution; pin project environments via `shell.nix` or
  `flake.nix`, and combine with Qlot for stronger dependency reproducibility.
- **Roswell**: `ros install sbcl`, `ros run`, `ros build app.ros` for implementation management and script
  execution.

## Related

- [trust-boundaries](../trust-boundaries/SKILL.md): general input-validation rules (limits before
  allocation, validate before normalize) that the FORMAT-injection and pathname-coercion traps above
  instantiate in Common Lisp.
- [serena-usage](../serena-usage/SKILL.md): navigating CLOS hierarchies, generic functions, and symbol
  definitions.
- [investigation-patterns](../investigation-patterns/SKILL.md): debugging condition handling, macro
  expansion, and SBCL-specific issues; the evidence-driven methodology behind the SBCL debugging workflow.
- [nix-ecosystem](../nix-ecosystem/SKILL.md): pinned SBCL runtime environments with nix shell/flake.
- [performance-benchmarking](../performance-benchmarking/SKILL.md): benchmark methodology (paired protocols,
  noise floor, interval-based gating) behind the SBCL profiling tools.
- [test-integrity](../test-integrity/SKILL.md): false-green testing: suites that report success without
  exercising the contract.
