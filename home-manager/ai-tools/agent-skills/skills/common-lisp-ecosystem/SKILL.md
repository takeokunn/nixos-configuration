---
name: common-lisp-ecosystem
description: Use for Common Lisp, SBCL, or Coalton, covering CLOS, ASDF, defpackage and defsystem. Also covers package hygiene, condition design, format-string injection, Unicode predicates, reader macros, macro-argument evaluation hygiene, atomic file publishing, and hash-table key safety.
version: 3.0.0
---

Silent failure modes in Common Lisp, CLOS, ASDF, and package systems: cases where correct-looking
code loads and runs without error but does the wrong thing. Ordinary CLOS, condition, and ASDF usage
is assumed knowledge; this file exists for the traps that pass a naive review.

## CLOS: the construction boundary is the class, not the constructor

Exporting a class exports `make-instance` on it. Any validation or invariant that lives only in a
convenience constructor like `make-foo` is advisory: a caller who writes `(make-instance 'foo :slot
...)` bypasses all of it and gets an object the rest of the system assumes cannot exist. The
convenience constructor reads like the API, so reviewers check it and stop; nothing in `make-foo`
hints that a second, unvalidated construction path is exported alongside it.

Pick one of three, and state which in the class documentation: enforce invariants in an
`initialize-instance :after` (or `shared-initialize`) method so every path runs them; keep the class
package-internal and export only the constructor; or, when the class must be exported as-is, give
every optional slot a bound `:initform` so direct `make-instance` is as safe as convenience
construction. A privileged fast path must never be expressible as an initarg; carry it in
package-internal dynamic state or a private constructor, or `make-instance` becomes a way to request
the unvalidated path by name.

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
concrete reason behind the fresh-process-per-unit rule in
[sbcl-usage](../sbcl-usage/SKILL.md).

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
package's external symbols so the next deletion is caught mechanically:

```lisp
;; audit every external symbol of a package for a live function binding
(loop for sym being the external-symbols of (find-package :my-project)
      unless (or (fboundp sym) (boundp sym) (find-class sym nil))
        collect sym)
;; a non-empty result means the package promises names it cannot deliver
```

**Balance is not nesting correctness.** A structural checker proves the parentheses balance, not that
the nesting is what the author meant. A misplaced closing parenthesis can nest two `defun`s inside a
third, or produce `(defparameter (defparameter *table* ...))`, and the file still reads as valid Lisp.
The exported symbols then exist but are not fbound, because the definitions never became top-level
forms. Passing a structural check feels like proof, which is exactly what makes this dangerous: the
tooling's green result is used as evidence for a property it never examined. Two independent
occurrences of this shape were observed in unrelated files. Use the full ladder and stop treating any
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
;; robust: anchored to the system's source directory
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
wrapping registry rather than your configuration; see the ASDF plan-layer triage in
[sbcl-usage](../sbcl-usage/SKILL.md).

## ASDF system definition pitfalls

Recurring traps when defining a library system plus its test system in a `.asd` file:

- **Conditional test-system definition.** Guarding the test-system definition with `(unless
  (asdf:find-system "proj/test" nil) ...)` makes `asdf:test-system` recurse into the same `.asd` load
  path and can surface as a circular dependency during system discovery. Define the library system and
  the test system unconditionally; let ASDF handle repeated loads/redefinitions of the `.asd` file.
- **Bare operation symbol in `:perform`.** Writing `:perform (test-op ...)` or `:in-order-to` with a
  bare `test-op` resolves to `COMMON-LISP-USER::TEST-OP`, not the ASDF operation class, and fails with
  class-not-found at run time. Qualify the operation as `asdf:test-op` in `:perform`, and prefer an
  explicit `(asdf:test-system ...)` call in the `:perform` body over a chained `:in-order-to` graph,
  which is easier to isolate and less likely to stall the compile/load plan.
- **Relative file pathnames in a raw checkout.** `:file "src/..."` / `:file "t/..."` relative
  component paths can raise "Invalid relative pathname" in a raw checkout. Group components under
  `(:module "src" :pathname "src" :components (...))` so the module carries the pathname, rather than
  embedding directory segments in each `:file`.
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
ASDF still names components that no longer exist, so the next fresh load fails for everyone. Three
unrelated codebases independently produced the same surface list, which is why it is worth carrying as
a checklist:

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
launcher, before `load-system`, to a private per-user cache, and keep that initialization in the
packaged launcher (not only in ad hoc scripts) so every subcommand inherits it:

```lisp
(asdf:initialize-output-translations
  '(:output-translations
    (t (:home ".cache" "common-lisp" :implementation))
    :ignore-inherited-configuration))
```

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

The counterpart to "shrink the compile unit" advice in [sbcl-usage](../sbcl-usage/SKILL.md). Splitting
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
form, the file wants one cohesive data fragment plus a thin loader rather than N fragments. An observed
four-way split of a registry file proved brittle at every boundary and was collapsed back. The signal
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

**Retry only on a confirmed collision.** Open the temporary exclusively with `:if-exists nil`, and
retry only when the resulting `file-error` is confirmed to be a name collision by `probe-file`. Every
other open failure must escape immediately. Bound the loop with an explicit attempt count and treat
exhaustion as a structured operation failure rather than an infinite retry. Without the `probe-file`
confirmation, a permission error or a missing parent directory is retried the full attempt count and
then reported as "could not find a free temporary name", which points the investigation at name
generation instead of at permissions.

```lisp
;; exclusive create; NIL means the name was taken
(let ((stream (open candidate :direction :output :if-exists nil)))
  (cond (stream stream)
        ((probe-file candidate) :retry)      ; genuine collision
        (t (error 'temp-file-open-failure :path candidate))))
```

## Numeric frontend correctness

Rules for writing or testing a numeric front end (a parser, a serializer, a converter). Both exist
because the obvious reference point is the host implementation, and the host implementation is not a
specification.

**The host reader is not a floating-point oracle.** Do not validate a float parser by requiring
identity with the implementation's own reader. A reader can be off by one unit in the last place on
subnormals and other hard cases, so a differential test using it as the oracle reports failures where
the implementation under test is the more accurate of the two. Use an exact rational-to-binary64
computation (or libc `strtod`) as the reference. Measured case: of 90,041 inputs where both sides
produced double-floats, 1,118 differed; every difference was an adjacent subnormal one unit apart, and
exact rational distance favored the direct parser in all 1,118. Reading that run as 1,118 bugs would
have meant "fixing" the correct implementation to reproduce the reader's error. Build the oracle from
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
memoized signature) alongside the data it is derived from. All three failed in observed code without
raising a single error; the system simply computed against a stale view.

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

Never evaluate a caller-supplied argument form more than once, and never reorder the left-to-right
evaluation of caller-supplied forms: a macro that evaluates `(incf counter)` twice, or evaluates argument B
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

## Related

- [trust-boundaries](../trust-boundaries/SKILL.md): general input-validation rules (limits before
  allocation, validate before normalize) that the FORMAT-injection and pathname-coercion traps above
  instantiate in Common Lisp.
- [sbcl-usage](../sbcl-usage/SKILL.md): operational SBCL execution, debugger, profiling, executable
  builds, fresh-process-per-unit rationale, and the compile-unit-shrinking advice this file's
  file-decomposition section complements.
- [serena-usage](../serena-usage/SKILL.md): navigating CLOS hierarchies, generic functions, and symbol
  definitions.
- [context7-usage](../context7-usage/SKILL.md): fetching current ASDF, SBCL, and Common Lisp library
  documentation.
- [investigation-patterns](../investigation-patterns/SKILL.md): debugging condition handling, macro
  expansion, and SBCL-specific issues.
