---
name: common-lisp-ecosystem
description: Use for Common Lisp, SBCL, or Coalton, covering CLOS, ASDF, package hygiene, condition design, format-string injection, Unicode predicates, reader macros, macro hygiene, atomic file publishing, and hash-table key safety. Also SBCL execution and debugging (--script, REPL/SLY, backtraces, save-lisp-and-die, profiling, compile/load hangs, terminating a stuck SBCL, sb-thread/sb-cover hazards).
metadata:
  version: "4.1.0"
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

**Load the workspace, not a published build.** A development environment can pre-register a store copy of the
system under test, and a benchmark or test then compiles that copy while the edits sit unread. Register the
workspace root as a `:directory` entry (Register directories, not trees), then run
`(asdf:clear-system "proj")`, `(asdf:load-asd #p"/abs/workspace/proj.asd")`, and
`(asdf:load-system "proj" :force t)`, and assert that `(asdf:system-source-file "proj")` and the compiled
output paths lie inside the workspace.

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
             (unless (uiop:symbol-call :fiveam '#:run!
                       (uiop:find-symbol* '#:proj-suite :proj/test))
               (error "Test suite failed."))))
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

## Detailed references

- [ASDF and source reliability](references/asdf-and-source-reliability.md) covers system loading, source layout, dependencies, data integrity, output, tests, and macro hygiene.
- [SBCL operations](references/sbcl-operations.md) covers execution, debugging, process management, threading, coverage, profiling, and release.

## Related

- [trust-boundaries](../trust-boundaries/SKILL.md): general input-validation rules (limits before
  allocation, validate before normalize) that the FORMAT-injection and pathname-coercion traps above
  instantiate in Common Lisp.
- [serena-usage](../serena-usage/SKILL.md): navigating CLOS hierarchies, generic functions, and symbol
  definitions.
- [investigation-patterns](../investigation-patterns/SKILL.md): debugging condition handling, macro
  expansion, and SBCL-specific issues.
- [nix-ecosystem](../nix-ecosystem/SKILL.md): pinned SBCL runtime environments with nix shell/flake.
- [performance-benchmarking](../performance-benchmarking/SKILL.md): benchmark methodology (paired protocols,
  noise floor, interval-based gating) behind the SBCL profiling tools.
- [test-integrity](../test-integrity/SKILL.md): false-green testing: suites that report success without
  exercising the contract.
