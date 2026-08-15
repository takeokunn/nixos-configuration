---
name: lisp-macro
description: Use when writing a macro, defmacro, DSL, hygienic macro, code walker, CPS transform, anaphoric macro, once-only, g!-symbol, or pandoric macro in Common Lisp or Emacs Lisp — including auditing a source-to-source rewriter. Defer to common-lisp-ecosystem / emacs-ecosystem for language basics.
version: 3.0.0
---

Dialect-agnostic macro-writing skill combining two layers: (1) the canonical technique catalog for
individually "correct" macros from Paul Graham's On Lisp and Doug Hoyte's Let Over Lambda — once-only,
anaphora, auto-gensym, generalized variables, CPS macros, macro-defining-macros, duality of syntax, pandoric
macros — and (2) an engineering discipline for turning those techniques into multi-clause DSLs safely: phase
separation, hygiene, evaluation-order preservation, compile-time diagnostics, and a parser/analyzer/emitter
pipeline behind a thin defmacro. Reach for (1) when writing or reviewing any single macro; reach for (2) when
the macro is a DSL with several clause forms or non-trivial static analysis. CLOS/ASDF/condition-system
fundamentals, SBCL runtime operations, and Emacs package/LSP integration are out of scope — see
[common-lisp-ecosystem](../common-lisp-ecosystem/SKILL.md), [sbcl-usage](../sbcl-usage/SKILL.md),
[emacs-ecosystem](../emacs-ecosystem/SKILL.md). Reader macros (`set-macro-character`) are covered only as a
scoped, opt-in technique, not a default tool.

## Design order

Draft the DSL's user-facing input S-expression first, optimizing for the lowest cognitive load — the user
should never think about registers, continuations, or environment plumbing. Then hand-write the ideal,
fully-expanded, runtime-optimal S-expression that input should produce: this is the executable proof of
efficiency the implementation is judged against. Only then implement `parse` (raw form → validated AST,
signaling a compile-time error on malformed input), `analyze` (AST → annotated AST via static analysis —
dependency graphs, liveness, branch enumeration — with no code emitted yet), and `emit` (annotated AST → the
Phase-1 S-expression) as independently testable, pure functions. Expose them through the thinnest possible
`defmacro`. Verify by running `macroexpand-1`/`macroexpand` (CL) or `macroexpand-1`/`pp-macroexpand-last-sexp`
(Elisp) against the example input and diffing against the Phase-1 ideal, then check: does any argument form
appear more than once in the expansion, is every internal symbol gensym'd, does a malformed clause raise a
named compile-time error?

## Core laws

**No runtime resolution.** Resolve everything resolvable at macro-expansion time — clause structure,
dependency graphs, lifetimes, continuation chains — never `eval`. Every fact knowable from the S-expression
shape alone is a fact the runtime should never recompute; deferring it to `eval` pays a performance and safety
tax on every execution instead of once at compile time. Build a static graph (plist/struct/alist) in the
analyzer stage and answer questions like "is this register still live here?" by walking that graph, never by
generating code that asks at runtime:

```lisp
;; Analyzer answers "is REGISTER still live after this instruction?" once, by walking
;; the AST and recording last-use positions — emitted code never asks this again.
(defun vm--analyze-liveness (instructions)
  (let ((last-use (make-hash-table)))
    (loop for instr in instructions
          for pos from 0
          do (dolist (reg (vm--instr-reads instr))
               (setf (gethash reg last-use) pos)))
    last-use))
```

**Strict phase separation.** Compile-time helper functions (parser, analyzer, emitter) must exist in the
compile-time environment. Without this, cross-compilation, fasl-only loads, or a fresh REPL load order in CL
signals "undefined function" during macroexpansion; in Elisp, byte-compiling a file that uses a macro from
another file silently falls back to a runtime function call and loses expansion-time errors.

```lisp
;; Common Lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun dsl--parse-clause (clause) ...))

;; Emacs Lisp (requires -*- lexical-binding: t; -*-)
(eval-and-compile
  (defun dsl--parse-clause (clause) ...))
```

**Evaluation order and single evaluation.** Never evaluate a user-supplied argument form more than once, and
never reorder the left-to-right evaluation of user-supplied forms. A macro that evaluates `(incf counter)`
twice, or evaluates argument B before argument A, silently breaks any caller relying on ordinary function-call
semantics — the single most common macro-hygiene bug. Bind every argument form exactly once via gensym'd
let-bindings in the order they appear (once-only), then reference only the bound symbols:

```lisp
;; Common Lisp (alexandria:once-only handles this idiom directly — prefer it over hand-rolling)
(defmacro my-max2 (a b)
  (alexandria:once-only (a b)
    `(if (> ,a ,b) ,a ,b)))

;; Emacs Lisp (manual once-only, since cl-lib has no equivalent)
(defmacro my-max2 (a b)
  (let ((ga (make-symbol "a")) (gb (make-symbol "b")))
    `(let ((,ga ,a) (,gb ,b))
       (if (> ,ga ,gb) ,ga ,gb))))
```

**Compile-time diagnostics.** Reject malformed DSL input during macro-expansion with an actionable error, not
at runtime. DSL users write S-expressions, not English; the parser is their only source of feedback, and a
runtime error three call frames deep costs far more debugging time than an expansion-time error naming the
offending clause:

```lisp
(defun dsl--parse-clause (clause)
  (unless (and (consp clause) (symbolp (first clause)))
    (error "state-machine: expected (STATE-NAME . TRANSITIONS), got ~S" clause))
  ...)
```

**Editor/DX parity.** A DSL macro must indent, debug-step, and macroexpand as naturally as a built-in special
form — if the user must think about internal indentation or step-debugging quirks, the abstraction has
failed. In CL use `&body` (not `&rest`) for the trailing body argument so SLY/SLIME's arglist-derived
indentation works automatically. In Elisp, `(declare (indent N) (debug FORM))` as the first form is not
automatic and must be written explicitly:

```lisp
;; Emacs Lisp
(defmacro with-resource (var resource-form &rest body)
  "Bind VAR to RESOURCE-FORM for the dynamic extent of BODY."
  (declare (indent 1) (debug ((symbolp form) body)))
  (let ((gvar (make-symbol "resource")))
    `(let ((,gvar ,resource-form))
       (let ((,var ,gvar))
         (unwind-protect (progn ,@body)
           (close-resource ,gvar))))))
```

**Ban on macro monoliths.** A `defmacro` body with three or more nested backquote levels is an architecture
failure — nested backquote/comma is nearly unreadable and impossible to unit-test in isolation; every bug
becomes a full-expansion debugging session. The body should be a 1-3 line pipeline call, `(emit (analyze
(parse forms)))`; all logic, including code generation, lives in ordinary functions that return S-expressions
and can be unit-tested directly:

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sm--parse (clauses) ...)     ; -> AST
  (defun sm--analyze (ast) ...)       ; -> annotated AST (static checks)
  (defun sm--emit (ast) ...))         ; -> single top-level S-expression

(defmacro state-machine (name &body clauses)
  "Define a compile-time-verified finite state machine NAME."
  (sm--emit (sm--analyze (sm--parse clauses))))
```

**Total hygiene.** Every symbol the macro introduces that the user did not write must be gensym'd; every
symbol the macro intentionally exposes to user code (anaphora) must be documented as such. Unhygienic macros
cause variable-capture bugs invisible in the macro source that appear only at obscure call sites — the
classic Lisp macro footgun. In CL, `(gensym "PREFIX-")`; in Elisp, `(cl-gensym "prefix-")` or `(make-symbol
"prefix")` — never bare `(gensym)` in Elisp without a distinguishing prefix, and require `lexical-binding: t`:

```lisp
;; hygienic internal temp
(defmacro my-swap (a b)
  (let ((tmp (gensym "TMP-")))
    `(let ((,tmp ,a))
       (setf ,a ,b ,b ,tmp))))

;; intentional anaphoric capture, documented rather than hidden
(defmacro aif (test then &optional else)
  "Anaphoric IF. Binds IT to the value of TEST, visible inside THEN/ELSE.
   This capture is intentional; see anaphoric macro convention."
  `(let ((it ,test))
     (if it ,then ,else)))
```

**Derived-name interning is a second hygiene axis** that the gensym rule does not cover: gensym hygiene
protects against capture, this protects against misplacement. Any macro building a derived name — `foo-p`,
`make-foo`, `foo-supplied-p`, `with-foo` — must choose the interning package deliberately. Deriving the
package with `(symbol-package slot)` looks careful and is wrong the moment a user-supplied name is an
inherited symbol: because packages `:use #:cl`, an unqualified slot name colliding with a standard symbol
(`position`, `length`, `type`, `class`) resolves to the `COMMON-LISP` symbol, and the macro tries to intern
`POSITION-SUPPLIED-P` into `COMMON-LISP` — SBCL signals a package-lock error from a definition that looks
entirely ordinary at the call site. The user wrote a field name; the error names a package lock. Intern
derived names into a package the macro determines — `*package*` at expansion time, the package of the macro's
own name argument, or an explicit package parameter — never `(symbol-package user-supplied-symbol)`:

```lisp
;; fragile: inherits the package from the caller's symbol, which may be COMMON-LISP
(intern (format nil "~A-SUPPLIED-P" (symbol-name slot)) (symbol-package slot))

;; deliberate: the derived name lands where the definition lives
(intern (format nil "~A-SUPPLIED-P" (symbol-name slot)) (symbol-package record-name))
```

If the macro is third-party and cannot be changed, the escape hatch is `:shadow`-ing the colliding name in the
defining package.

## Canonical technique library

Drawn from On Lisp and Let Over Lambda; apply to any single macro independent of whether it sits in a larger
DSL pipeline. Code is Common Lisp; Elisp equivalents are noted where they diverge.

**Auto-gensym / o!-symbols** (`defmacro!`, Let Over Lambda ch.4) collapses once-only and gensym into a naming
convention: any symbol whose name starts with `G!` is auto-gensym'd, and any `O!`-prefixed parameter is
auto-once-only'd.

```lisp
(defun g!-symbol-p (s)
  (and (symbolp s) (> (length (symbol-name s)) 2)
       (string= (symbol-name s) "G!" :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar (lambda (s) `(,s (gensym ,(subseq (symbol-name s) 2)))) syms)
         ,@body))))

(defun o!-symbol-p (s)
  (and (symbolp s) (> (length (symbol-name s)) 2)
       (string= (symbol-name s) "O!" :end1 2)))
(defun o!-symbol-to-g!-symbol (s)
  (symb "G!" (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

;; usage: o! args evaluated exactly once; g! symbols auto-gensym'd
(defmacro! my-max2 (o!a o!b)
  `(if (> ,g!a ,g!b) ,g!a ,g!b))
```

This is the idiomatic replacement for hand-writing once-only + gensym boilerplate on every macro; prefer it
once available in a project.

**Generalized variables** (On Lisp ch.12): a macro can abstract over any setf-able "place", not just a plain
variable, by expanding through `get-setf-expansion`.

```lisp
(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro toggle (place) `(_f not ,place))
;; usage: (toggle (gethash 'k table)) or (toggle (aref v i)) — works on any place
```

`define-modify-macro` covers the common case of a fixed operator, e.g. `(define-modify-macro appendf (&rest
args) append)`.

**CPS macros** (On Lisp ch.20): a macro-defining-macro, `=defun`, defines both a macro `NAME` (call-site
sugar) and a function `=NAME` (the CPS-transformed implementation taking an explicit continuation), so callers
write ordinary-looking code while control flow underneath is continuation-passing. This is the mechanism
behind the CPS technique — see [Continuation contracts](#continuation-contracts) below for what makes such a
layer correct once it carries real error handling.

```lisp
(defvar *cont* #'identity)

(defmacro =lambda (parms &body body) `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals) `(funcall *cont* ,@retvals))

;; usage
(=defun add1 (n) (=values (1+ n)))
(=bind (result) (add1 41) (print result)) ; => prints 42
```

**Duality of syntax** (Let Over Lambda ch.4): because Common Lisp is a Lisp-2, a single name can be
simultaneously a macro (`(name args)`) and a symbol-macro (bare `name`), giving callers a choice of syntax for
the same computation. Use sparingly — it trades a small amount of surprise for call-site brevity, and should
be documented at the definition site.

```lisp
(define-symbol-macro tau (* 2 pi))    ; bare TAU expands to (* 2 pi)
(defmacro tau (radius) `(* 2 pi ,radius))  ; (TAU r) expands to (* 2 pi r)
```

**Pandoric macros** (Let Over Lambda ch.5-6): closures whose internal lexical variables are exposed for
controlled external get/set access via a dispatching function, blurring the line between a closure and an
object with named slots. The full implementation (`pandoriclet`/`plambda`/`pandoric-let`/`with-pandoric`,
built on `dlambda` and `symbol-macrolet` tricks) is intricate and easy to get subtly wrong from memory — pull
in the tested let-over-lambda library rather than hand-rolling it inline.

**Once-only** (On Lisp ch.8) — the textbook, reusable implementation, for reference:

```lisp
(defmacro once-only (names &body body)
  (let ((gensyms (mapcar (lambda (n) (gensym (string n))) names)))
    `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
             ,@body)))))
```

Its triple-backquote body is exactly the kind of nesting the ban-on-macro-monoliths law warns against — a
deliberately hardened, well-tested exception. Prefer `alexandria:once-only` over reproducing this.

Also in the catalog: **anaphoric macros** (`aif`/`awhen`/`aand`/`alambda`, capturing `it` — see Total hygiene
above for the pattern).

## Continuation contracts

The correctness contracts of a CPS or callback layer, as opposed to its mechanism. A macro system generating
continuation-passing code owns these contracts on the user's behalf — the user writes what looks like a
direct call, so any way the generated plumbing invokes the wrong continuation, invokes two, or loses a
distinction is invisible at the call site. All three below were found in review of working, tested code.

**Protect the computation, never the continuation call.** An error-handling boundary in a CPS layer must wrap
only the fallible local computation. If the continuation call sits inside the handler's scope, an error raised
by the continuation — everything downstream — is caught and misclassified as a local failure, and the failure
continuation fires for a computation that already succeeded: a double delivery, violating exactly-once
continuation invocation. The bug is nearly invisible because the natural way to write it is the wrong way: the
whole body goes inside `condition-case`/`handler-case`, and the continuation call happens to be the last form.
It only manifests when downstream code signals — a rare path in tests, a common path in production.

```lisp
;; wrong: K is called inside the handler, so K's own errors select ON-ERROR
(condition-case err
    (funcall k (parse-entry text))
  (error (funcall on-error err)))

;; right: the handler yields a result; the continuation call is outside it
(let ((outcome (condition-case err
                   (cons :ok (parse-entry text))
                 (error (cons :err err)))))
  (if (eq (car outcome) :ok)
      (funcall k (cdr outcome))
    (funcall on-error (cdr outcome))))
```

Test three things explicitly: a local error selects the failure continuation; an error raised by the success
continuation propagates unchanged (including quit/interrupt, which handler forms often swallow by accident);
exactly one continuation is invoked exactly once.

**Symbol-property annotation for generated call shapes.** When two sibling macros generate functions with
different continuation arities, the call-site macro cannot tell them apart from the name alone — a
hand-written pair that follows the naming convention without being generated will fool it. Record the contract
as a symbol property on the generated name at expansion time; the call-site macro reads it during its own
expansion.

```lisp
;; the defining macro records the calling pattern on the generated name
(eval-and-compile
  (put 'my-op/k 'my-cps-pattern 'success-and-failure))

;; the call-site macro checks it during ITS expansion — best effort, see limitation below
(let ((pattern (get callee 'my-cps-pattern)))
  (when (and pattern (not (eq pattern expected)))
    (error "my-bind: %S expects the %S calling pattern" callee pattern)))
```

Hand-written definitions that participate in the protocol must set the property themselves — an opt-in the
compiler cannot enforce, and hand-written pairs have been found missing it. Limitation: a `(get name
'property)` read during macroexpansion only sees definitions already evaluated in that image. If the guarded
definition is compiled before the definition it refers to, the get returns nil and the guard silently passes.
This makes the check best-effort within a file and unreliable across files — useful as an early-warning
diagnostic, never a guarantee. State the limitation wherever the guard is documented, and keep a runtime or
test-level check for the same contract.

**A sync wrapper cannot encode a two-outcome contract.** Wrapping a success/failure continuation pair into a
synchronous function by passing `identity` as success and a no-op as failure collapses two outcomes into one
return value — it cannot distinguish "succeeded, and the value is nil" from "failed", and every caller
silently inherits that conflation. In Elisp the collision is unavoidable rather than unlucky: nil is
simultaneously false, the empty list, and a legitimate stored value, so a cache or table lookup that branches
on the value itself reports a miss for every stored nil (observed to make a capacity-one cache grow to two
entries, because eviction never recognized the existing key). Do not branch on the value to decide presence;
carry presence separately — an explicit found-p sentinel or distinguished not-found object inside a CPS layer,
two values (CL) or a status-tag cons (Elisp) from a sync wrapper. This is the null-conflation problem behind
option/maybe types, and it applies to any key domain that includes the absence marker. Adjacent Elisp trap: 0
is truthy, so `(or cached-count default)` silently accepts a genuine zero — use an explicit positive-count
test.

## FASL-safe expansion

Constraints on what a macro may place into the code it returns, so the expansion survives file-compilation
(`compile-file`/FASL dump) and self-hosted or descriptor-backed evaluation, not just interactive
macroexpansion at the REPL — interactive expansion hides these bugs because nothing is serialized.

**Never embed a function object.** A macro that captures `#'fn` (e.g. as a default callback) and splices it
into the expansion embeds a function object the file compiler cannot externalize — `compile-file` fails at
FASL-dump time. Literal function objects are not dumpable; only source-level function designators are, and a
REPL `macroexpand` never triggers the failure because it never dumps. Splice a function-designator symbol
(`identity`, not `#'identity`) and `funcall` it, or resolve `#'fn` at the call site.

```lisp
;; bad: the &optional default is evaluated at macroexpansion to a FUNCTION OBJECT,
;; which is then spliced into the expansion and cannot be dumped to a FASL.
(defmacro deftransform (name &optional (fn #'identity))
  `(defun ,name (x) (funcall ,fn x)))   ; ,fn splices a compiled function object

;; good: let a function-designator SYMBOL flow through; funcall accepts it.
(defmacro deftransform (name &optional (fn ''identity))
  `(defun ,name (x) (funcall ,fn x)))   ; ,fn splices the symbol IDENTITY
```

**Macro-body declarations are not calls.** Leading `(declare ...)` forms in a macro body are declarations, not
runtime calls. Any machinery that evaluates a macro body form-by-form — a self-hosted or descriptor-backed
evaluator — must strip or specially handle leading declarations before evaluation, or `(declare (ignore x))`
is evaluated as a call to a nonexistent function `IGNORE`. Standard CL compilers already handle this; this is
an observation from a self-hosting expander context.

## Binding-form scope reference

A macro that mechanically rewrites code — rename a symbol, extract a function, inline a binding, hoist a let
— is only correct if it respects the exact scope, shadowing, and namespace rules of every binding form it
walks. The most common macro/refactor bug is treating every parenthesized `(name value)` shape as one uniform
"binding": the safety of a mechanical edit depends on which namespace a name lives in and which sub-forms it
shadows.

- **Value vs. callable namespaces.** `let`, `let*`, `symbol-macrolet`, `do`/`do*`, `prog`/`prog*`, `dolist`,
  `dotimes`, `with-slots`, `with-accessors` bind value/place names. `symbol-macrolet` is specifically a
  value-place form: its names must not shadow function-call heads, and its expansion forms resolve in the
  outer environment while its body is shadowed by the symbol-macro names. `flet`, `labels`, `macrolet`,
  `compiler-macrolet` introduce callable bindings that DO shadow an outer callable rename target within their
  body scope.
- **Which sub-forms see which environment**, per form — this determines which occurrences a rename may
  rewrite: `let`/`prog` evaluate init forms in the outer scope (parallel), body sees all bound names.
  `let*`/`do*`/`prog*` are sequential — each init sees earlier bound names but not its own. `do` evaluates
  init forms outer-scope; step forms, end-test, and body see all iteration variables. `dolist`/`dotimes`
  evaluate the list/count source outer-scope; the iteration variable shadows the target in the optional result
  form and body. `symbol-macrolet` expansion forms resolve in the outer environment — only body references
  count as in-scope references to the symbol macro; in quasiquote, preserve comma/comma-at prefixes and rename
  only the unquoted symbol-macro references. `with-slots`/`with-accessors`: the instance expression is
  outer-scope, slot/accessor names in specs are not value references, body references are the shadowed ones —
  renaming a bare `with-slots` spec must expand it from `slot` to `(new-name slot)` to preserve the mapping.
  `handler-bind`/`restart-bind`: the spec head (condition type/restart name) and restart-bind option keywords
  (`:report`, `:test`, `:interactive`) are designators, not value references; a lambda in a handler or
  restart-option position shadows only that lambda's own body.
- **Reader prefixes and quasiquote as reference-literal boundaries.** Under `quote`, symbols are data and must
  not be rewritten. Under quasiquote, data is protected but unquote (`,`/`,@`) re-enters evaluation, so a
  rename must thread quasiquote depth — rewrite only unquoted references and preserve the `,`/`,@` prefixes on
  the replacement. `#'symbol`/`(function symbol)` are callable designators: rename the target in executable
  context, but skip them inside quasiquoted data unless an unquote re-enters evaluation.
- **`define-modify-macro`'s implicit place argument** precedes the user lambda-list parameters. Any call-site
  rewrite (add/remove/move/reorder an argument) must offset user arguments by one to preserve the place
  argument.

## Source-rewriting correctness

Rules for any tool that reads Lisp, transforms it, and writes it back — a code walker, formatter, codemod,
refactoring command, or a macro emitting an edited copy of its input. The governing fact: **corrupted Lisp is
usually still valid Lisp**, so a transformation can change program meaning, report success, and pass its whole
test suite. Every failure shape below was found by running the built tool against adversarial input, never by
a test.

**Reparse success is not correctness.** A write-path guard that refuses any rewrite whose output fails to
reparse is a real guard against one narrow failure — unreadable text — and no guard at all against meaning
change; the output of a broken transform reparses cleanly because the corruption is well-formed. A structural
equivalence check on the parsed trees is stronger but inherits the same blindness for anything the node tree
does not represent. Observed at its worst in a canonicalizing rewrite that silently deleted every comment in a
file, wrote it, and exited zero — the renderer did not emit comments and the tree-equivalence guard did not
represent them, so both the transform and its own verification agreed the output was identical. Enumerate what
your node tree does not carry — comments, reader prefixes, whitespace significance, original numeric notation
— and assert on the exact output text for each.

Failure shapes to guard against, each silent and each observed in practice:

- **Whitespace transforms inside atom spans.** The rule is "outside every atom span", not "outside strings" —
  multi-line block comments and character literals have the same interior-text problem as strings; collapsing
  whitespace inside a block comment or rewriting the space in a character literal changes or destroys the
  atom. Compute atom spans once (strings, block/line comments, character literals, dialect-specific literals)
  and make every textual transform consult that map.
- **Dialect-blind operator tables.** Spelling and meaning of core syntax vary by dialect — in Common Lisp `#'`
  is shorthand for the `function` special operator; in Scheme, Racket, and Clojure `#'` is a syntax-object or
  var-quote reader macro and `function` is an ordinary symbol with no special meaning. Key every operator and
  reader-macro table by dialect, determined explicitly (file extension, flag, declared setting) — never
  inferred from content. A tool that cannot determine the dialect must refuse to rewrite.
- **Reader prefixes discarded.** A rewrite that normalizes a form must carry its reader prefixes with it.
  Simplifying `(quote x)` to `'x` is meaning-preserving; doing it to `#+sbcl (quote x)` and emitting `'x`
  deletes a read-time conditional, so code excluded on other implementations becomes unconditionally present.
  Represent reader prefixes as attributes of the node they precede so any node-level rewrite carries them
  automatically — a prefix stored as a sibling node is dropped by the first transform that replaces its
  neighbour.
- **Folding through unevaluated contexts.** Constant folding, simplification, and inlining must stop at
  `quote`/`quasiquote` boundaries: under `quote` the text is data, and folding `(+ 1 2)` inside quoted data
  changes the data. Under quasiquote the same applies except where an unquote re-enters evaluation — thread
  quasiquote depth rather than testing for a quote head, for every transform, not only renames.
- **Host-language escaping leaking into output.** Re-emitting a string literal through the host language's own
  escaping routine injects escapes the target dialect does not use. Lisp string syntax escapes only backslash
  and double quote; a host that also escapes newlines, tabs, or non-ASCII characters silently rewrites a
  literal containing a real newline into a two-character escape sequence. Write a dialect-specific serializer
  for every literal type rather than reusing the implementation language's debug/repr formatting; round-trip
  every literal shape through parse and print and assert byte equality.
- **Numeric-tower assumptions.** Arithmetic simplification in the tool's own numeric model, rather than the
  target dialect's, produces confidently wrong constants — folding `(/ 1.0 2)` through an integer division
  path yields `0`. Fold only where the tool can reproduce the target's contagion and division rules exactly,
  and refuse to fold mixed-type or division expressions otherwise; preserving the original expression is
  always acceptable, emitting a wrong literal is not.
- **Indentation anchored to the wrong line.** Re-indenting a moved or extracted form against the indentation
  of the definition's own first line, rather than the column it is being placed at, produces output that is
  valid but progressively mis-shaped — for a formatter, that is the entire product.
- **Comments absent from the node tree.** If comments are not nodes, every transform deletes them, and no
  tree-based verification notices — the most destructive shape because it fails silently across the whole
  file at once. Attach comments as first-class nodes with an explicit attachment rule (leading, trailing,
  own-line relative to a sibling), and add a comment-count/text assertion to the tool's own output
  verification. If comments genuinely cannot be represented, the tool must refuse to write rather than write a
  lossy result with a zero exit status.

Verification protocol: reparsing the output is necessary, not sufficient — assert the exact expected text,
byte for byte. Run the built binary or the real command path, since several of these shapes appear only there
(a unit test calling the transform function directly bypasses the renderer, the write path, and the exit-status
logic — where the corruption and the false success both live). Drive the tool with adversarial input, not tidy
fixtures: block comments spanning lines, character literals for whitespace, reader conditionals, quoted and
quasiquoted data, mixed-type arithmetic, strings holding quotes and real newlines, files that are entirely
comments. Verify at the file level as well as the form level — a per-form test cannot detect a loss uniform
across the file, which is exactly how comment deletion escaped.

## Lambda-list and quasiquote traps

Implementation-detail traps when a macro system parses and re-emits macro lambda lists, and when tests
construct expected expansions by hand.

- **`&whole` binding shape.** `&whole` binds the entire original form. If a destructuring routine stores its
  result as an alist consumed via `(cdr (assoc ...))`, the `&whole` entry must be a dotted pair `(cons whole
  arg)` — but that dotted shape is not itself a valid `let` binding, so normalize dotted/improper entries to
  `(var value)` at the code-generation boundary before emitting `let`/`let*`.
- **`&environment` extraction.** Requires two coordinated steps: extract the environment symbol and wrap the
  expander body so it is bound, AND remove `&environment` from the lambda list used to generate call-site
  argument bindings, or the environment symbol is overwritten by an argument binding. Some expansion paths
  call a local expander with a nil environment — normalize nil to a non-nil sentinel when the macro requests
  `&environment`.
- **Quasiquote in expected test values.** Do not construct an expected expansion with `'(,name ...)` inside a
  backquote — nested quote/backquote can preserve the comma as literal data instead of interpolating the
  gensym. Build the expected structure explicitly with `list`/`cons`.
- **Reuse lambda-list binding generation.** `defsetf` long form and similar accessor-defining macros should
  reuse the same lambda-list binding generation as macro lambda lists, so optional/rest/key/aux structure is
  supported uniformly; bind the store variable separately and emit the body inside a `let`/`let*` wrapper.
- **Quote requested data into helpers.** When an expander forwards user-supplied structural options into a
  runtime helper (e.g. the symbol-type list of `with-package-iterator`: `:internal`/`:external`/`:inherited`),
  pass them as quoted data, not a live form — otherwise `(:internal :external :inherited)` is emitted as a
  runtime call.
- **`with-standard-io-syntax`-style bindings.** Must rebind the full ANSI set (21 variables, CLHS
  `with-standard-io-syntax`), not a subset. Beyond the read/print numeric and escape variables it must bind
  `*readtable*` to a standard readtable (`copy-readtable nil`), `*print-pprint-dispatch*` to a standard
  dispatch (`copy-pprint-dispatch nil`), and `*print-readably*` to `t`. Omitting `*readtable*` or the
  pprint-dispatch/readably bindings is a common incomplete-reimplementation bug.

## Dialect notes

| | Common Lisp | Emacs Lisp |
|---|---|---|
| Phase separation | `eval-when (:compile-toplevel :load-toplevel :execute)` | `eval-and-compile`; requires `-*- lexical-binding: t; -*-` |
| Hygiene primitive | `gensym` (or `alexandria:once-only`/`with-gensyms` for batches) | `cl-gensym` or `make-symbol`; bare `gensym` has no namespacing — always pass a prefix |
| Editor indentation | Derived automatically from `&body` by SLY/SLIME | Explicit `(declare (indent N) (debug SPEC))` as the first form — not automatic |
| Verification | `macroexpand-1` / `macroexpand`; `sly-macroexpand-1`/`-all` interactively | `macroexpand-1`, `macroexpand-all` (cl-macs); `pp-macroexpand-last-sexp` or macrostep.el |
| Debug tooling | `sly-macrostep` (macrostep.el via SLY) | macrostep.el |
| Byte-compilation | — | Compile-time helpers not wrapped in `eval-and-compile` fail silently to a runtime call — always verify with `emacs --batch -f batch-byte-compile` |

## Macro vs. function

If the abstraction needs to control evaluation (timing/order/whether an argument is evaluated at all), capture
syntax, or generate code shaped by its arguments, use a macro — but keep it a thin pipeline hook over pure
parse/analyze/emit functions. Otherwise use an ordinary or higher-order function; a macro adds hygiene risk
and debugging friction for no semantic benefit.

## References

- Paul Graham, *On Lisp* (1993) — Macro Basics (defmacro/backquote/mkstr/symb/flatten), Generalized Variables
  (`get-setf-expansion`), Utility of Once-Only, Anaphoric Macros, Macros Returning Functions,
  Macros as Programs (macro-defining-macros), Continuations (`=defun`/`=lambda`/`=bind`/`=values`).
- Doug Hoyte, *Let Over Lambda* (2008) — Macro Basics (g!/o!-symbols, `defmacro/g!`, `defmacro!`), Read Macros
  (opt-in, narrowly-scoped), Duality of Syntax, Pandoric Macros.

Use these as the source of truth for "is this macro written correctly"; when in doubt about a technique's
exact semantics, prefer consulting the book or a tested library (alexandria, let-over-lambda) over
reproducing intricate code (once-only, pandoric) from memory.

## Related

- [common-lisp-ecosystem](../common-lisp-ecosystem/SKILL.md) — CLOS, ASDF, condition-system, and general
  Common Lisp fundamentals underlying macro design.
- [emacs-ecosystem](../emacs-ecosystem/SKILL.md) — Emacs Lisp fundamentals, use-package, and editor
  integration the DX laws above build on.
- [sbcl-usage](../sbcl-usage/SKILL.md) — macroexpand/trace/inspect workflows for verifying and debugging
  expansions at runtime.
- [investigation-patterns](../investigation-patterns/SKILL.md) — evidence-driven debugging methodology for
  tracking down capture/evaluation-order bugs.
- [serena-usage](../serena-usage/SKILL.md) — symbol navigation across macro definitions and their call sites.
- [test-integrity](../test-integrity/SKILL.md) — false-green testing, the general form of a rewriting tool
  that corrupts source and passes its own suite.
