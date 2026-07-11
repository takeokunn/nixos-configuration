---
name: Lisp Macro Architecture
description: This skill should be used when the user asks to "write a macro", "defmacro", "design a DSL", "build a compiler-time code transformer", "hygienic macro", "macro hygiene", "code walker", "CPS transform", "anaphoric macro", "once-only", "g!-symbol", "duality of syntax", "pandoric macro", "On Lisp", "Let Over Lambda", or works with compile-time metaprogramming in Common Lisp or Emacs Lisp. Provides both the canonical macro-writing technique catalog (On Lisp, Let Over Lambda) and a rigorous engineering discipline — phase separation, hygiene, evaluation-order preservation, compile-time diagnostics — for turning those techniques into correct, production-quality macros and DSLs. For general CL/Elisp language basics, defer to common-lisp-ecosystem / emacs-ecosystem.
version: 2.1.0
---

<purpose>
  Serve as a general-purpose Lisp macro-writing skill, dialect-agnostic across Common Lisp
  and Emacs Lisp, combining two layers:
  (1) the canonical technique catalog for "correct" macros as established in Paul Graham's
  On Lisp and Doug Hoyte's Let Over Lambda —
  once-only, anaphora, auto-gensym (g!/o!-symbols), generalized variables, macros returning
  functions, CPS macros, macro-defining-macros, duality of syntax, pandoric macros — and
  (2) an engineering discipline that turns those techniques into non-trivial, multi-clause
  DSLs safely: phase separation, hygiene, evaluation-order preservation, compile-time
  diagnostics, and a parser/analyzer/emitter pipeline behind a thin defmacro.
  Reach for layer (1) when writing or reviewing any individual macro; reach for layer (2)
  when the macro is a DSL with several clause forms or non-trivial static analysis.
</purpose>

<scope>
  <in_scope>
    <item>The canonical macro technique catalog: once-only, anaphora, auto-gensym (g!/o!-symbols), generalized variables, macros returning functions, CPS macros, macro-defining-macros, duality of syntax, pandoric macros (see canonical_technique_library)</item>
    <item>Macro/DSL architecture: parser → analyzer/walker → emitter pipelines behind a thin defmacro</item>
    <item>Phase separation between compile-time helpers and runtime code (eval-when / eval-and-compile)</item>
    <item>Hygiene: gensym discipline, intentional anaphoric capture, evaluation-order preservation</item>
    <item>Compile-time diagnostics: turning malformed DSL input into macro-expansion-time errors</item>
    <item>Editor/DX metadata: &amp;body vs &amp;rest, (declare (indent ...) (debug ...)) in Elisp</item>
    <item>Self-verification of expansions (macroexpand-1 / macroexpand / pp-macroexpand-last-sexp)</item>
  </in_scope>
  <out_of_scope>
    <item>CLOS, ASDF, condition system fundamentals — see common-lisp-ecosystem</item>
    <item>SBCL runtime operations, debugging, profiling — see sbcl-usage</item>
    <item>Emacs package system, use-package, LSP integration — see emacs-ecosystem</item>
    <item>Reader macros / set-macro-character — covered only as a scoped, opt-in technique, not a default tool</item>
  </out_of_scope>
</scope>

<tools>
  <tool>Read - Inspect existing macro definitions and their call sites</tool>
  <tool>Edit - Modify defmacro forms and their compile-time helper functions</tool>
  <tool>Bash - Run sbcl/emacs --batch to verify macroexpansion and byte-compilation</tool>
  <tool>mcp__plugin_claude-code-home-manager_context7__query-docs - Verify ASDF/SBCL/Elisp macro-system edge cases</tool>
</tools>

<absolute_laws priority="critical">
  <law name="no_runtime_resolution">
    <statement>Resolve everything resolvable at macro-expansion time: clause structure, state/register dependency graphs, lifetimes, continuation chains. Never reach for eval.</statement>
    <why>Every fact known from the S-expression shape alone is a fact the runtime should never have to recompute. A DSL that defers this to eval pays a performance and safety tax on every execution instead of once at compile time.</why>
    <how_to_apply>In the analyzer stage, build a static graph (plist/struct/alist) over the AST and answer questions ("is this register still live here?", "does this clause reference an undefined state?") by walking that graph, not by generating code that asks the question at runtime.</how_to_apply>
  </law>

  <law name="strict_phase_separation">
    <statement>Compile-time helper functions (parser, analyzer, emitter) must be declared so they exist in the compile-time environment.</statement>
    <why>Without this, cross-compilation, minimal-compile (fasl-only), or a fresh REPL load order in CL will signal "undefined function" during macroexpansion; in Elisp, byte-compiling a file that uses a macro from another file will silently fall back to a runtime function call and lose expansion-time errors.</why>
    <how_to_apply>
      CL: wrap parser/analyzer/emitter defuns in <code>(eval-when (:compile-toplevel :load-toplevel :execute) ...)</code>.
      Elisp: wrap them in <code>(eval-and-compile ...)</code>, and require lexical-binding: t at the top of the file.
    </how_to_apply>
  </law>

  <law name="evaluation_order_and_single_evaluation">
    <statement>Never evaluate a user-supplied argument form more than once, and never reorder the left-to-right evaluation of user-supplied forms.</statement>
    <why>A macro that evaluates <code>(incf counter)</code> twice or evaluates argument B before argument A silently breaks any caller relying on ordinary function-call semantics — the single most common macro-hygiene bug.</why>
    <how_to_apply>Bind every argument form exactly once via gensym'd let-bindings in the order they appear (the "once-only" idiom), then reference only the bound symbols in the expansion body.</how_to_apply>
  </law>

  <law name="compile_time_diagnostics">
    <statement>Reject malformed DSL input during macro-expansion with an actionable error, not at runtime.</statement>
    <why>DSL users write S-expressions, not English; the parser is their only source of feedback. A runtime error three call frames deep costs far more debugging time than an expansion-time error that names the offending clause.</why>
    <how_to_apply>The parser validates shape as it builds the AST and calls (error "~S: expected (VAR FORM) in clause, got ~S" 'macro-name clause) [CL] / (error "macro-name: expected (VAR FORM) in clause, got %S" clause) [Elisp] before the emitter ever runs.</how_to_apply>
  </law>

  <law name="editor_dx_parity">
    <statement>A DSL macro must indent, debug-step, and macroexpand as naturally as a built-in special form.</statement>
    <why>If the user must think about internal indentation or step-debugging quirks, the abstraction has failed — cognitive load has leaked from "what the DSL means" to "how the DSL is implemented."</why>
    <how_to_apply>
      CL: use &amp;body (not &amp;rest) for the trailing body argument so SLY/SLIME's arglist-derived indentation works automatically.
      Elisp: add <code>(declare (indent N) (debug FORM))</code> as the first form in the macro body.
    </how_to_apply>
  </law>

  <law name="ban_on_macro_monoliths">
    <statement>A defmacro body containing three or more nested backquote levels is an architecture failure.</statement>
    <why>Nested backquote/comma is nearly unreadable and impossible to unit-test in isolation; every bug becomes a full-expansion debugging session.</why>
    <how_to_apply>The defmacro body should be a 1-3 line pipeline call: <code>(emit (analyze (parse forms)))</code>. All real logic — including code generation — lives in ordinary functions that return S-expressions and can be unit-tested directly without macroexpansion.</how_to_apply>
  </law>

  <law name="total_hygiene">
    <statement>Every symbol introduced by the macro that the user did not write must be gensym'd; every symbol the macro intentionally exposes to user code (anaphora) must be documented as such.</statement>
    <why>Unhygienic macros cause variable capture bugs that are invisible in the macro source and only appear at obscure call sites — the classic Lisp macro footgun.</why>
    <how_to_apply>
      CL: (gensym "PREFIX-") for internal temporaries.
      Elisp: (cl-gensym "prefix-") or (make-symbol "prefix") — never (gensym) alone without a distinguishing prefix in Elisp; require lexical-binding: t.
      Anaphoric macros (e.g. aif binding `it`): use the literal symbol, and state the capture explicitly in the docstring.
    </how_to_apply>
  </law>
</absolute_laws>

<workflow>
  <phase name="semantic_design">
    <objective>Design the DSL's input S-expression and prove its expansion is efficient before writing any macro code</objective>
    <step order="1">
      <action>Declare the target dialect (Common Lisp or Emacs Lisp) explicitly — hygiene primitives and phase-separation forms differ</action>
      <output>Dialect declared</output>
    </step>
    <step order="2">
      <action>Draft the DSL's user-facing input S-expression, optimizing for the lowest possible cognitive load ("集中力の放棄" — the user should never think about registers, continuations, or environment plumbing)</action>
      <output>Example input S-expression</output>
    </step>
    <step order="3">
      <action>Hand-write the ideal, fully-expanded, runtime-optimal S-expression that this input should produce — this is the executable proof of efficiency the design is judged against</action>
      <output>Example expanded S-expression, annotated with why each part is there</output>
    </step>
  </phase>

  <phase name="compiler_architecture">
    <objective>Implement parser, analyzer, and emitter as independently testable compile-time pure functions</objective>
    <step order="1">
      <action>Parser: read the raw DSL form, validate its shape, and build an intermediate representation (plist/alist/defstruct). Signal a compile-time error immediately on malformed input.</action>
      <output>parse(form) -&gt; AST, or a compile-time error</output>
    </step>
    <step order="2">
      <action>Analyzer/Walker: traverse the AST to perform static analysis — dependency graphs, lifetime/liveness checks, non-deterministic-branch enumeration — entirely without emitting code yet</action>
      <output>analyze(ast) -&gt; annotated AST</output>
    </step>
    <step order="3">
      <action>Emitter: transform the annotated AST into the final, flattened, optimized S-expression from Phase 1</action>
      <output>emit(annotated-ast) -&gt; S-expression</output>
    </step>
  </phase>

  <phase name="macro_exposure">
    <objective>Expose the pipeline through the thinnest possible defmacro</objective>
    <step order="1">
      <action>Write defmacro as a direct pipeline call: (emit (analyze (parse forms))) — no independent logic in the macro body itself</action>
      <output>Thin defmacro, ≤3 lines of logic</output>
    </step>
    <step order="2">
      <action>Attach editor/DX metadata: &amp;body argument ordering in CL, or (declare (indent ...) (debug ...)) in Elisp</action>
      <output>Macro indents and debug-steps like a built-in form</output>
    </step>
  </phase>

  <phase name="proof_of_correctness">
    <objective>Verify the expansion matches the Phase-1 proof and preserves hygiene/evaluation-order guarantees</objective>
    <step order="1">
      <action>Run macroexpand-1 / macroexpand (CL) or macroexpand-1 / pp-macroexpand-last-sexp (Elisp) against the example input and diff it against the Phase-1 ideal expansion</action>
      <output>Expansion matches design proof</output>
    </step>
    <step order="2">
      <action>Self-check: does any argument form appear more than once in the expansion? Are all internal symbols gensym'd? Does a malformed clause raise a compile-time error naming the clause?</action>
      <output>Hygiene and diagnostics checklist passed</output>
    </step>
  </phase>
</workflow>

<patterns>
  <pattern name="thin_macro_pipeline">
    <description>The defmacro itself is a 1-3 line hook; all logic lives in ordinary, independently testable functions (Law: ban_on_macro_monoliths)</description>
    <example>
      ;; Common Lisp
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (defun sm--parse (clauses) ...)     ; -&gt; AST
        (defun sm--analyze (ast) ...)       ; -&gt; annotated AST (static checks)
        (defun sm--emit (ast) ...))         ; -&gt; single top-level S-expression

      (defmacro state-machine (name &amp;body clauses)
        "Define a compile-time-verified finite state machine NAME."
        (sm--emit (sm--analyze (sm--parse clauses))))
    </example>
  </pattern>

  <pattern name="phase_separation">
    <description>Compile-time helpers must exist in the compile-time environment (Law: strict_phase_separation)</description>
    <example>
      ;; Common Lisp
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (defun dsl--parse-clause (clause) ...))

      ;; Emacs Lisp (requires -*- lexical-binding: t; -*-)
      (eval-and-compile
        (defun dsl--parse-clause (clause) ...))
    </example>
  </pattern>

  <pattern name="once_only_evaluation_order">
    <description>Bind every user-supplied form exactly once, in argument order, before referencing it in the expansion (Law: evaluation_order_and_single_evaluation)</description>
    <example>
      ;; Common Lisp (alexandria:once-only handles this idiom directly)
      (defmacro my-max2 (a b)
        (alexandria:once-only (a b)
          `(if (&gt; ,a ,b) ,a ,b)))

      ;; Emacs Lisp (manual once-only, since cl-lib has no equivalent)
      (defmacro my-max2 (a b)
        (let ((ga (make-symbol "a")) (gb (make-symbol "b")))
          `(let ((,ga ,a) (,gb ,b))
             (if (&gt; ,ga ,gb) ,ga ,gb))))
    </example>
  </pattern>

  <pattern name="compile_time_diagnostics">
    <description>Reject malformed input with a named, actionable error before the emitter runs (Law: compile_time_diagnostics)</description>
    <example>
      ;; Common Lisp
      (defun dsl--parse-clause (clause)
        (unless (and (consp clause) (symbolp (first clause)))
          (error "state-machine: expected (STATE-NAME . TRANSITIONS), got ~S" clause))
        ...)

      ;; Emacs Lisp
      (defun dsl--parse-clause (clause)
        (unless (and (consp clause) (symbolp (car clause)))
          (error "state-machine: expected (STATE-NAME . TRANSITIONS), got %S" clause))
        ...)
    </example>
  </pattern>

  <pattern name="editor_dx_metadata">
    <description>Make the DSL indent and step-debug like a built-in form (Law: editor_dx_parity)</description>
    <example>
      ;; Common Lisp: &amp;body (not &amp;rest) signals "indent as code" to SLY/SLIME
      (defmacro with-resource ((var resource-form) &amp;body body)
        `(let ((,var ,resource-form))
           (unwind-protect (progn ,@body)
             (close-resource ,var))))

      ;; Emacs Lisp: declare indent + debug spec explicitly
      (defmacro with-resource (var resource-form &amp;rest body)
        "Bind VAR to RESOURCE-FORM for the dynamic extent of BODY."
        (declare (indent 1) (debug ((symbolp form) body)))
        (let ((gvar (make-symbol "resource")))
          `(let ((,gvar ,resource-form))
             (let ((,var ,gvar))
               (unwind-protect (progn ,@body)
                 (close-resource ,gvar))))))
    </example>
  </pattern>

  <pattern name="total_hygiene_and_intentional_capture">
    <description>Gensym internal temporaries; document anaphoric capture explicitly instead of hiding it (Law: total_hygiene)</description>
    <example>
      ;; Common Lisp: hygienic internal temp
      (defmacro my-swap (a b)
        (let ((tmp (gensym "TMP-")))
          `(let ((,tmp ,a))
             (setf ,a ,b ,b ,tmp))))

      ;; Common Lisp: intentional anaphoric capture, documented
      (defmacro aif (test then &amp;optional else)
        "Anaphoric IF. Binds IT to the value of TEST, visible inside THEN/ELSE.
         This capture is intentional; see anaphoric macro convention."
        `(let ((it ,test))
           (if it ,then ,else)))

      ;; Emacs Lisp: hygienic internal temp
      (defmacro my-swap (a b)
        (let ((tmp (cl-gensym "tmp-")))
          `(let ((,tmp ,a))
             (setf ,a ,b ,b ,tmp))))
    </example>
  </pattern>

  <pattern name="no_runtime_resolution">
    <description>Push static analysis (dependency/lifetime graphs, non-deterministic branch enumeration) entirely into the analyzer stage (Law: no_runtime_resolution)</description>
    <example>
      ;; Analyzer stage answers "is REGISTER still live after this instruction?"
      ;; by walking the AST once and recording last-use positions — the emitted
      ;; code never asks this question again; it just reuses or frees the slot.
      (defun vm--analyze-liveness (instructions)
        (let ((last-use (make-hash-table)))
          (loop for instr in instructions
                for pos from 0
                do (dolist (reg (vm--instr-reads instr))
                     (setf (gethash reg last-use) pos)))
          last-use))
    </example>
  </pattern>
</patterns>

<canonical_technique_library>
  <description>
    The core repertoire for writing individually "correct" macros, drawn from Paul Graham's
    On Lisp and Doug Hoyte's Let Over Lambda. Apply these techniques to any single macro,
    independent of whether it is part of a larger DSL pipeline (see workflow/patterns above).
    Code below is Common Lisp; Elisp equivalents are noted where they diverge.
  </description>

  <technique name="once_only" source="On Lisp, ch.8 (Utility of Once-Only)">
    <description>The textbook, reusable once-only: binds each named form to a fresh gensym exactly once, preserving argument evaluation order, so the macro body can reference the names freely without re-evaluating or reordering user code.</description>
    <example>
      (defmacro once-only (names &amp;body body)
        (let ((gensyms (mapcar (lambda (n) (gensym (string n))) names)))
          `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
             `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
                ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
                   ,@body)))))

      ;; usage
      (defmacro my-max2 (a b)
        (once-only (a b)
          `(if (&gt; ,a ,b) ,a ,b)))
    </example>
    <note>The triple-backquote body is exactly the kind of nesting the ban_on_macro_monoliths law warns against — it is a deliberately-hardened, well-tested exception. Prefer a battle-tested library implementation (alexandria:once-only) over hand-rolling this in application code.</note>
  </technique>

  <technique name="anaphoric_macros" source="On Lisp, ch.14 (Anaphoric Macros)">
    <description>Intentionally capture a fixed variable name (conventionally `it`) so the body can refer to a just-computed value without re-stating it. Document the capture; it is the entire point of the macro.</description>
    <example>
      (defmacro aif (test then &amp;optional else)
        "Anaphoric IF: binds IT to the value of TEST within THEN/ELSE."
        `(let ((it ,test))
           (if it ,then ,else)))

      (defmacro awhen (test &amp;body body)
        `(aif ,test (progn ,@body)))

      (defmacro aand (&amp;rest args)
        (cond ((null args) t)
              ((null (cdr args)) (car args))
              (t `(aif ,(car args) (aand ,@(cdr args))))))

      (defmacro alambda (parms &amp;body body)
        "Anonymous function that can recurse via SELF."
        `(labels ((self ,parms ,@body))
           #'self))

      ;; usage: (alambda (n) (if (&lt;= n 1) 1 (* n (self (1- n)))))
    </example>
  </technique>

  <technique name="auto_gensym_g_o_symbols" source="Let Over Lambda, ch.4 (Macro Basics)">
    <description>defmacro! auto-gensyms every symbol whose name starts with "G!" appearing anywhere in the macro body, and auto-once-only's every "O!"-prefixed parameter in the argument list — collapsing the once_only and gensym techniques above into a declarative naming convention instead of manual boilerplate.</description>
    <example>
      ;; prerequisite utilities (On Lisp, ch.4)
      (defun mkstr (&amp;rest args)
        (with-output-to-string (s) (dolist (a args) (princ a s))))
      (defun symb (&amp;rest args)
        (values (intern (apply #'mkstr args))))
      (defun flatten (x)
        (labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec (car x) (rec (cdr x) acc))))))
          (rec x nil)))

      (defun g!-symbol-p (s)
        (and (symbolp s) (&gt; (length (symbol-name s)) 2)
             (string= (symbol-name s) "G!" :end1 2)))

      (defmacro defmacro/g! (name args &amp;rest body)
        (let ((syms (remove-duplicates
                     (remove-if-not #'g!-symbol-p (flatten body)))))
          `(defmacro ,name ,args
             (let ,(mapcar (lambda (s) `(,s (gensym ,(subseq (symbol-name s) 2)))) syms)
               ,@body))))

      (defun o!-symbol-p (s)
        (and (symbolp s) (&gt; (length (symbol-name s)) 2)
             (string= (symbol-name s) "O!" :end1 2)))
      (defun o!-symbol-to-g!-symbol (s)
        (symb "G!" (subseq (symbol-name s) 2)))

      (defmacro defmacro! (name args &amp;rest body)
        (let* ((os (remove-if-not #'o!-symbol-p args))
               (gs (mapcar #'o!-symbol-to-g!-symbol os)))
          `(defmacro/g! ,name ,args
             `(let ,(mapcar #'list (list ,@gs) (list ,@os))
                ,(progn ,@body)))))

      ;; usage: o! args are evaluated exactly once; g! symbols are auto-gensym'd
      (defmacro! my-max2 (o!a o!b)
        `(if (&gt; ,g!a ,g!b) ,g!a ,g!b))
    </example>
    <note>This is the idiomatic replacement for hand-writing once_only + gensym boilerplate on every macro. Prefer it once available in a project; the manual once_only_evaluation_order pattern above shows what it replaces.</note>
  </technique>

  <technique name="generalized_variables" source="On Lisp, ch.12 (Generalized Variables)">
    <description>Write a macro that abstracts over any "place" (any form valid as a setf target), not just plain variables, by expanding through get-setf-expansion.</description>
    <example>
      (defmacro _f (op place &amp;rest args)
        (multiple-value-bind (vars forms var set access)
            (get-setf-expansion place)
          `(let* (,@(mapcar #'list vars forms)
                  (,(car var) (,op ,access ,@args)))
             ,set)))

      (defmacro toggle (place)
        `(_f not ,place))

      ;; usage: (toggle (gethash 'k table)) or (toggle (aref v i)) — works on any place
    </example>
    <note>Standard CL also provides define-modify-macro for the common case of a fixed operator, e.g. (define-modify-macro appendf (&amp;rest args) append).</note>
  </technique>

  <technique name="cps_macros" source="On Lisp, ch.20 (Continuations)">
    <description>A macro-defining-macro: =defun defines both a macro NAME (the call-site sugar) and a function =NAME (the CPS-transformed implementation taking an explicit continuation), so callers write ordinary-looking code while the underlying control flow is continuation-passing.</description>
    <example>
      (defvar *cont* #'identity)

      (defmacro =lambda (parms &amp;body body)
        `#'(lambda (*cont* ,@parms) ,@body))

      (defmacro =defun (name parms &amp;body body)
        (let ((f (intern (concatenate 'string "=" (symbol-name name)))))
          `(progn
             (defmacro ,name ,parms
               `(,',f *cont* ,,@parms))
             (defun ,f (*cont* ,@parms) ,@body))))

      (defmacro =bind (parms expr &amp;body body)
        `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

      (defmacro =values (&amp;rest retvals)
        `(funcall *cont* ,@retvals))

      ;; usage
      (=defun add1 (n) (=values (1+ n)))
      (=bind (result) (add1 41) (print result)) ; => prints 42
    </example>
    <note>This is the direct, textbook mechanism behind the "CPS transform" the Absolute Laws refer to: the continuation chain is built entirely by macro-defined plumbing (*cont*), and =defun/=bind read like ordinary function calls at every call site.</note>
  </technique>

  <technique name="duality_of_syntax" source="Let Over Lambda, ch.4 (Duality of Syntax)">
    <description>Because Common Lisp is a Lisp-2 (separate function and variable namespaces), a single name can be simultaneously a macro (callable as `(name args)`) and a symbol-macro (usable bare as `name`), giving callers a choice of syntax for the same underlying computation.</description>
    <example>
      (define-symbol-macro tau (* 2 pi))    ; bare TAU expands to (* 2 pi)

      (defmacro tau (radius)                ; (TAU r) expands to (* 2 pi r)
        `(* 2 pi ,radius))

      tau        ; => 6.283185307179586
      (tau 5)    ; => 31.41592653589793
    </example>
    <note>Minimal illustration in the spirit of Let Over Lambda's duality-of-syntax technique, not a verbatim book example. Use sparingly — it trades a small amount of surprise for call-site brevity, and should be documented at the definition site.</note>
  </technique>

  <technique name="pandoric_macros" source="Let Over Lambda, ch.5-6 (Pandoric Macros)">
    <description>Closures whose internal lexical variables are exposed for controlled external get/set access via a dispatching function, blurring the line between a closure and an object with named slots.</description>
    <note>The full implementation (pandoriclet/plambda/pandoric-let/with-pandoric, built on dlambda and symbol-macrolet tricks) is intricate and easy to get subtly wrong from memory. Do not hand-roll it inline: pull in the tested let-over-lambda library implementation, or consult the book directly, before using this technique in production code.</note>
  </technique>
</canonical_technique_library>

<fasl_safe_expansion>
  <description>
    Constraints on what a macro may place into the code it returns, so the expansion survives
    file-compilation (compile-file / FASL dump) and self-hosted or descriptor-backed evaluation,
    not just interactive macroexpansion at the REPL. Interactive expansion hides these bugs
    because nothing is serialized.
  </description>

  <principle name="never_embed_a_function_object">
    <statement>Do not let a compiled function object become a literal in macro output. A macro that captures #'fn (e.g. as a default callback or transformer) and splices it into the expansion embeds a function object that the file compiler cannot externalize, and compile-file fails at FASL-dump time.</statement>
    <why>Literal function objects are not dumpable to a FASL; only source-level function designators are. A REPL macroexpand never triggers the failure because it never dumps.</why>
    <how_to_apply>Splice a function-designator symbol (identity, not #'identity) and funcall it in the expansion, or resolve #'fn at the call site rather than inside the macro. This keeps the expansion source-serializable.</how_to_apply>
    <example>
      ;; bad: the &optional default is evaluated at macroexpansion to a FUNCTION OBJECT,
      ;; which is then spliced into the expansion and cannot be dumped to a FASL.
      (defmacro deftransform (name &optional (fn #'identity))
        `(defun ,name (x) (funcall ,fn x)))   ; ,fn splices a compiled function object

      ;; good: let a function-designator SYMBOL flow through; funcall accepts it.
      (defmacro deftransform (name &optional (fn ''identity))
        `(defun ,name (x) (funcall ,fn x)))   ; ,fn splices the symbol IDENTITY
    </example>
  </principle>

  <principle name="macro_body_declarations_are_not_calls">
    <statement>Leading (declare ...) forms in a macro body are declarations, not runtime calls. Any machinery that evaluates a macro body form-by-form — a self-hosted or descriptor-backed evaluator — must strip or specially handle leading declarations before evaluation; otherwise (declare (ignore x)) is evaluated as a call to a nonexistent function IGNORE.</statement>
    <scope>General for any custom macro-body evaluator; standard CL compilers already handle this. Recorded as an observation from a self-hosting expander context.</scope>
  </principle>
</fasl_safe_expansion>

<binding_form_scope_reference>
  <description>
    A macro that mechanically rewrites code — rename a symbol, extract a function, inline a
    binding, hoist a let — is only correct if it respects the exact scope, shadowing, and
    namespace rules of every binding form it walks. The most common macro/refactor bug is
    treating all parenthesized (name value) shapes as one uniform "binding": the safety of a
    mechanical edit depends on which namespace a name lives in and which sub-forms it shadows.
    This catalogs the rules that make such rewrites safe.
  </description>

  <namespace_rule name="value_vs_callable_bindings">
    <statement>Distinguish value-place bindings from callable bindings, because a name in one namespace must not shadow a rename target in the other.</statement>
    <value_bindings>let, let*, symbol-macrolet, do/do*, prog/prog*, dolist, dotimes, with-slots, with-accessors bind value/place names. symbol-macrolet in particular is a value-place form: its names must not shadow function-call heads, and its expansion forms resolve in the outer environment while its body is shadowed by the symbol-macro names.</value_bindings>
    <callable_bindings>flet, labels, macrolet, compiler-macrolet introduce local callable bindings that DO shadow an outer callable rename target within their body scope.</callable_bindings>
  </namespace_rule>

  <scope_rule name="init_vs_body_evaluation_environment">
    <statement>For each binding form, know which sub-forms see the outer environment and which see the bound names — this determines which occurrences a rename may rewrite.</statement>
    <items>
      <item>let / prog: init forms are evaluated in the outer scope (parallel); the body sees all bound names.</item>
      <item>let* / do* / prog*: sequential — each init sees earlier bound names but not its own; the body sees all.</item>
      <item>do: init forms are outer-scope; step forms, the end-test, and the body see all iteration variables.</item>
      <item>dolist / dotimes: the list/count source is outer-scope; the iteration variable shadows the target in the optional result form and body.</item>
      <item>symbol-macrolet: expansion forms resolve in the outer environment; only body references count as in-scope references to the symbol macro (relevant to unused-binding and inline analyses). In quasiquote, preserve comma/comma-at prefixes and rename only the unquoted symbol-macro references.</item>
      <item>with-slots / with-accessors: the instance expression is outer-scope; slot/accessor names in specs are not value references; body references are the shadowed ones. Renaming a bare with-slots spec must expand it from slot to (new-name slot) to preserve the slot-name mapping.</item>
      <item>handler-bind / restart-bind: the spec head (condition type / restart name) and restart-bind option keywords (:report, :test, :interactive) are designators, not value references. A lambda in a handler function or restart-option position introduces parameters that shadow only that lambda's own body.</item>
    </items>
  </scope_rule>

  <boundary_rule name="reader_prefix_and_quasiquote">
    <statement>Treat quote and quasiquote reader prefixes as reference-literal boundaries, not merely (quote ...)/(list ...) heads. Under quote, symbols are data and must not be rewritten. Under quasiquote, data is protected but unquote (, / ,@) re-enters evaluation, so a rename must thread quasiquote depth: rewrite only unquoted references and preserve the , / ,@ prefixes on the replacement.</statement>
    <note>#'symbol and (function symbol) are callable designators: rename them with the target in executable context, but skip them inside quasiquoted data unless an unquote re-enters evaluation.</note>
  </boundary_rule>

  <special_rule name="define_modify_macro_implicit_place">
    <statement>define-modify-macro has an implicit leading place argument that precedes the user lambda-list parameters. Any call-site rewrite (add/remove/move/reorder an argument) must offset user arguments by one so the place argument is preserved.</statement>
  </special_rule>
</binding_form_scope_reference>

<lambda_list_and_quasiquote_traps>
  <description>
    Implementation-detail traps encountered when a macro system parses and re-emits macro
    lambda lists, and when tests construct expected expansions by hand.
  </description>

  <trap name="whole_binding_shape">
    <statement>&amp;whole binds the entire original form. If a destructuring routine stores its result as an alist consumed via (cdr (assoc ...)), the &amp;whole entry must be a dotted pair (cons whole arg) — but that dotted shape is not itself a valid let binding, so normalize dotted/improper entries to (var value) at the code-generation boundary before emitting let/let*.</statement>
  </trap>

  <trap name="environment_extraction_and_normalization">
    <statement>Handling &amp;environment requires two coordinated steps: extract the environment symbol and wrap the expander body so it is bound, AND remove &amp;environment from the lambda list used to generate the call-site argument bindings, or the environment symbol is overwritten by an argument binding. Because some expansion paths call a local expander with a nil environment, normalize a nil environment to a non-nil sentinel when the macro requests &amp;environment.</statement>
  </trap>

  <trap name="quasiquote_in_expected_values">
    <statement>When a test builds the expected expansion, do not construct it with '(,name ...) inside a backquote — nested quote/backquote can preserve the comma as literal data instead of interpolating the gensym. Build the expected structure explicitly with list/cons.</statement>
  </trap>

  <trap name="reuse_lambda_binding_generation">
    <statement>defsetf long form and similar accessor-defining macros should reuse the same lambda-list binding generation as macro lambda lists, so optional/rest/key/aux structure is supported uniformly; bind the store variable separately and emit the body inside a let/let* wrapper.</statement>
  </trap>

  <trap name="quote_requested_data_into_helpers">
    <statement>When an expander forwards user-supplied structural options into a runtime helper (e.g. the symbol-type list of with-package-iterator: :internal / :external / :inherited), pass them as quoted data, not as a live form — otherwise (:internal :external :inherited) is emitted as a runtime call.</statement>
  </trap>

  <reference name="with_standard_io_syntax_bindings">
    <statement>A with-standard-io-syntax-style macro must rebind the full ANSI set, not a subset. Beyond the read/print numeric and escape variables it must bind *readtable* to a standard readtable (copy-readtable nil), *print-pprint-dispatch* to a standard dispatch (copy-pprint-dispatch nil), and *print-readably* to t. Omitting *readtable* or the pprint-dispatch/readably bindings is a common incomplete-reimplementation bug.</statement>
    <scope>The 21-variable binding set is ANSI-specified (CLHS with-standard-io-syntax).</scope>
  </reference>
</lambda_list_and_quasiquote_traps>

<dialect_notes>
  <dialect name="common_lisp">
    <item>Phase separation: eval-when (:compile-toplevel :load-toplevel :execute)</item>
    <item>Hygiene primitive: gensym (or alexandria:once-only / alexandria:with-gensyms for batches)</item>
    <item>Editor indentation: derived automatically from &amp;body in the lambda list by SLY/SLIME; no explicit declare needed</item>
    <item>Verification: macroexpand-1 (single step) vs macroexpand (fully expand); use sly-macroexpand-1 / sly-macroexpand-all interactively</item>
    <item>Debug tooling: sly-macrostep (macrostep.el via SLY) for interactive step-through expansion</item>
  </dialect>
  <dialect name="emacs_lisp">
    <item>Phase separation: eval-and-compile; requires -*- lexical-binding: t; -*- at file top</item>
    <item>Hygiene primitive: cl-gensym (from cl-lib) or make-symbol; gensym alone has no namespacing convention — always pass a descriptive prefix</item>
    <item>Editor indentation/debugging: explicit (declare (indent N) (debug SPEC)) as the first form in the macro body — this is not automatic</item>
    <item>Verification: macroexpand-1, macroexpand-all (from cl-macs); interactively via pp-macroexpand-last-sexp or macrostep.el</item>
    <item>Byte-compilation: compile-time helpers not wrapped in eval-and-compile fail silently to a runtime call instead of erroring — always verify with `emacs --batch -f batch-byte-compile`</item>
  </dialect>
</dialect_notes>

<decision_tree name="macro_vs_function">
  <question>Does this abstraction need to control evaluation (timing/order/whether an argument is evaluated at all), capture syntax, or generate code shaped by its arguments?</question>
  <if_yes>Use a macro — but keep it a thin pipeline hook over pure parse/analyze/emit functions</if_yes>
  <if_no>Use an ordinary function or higher-order function; a macro here only adds hygiene risk and debugging friction for no semantic benefit</if_no>
</decision_tree>

<best_practices>
  <practice priority="critical">Reach for the canonical_technique_library first (once-only, anaphora, defmacro!/g!-o!-symbols, generalized variables, CPS macros) instead of inventing ad hoc gensym/evaluation-order handling</practice>
  <practice priority="critical">Design the input S-expression and hand-write its ideal expansion before writing any macro code (Phase 1 proof-first)</practice>
  <practice priority="critical">Keep defmacro bodies to a 1-3 line pipeline call; all logic lives in testable functions</practice>
  <practice priority="critical">Gensym every internal temporary; document every intentional anaphoric capture</practice>
  <practice priority="critical">Validate DSL clause shape in the parser and signal errors before the emitter runs</practice>
  <practice priority="high">Bind every user-supplied argument form exactly once, in left-to-right order, before referencing it in the expansion</practice>
  <practice priority="high">Wrap compile-time helpers in eval-when (CL) or eval-and-compile (Elisp)</practice>
  <practice priority="high">Use &amp;body in CL lambda lists for trailing body arguments; use (declare (indent ...) (debug ...)) in Elisp macros</practice>
  <practice priority="medium">Verify every non-trivial macro's expansion with macroexpand-1/macroexpand against the Phase-1 proof before considering it complete</practice>
  <practice priority="medium">Push static analysis (liveness, dependency graphs, non-determinism enumeration) into the analyzer stage, never into emitted runtime code</practice>
  <practice priority="high">Splice function designators (symbols), never #'fn function objects, into macro output so expansions remain FASL-dumpable</practice>
  <practice priority="high">Before mechanically renaming/extracting across a binding form, confirm its namespace (value vs callable) and which sub-forms it shadows</practice>
  <practice priority="medium">Thread quasiquote depth in code-walking rewrites; preserve , / ,@ prefixes and leave quoted data untouched</practice>
  <practice priority="medium">Reimplement standard binding macros (e.g. with-standard-io-syntax) against the full ANSI variable set, not a convenient subset</practice>
</best_practices>

<anti_patterns>
  <avoid name="backquote_monolith">
    <description>Three or more nested backquote/comma levels directly inside defmacro</description>
    <instead>Extract a pure emit function that builds and returns the S-expression; keep defmacro to a pipeline call</instead>
  </avoid>

  <avoid name="multiple_evaluation">
    <description>Referencing a user-supplied argument form more than once in the expansion (e.g. `(if ,test (foo ,test) (bar ,test))`)</description>
    <instead>Bind it once via gensym (the once-only idiom) and reference the bound symbol thereafter</instead>
  </avoid>

  <avoid name="runtime_eval_for_dsl_logic">
    <description>Generating code that calls eval, or deferring static-analyzable decisions (branch selection, lifetime checks) to runtime</description>
    <instead>Resolve them in the analyzer stage at macro-expansion time; emit only the already-decided, flattened result</instead>
  </avoid>

  <avoid name="unhygienic_temporaries">
    <description>Introducing a plain symbol like `result` or `tmp` in a macro expansion without gensym</description>
    <instead>gensym (CL) / cl-gensym or make-symbol (Elisp) for every macro-introduced temporary; reserve plain symbols for documented, intentional anaphora</instead>
  </avoid>

  <avoid name="runtime_only_errors">
    <description>Letting malformed DSL input pass through the parser unchecked and fail deep inside generated runtime code</description>
    <instead>Validate shape in the parser and error at macro-expansion time, naming the offending clause</instead>
  </avoid>

  <avoid name="missing_dx_metadata">
    <description>Shipping a multi-clause DSL macro that mis-indents in the editor or can't be stepped in the debugger</description>
    <instead>&amp;body in CL lambda lists; (declare (indent ...) (debug ...)) as the first form in Elisp macros</instead>
  </avoid>

  <avoid name="reader_macro_overuse">
    <description>Reaching for set-macro-character / custom reader syntax to simplify a DSL</description>
    <instead>Prefer ordinary defmacro; reader macros are global, non-composable, and break tooling that doesn't know about them. Reserve for narrow, well-documented cases.</instead>
  </avoid>

  <avoid name="function_object_in_expansion">
    <description>Splicing a captured #'fn function object into macro output (e.g. as a default callback), which the file compiler cannot externalize and fails to dump to a FASL</description>
    <instead>Splice a function-designator symbol and funcall it, or resolve #'fn at the call site; keep the expansion source-serializable</instead>
  </avoid>

  <avoid name="uniform_binding_assumption">
    <description>Treating every (name value) shape as one uniform "binding" during a mechanical rewrite, ignoring value-vs-callable namespace and per-form shadowing rules</description>
    <instead>Consult the binding_form_scope_reference: check the namespace of each name and which sub-forms it shadows before renaming, extracting, or inlining across it</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Every non-trivial macro must have a hand-written Phase-1 "ideal expansion" it is verified against via macroexpand-1/macroexpand</rule>
  <rule>defmacro bodies must be thin pipeline calls (parse → analyze → emit); no inline multi-level backquote logic</rule>
  <rule>Every macro-introduced symbol not documented as intentional anaphora must be gensym'd</rule>
  <rule>Every user-supplied argument form must be evaluated exactly once, in left-to-right order, in the expansion</rule>
  <rule>Malformed DSL input must fail at macro-expansion time with a clause-naming error, never silently at runtime</rule>
</rules>

<rules priority="standard">
  <rule>Compile-time helper functions must be wrapped in eval-when (CL) or eval-and-compile (Elisp)</rule>
  <rule>Use &amp;body for CL body arguments; use (declare (indent ...) (debug ...)) for Elisp macros</rule>
  <rule>Require lexical-binding: t in any Elisp file defining macros</rule>
  <rule>Avoid reader macros unless no defmacro-based design is viable</rule>
</rules>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Missing &amp;body/indent metadata causing cosmetic mis-indentation</example>
    <example severity="medium">Multiple evaluation of a side-effecting argument form</example>
    <example severity="high">Variable capture bug from a missing gensym, silently breaking a caller</example>
    <example severity="critical">Compile-time helper not phase-separated, causing macroexpansion to fail only in fresh/cross-compiled environments</example>
  </examples>
</error_escalation>

<constraints>
  <must>Prove the design with a hand-written ideal expansion before implementing (Phase 1)</must>
  <must>Keep defmacro as a thin pipeline hook over pure parse/analyze/emit functions</must>
  <must>Gensym all macro-introduced symbols except documented anaphora</must>
  <must>Preserve left-to-right, single evaluation of user-supplied forms</must>
  <must>Fail malformed DSL input at macro-expansion time with a named clause error</must>
  <avoid>Nested backquote/comma beyond two levels inside defmacro</avoid>
  <avoid>eval or other runtime resolution of statically-decidable DSL structure</avoid>
  <avoid>Reader macros as a default tool</avoid>
</constraints>

<references>
  <book title="On Lisp" author="Paul Graham" year="1993">
    <chapter name="Macro Basics">Fundamentals of defmacro, backquote, mkstr/symb/flatten utilities</chapter>
    <chapter name="Generalized Variables">get-setf-expansion-based place abstraction (_f, toggle)</chapter>
    <chapter name="Utility of Once-Only">The textbook once-only implementation</chapter>
    <chapter name="Anaphoric Macros">aif, awhen, aand, alambda and intentional capture</chapter>
    <chapter name="Macros Returning Functions">Compile-time selection among generated closures</chapter>
    <chapter name="Macros as Programs">Macro-defining-macros; code generation as ordinary programming</chapter>
    <chapter name="Continuations">CPS macros: =defun, =lambda, =bind, =values</chapter>
  </book>
  <book title="Let Over Lambda" author="Doug Hoyte" year="2008">
    <chapter name="Macro Basics">g!-symbols / o!-symbols, defmacro/g!, defmacro! as a declarative once-only + gensym convention</chapter>
    <chapter name="Read Macros">Reader-syntax extension; treat as an opt-in, narrowly-scoped technique</chapter>
    <chapter name="Duality of Syntax">Pairing defmacro with define-symbol-macro for a name usable both bare and applied</chapter>
    <chapter name="Pandoric Macros">Closures with externally accessible named slots via dlambda-based dispatch</chapter>
  </book>
  <usage_note>Use these as the source of truth for "is this macro written correctly" — when in doubt about a technique's exact semantics, prefer consulting the book or a tested library (e.g. alexandria, let-over-lambda) over reproducing intricate code (once-only, pandoric) from memory.</usage_note>
</references>

<related_skills>
  <skill name="common-lisp-ecosystem">CLOS, ASDF, condition-system, and general Common Lisp fundamentals underlying macro design</skill>
  <skill name="emacs-ecosystem">Emacs Lisp fundamentals, use-package, and editor integration this skill's DX laws build on</skill>
  <skill name="sbcl-usage">macroexpand/trace/inspect workflows for verifying and debugging expansions at runtime</skill>
  <skill name="investigation-patterns">Evidence-driven debugging methodology for tracking down capture/evaluation-order bugs</skill>
  <skill name="serena-usage">Symbol navigation across macro definitions and their call sites</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate existing macro definitions and call sites in this skill domain</agent>
  <agent name="quality-assurance">Review macro hygiene, evaluation-order, and diagnostics quality against this skill's laws</agent>
  <agent name="code-quality">Flag backquote-monolith macros and suggest parser/analyzer/emitter decomposition</agent>
</related_agents>
