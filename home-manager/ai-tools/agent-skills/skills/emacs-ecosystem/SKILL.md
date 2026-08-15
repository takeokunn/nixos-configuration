---
name: emacs-ecosystem
description: Use for Emacs Lisp, init.el, use-package, and Emacs runtime hazards such as hook ordering, condition-case versus quit, overlays versus text properties, buffer-local state, keymap precedence, and subprocess handling. For org-mode, see org-ecosystem.
version: 3.0.0
---

Emacs mechanisms whose documented behaviour differs from what their names suggest. Elisp syntax, `defun`,
`let`, `pcase`, and the package catalogue are assumed; **every rule below is a place where correct-looking code
is silently wrong.**

## Baseline choices

Emacs 30.x is the baseline; defer to the active package set for the exact point release.

- `lexical-binding: t` in the file header, always.
- **eglot** for LSP — built-in since 29 and the recommended default.
- **Tree-sitter `*-ts-mode`** variants where available (29+, improved in 30.2).
- The modern completion stack: vertico, orderless, marginalia, consult, corfu, cape.
- `use-package` is built-in since 29; prefer `:custom` over `setq`, `:hook` over `add-hook`, and defer with
  `:defer` / `:commands` / `:hook`.
- `#'function-name` for function references, so the byte-compiler can warn.
- Namespace every symbol with the package prefix; `provide` at end of file.

## Tree-sitter modes: `fboundp` is insufficient

On 29.1+ the `*-ts-mode` functions are **autoloaded built-ins**, so their symbols are always `fboundp`
regardless of whether the grammar shared library is installed. `(fboundp 'json-ts-mode)` returns non-nil even
when activating the mode fails with "language grammar for LANG is unavailable". **`fboundp` answers "is this
mode defined?", not "can this mode run?"**

Use `treesit-language-available-p`, which takes a *language* symbol rather than a mode symbol — so a
mode-to-language mapping is required, because the language name is not always the mode prefix
(`js-ts-mode` → `javascript`). `treesit-ready-p` is the higher-level form that also emits the standard
user-facing warning.

```elisp
(defvar my-ts-mode-language-alist
  '((json-ts-mode   . json)
    (js-ts-mode     . javascript)
    (python-ts-mode . python)))

(defun my-ts-mode-available-p (mode)
  (when-let ((lang (alist-get mode my-ts-mode-language-alist)))
    (and (fboundp mode) (treesit-language-available-p lang))))
```

When the mapping is maintained separately from the dispatch list, **the two tables drift**: a new ts-mode is
added to the dispatch list, its language is never registered, the availability check silently returns nil, and
the mode is never selected. Assert the sync in a unit test so a silent runtime fallthrough becomes a
deterministic failure.

## Keymaps

### Asserting contents

`lookup-key` and `where-is-internal` are **lossy for test assertions**. For a key sequence that is only a
prefix of a longer binding, `lookup-key` returns an integer — the number of events consumed — which is easy to
misread as "bound to something". Bindings inside a composed keymap or a nested prefix keymap can also be missed
depending on how the lookup is issued.

For composed or prefix-heavy keymaps, walk the raw structure with `map-keymap`, descending into nested keymaps
yourself:

```elisp
(defun my-keymap-commands (keymap)
  (let (acc)
    (map-keymap
     (lambda (_event binding)
       (cond ((keymapp binding) (setq acc (append acc (my-keymap-commands binding))))
             ((commandp binding) (push binding acc))))
     keymap)
    acc))
```

**Named function keys need angle brackets.** `(kbd "<left>")` returns `[left]`, which is what keymaps store.
`(kbd "[left]")` returns the six literal characters — and because `[` is itself a self-inserting prefix,
`lookup-key` can return an integer partial-match that disguises the mistake.

Code dispatching via `call-interactively` requires the target to satisfy `commandp`. A test replacing a real
command with a plain lambda gets `(wrong-type-argument commandp ...)`; give mock lambdas an `(interactive)`
form.

A mode's keymap and its entry point are frequently in **different files** — the keymap via `defvar-keymap` in
the main feature file, helper commands in a sibling. A test requiring only the helper feature observes an
unbound or empty keymap and asserts against nothing. Require the feature that actually defines the keymap.

### Precedence

Emacs consults keymaps in a fixed order, and **the major-mode map sits near the bottom**: overriding and
terminal-local maps, then `emulation-mode-map-alists`, then `minor-mode-overriding-map-alist`, then
`minor-mode-map-alist`, then the local (major-mode) map, then global. Any package installing bindings through
the minor-mode layers — every modal editing package does — **shadows a major-mode map unconditionally**,
however that map is populated. A feature that must see raw keys cannot achieve it with a major-mode keymap.

Install bindings that must outrank other minor modes through `emulation-mode-map-alists`, and **re-assert your
entry at the head of that list every time your mode is enabled**, not once at load time, because a package
enabled later pushes its own entry ahead of yours. `minor-mode-overriding-map-alist` is the buffer-local
equivalent and the smaller hammer.

**Never mutate a shared keymap for local state.** The map created by `define-minor-mode` or bound by
`defvar-keymap` is a single global object reached by every buffer using that mode. Mutating it to reflect
buffer-local state — most commonly `set-keymap-parent` to select an input mode or layout — **changes the
effective bindings of every other buffer using that mode**, and the symptom appears far from the cause.
Compose per buffer instead:

```elisp
(use-local-map (make-composed-keymap buffer-specific-map shared-mode-map))
```

Composition allocates a fresh object per buffer and leaves the shared map untouched. This is the keymap
instance of a wider rule — **buffer-local state belongs in an object the buffer owns** — which also governs
overlays versus text properties below.

## Verification lies

### A stale `.elc` masks the source

With both `LIB.el` and `LIB.elc` on the same load-path entry, `load` uses the **`.elc` even when the `.el` is
newer**, emitting only a warning that is easy to miss in batch output. A `.eln` beats `.elc` beats `.el`. So a
stale `.elc` hides a source fix: a passing test does not prove the patch works, and a failing test may not
reflect current source.

```sh
find . -name '*.elc' -delete
emacs -Q --batch --eval '(setq load-prefer-newer t)' \
  -L . -L test -l ert -l my-feature -l my-feature-test \
  -f ert-run-tests-batch-and-exit
```

Better still, byte-compile to a temporary destination so verification never leaves `.elc` in the tree. **If a
result contradicts a source change, suspect stale bytecode first.**

### Candidate order beats timestamp

`load-prefer-newer` chooses between `.el` and `.elc` **within a single directory. It says nothing about which
directory is consulted first.** When the same feature exists in both the worktree and an installed location — a
Nix site-lisp path, a `package.el` tree — the first matching candidate in `load-path` wins regardless of
modification time, so an installed `.elc` shadows the source you just edited even with `load-prefer-newer` set.
In one investigation, **ten of fifteen apparent test failures were this loader false negative** rather than a
regression.

Place every worktree source directory ahead of any installed location explicitly, then *prove* provenance:
`(symbol-file 'my-feature-function)` reports where a definition was actually loaded from, and
`(locate-library "my-feature")` reports which candidate the loader would pick. Deleting `.elc` and setting
`load-prefer-newer` does not settle the multi-candidate case.

### Compilation removes the seam you stubbed

`cl-letf` on a function cell intercepts only calls that go through that cell, and two byte-compiler behaviours
route around it **silently**. The compiler lowers many primitives to dedicated opcodes: a compiled caller of
`set`, `setcar`, `setcdr`, `car`, `cdr`, or `aref` emits the opcode and never consults the function cell. And a
`defsubst` is inlined into its compiled callers, so a stub on its own symbol is never consulted. In both cases
**the stub installs without complaint and the real implementation runs** — a green result for an injection that
never happened, or a red one attributed to the wrong cause.

Inject faults at a named, non-inlinable boundary you own: put the primitive mutation behind an ordinary
`defun`, or expose an injectable hook variable. **Never promote a function tests rely on as a seam to
`defsubst` for speed.** Assert the injected fault actually fired — a counter, a sentinel — rather than only the
downstream outcome, and run that assertion in both the interpreted and byte-compiled configurations, because
the interpreted run is the one where the stub does work.

### Other compilation traps

**Cross-file macros.** Macros expand at compile time *in the calling file*. Recompiling only the
macro-defining file leaves the call site carrying an old expansion, failing with `invalid-function` or calling
a stale expansion. **Treat a macro's callers as part of its compilation unit.**

**Batch load-path completeness.** A batch run fails at load time, before any test executes, if a required
feature's directory is absent. Transitive requires matter: pass one `-L` per directory contributing a required
feature, including test-support helpers.

**Macro expansion shape.** A `defun`-generating macro can expand to `(defalias NAME #'(lambda ...))` rather
than a literal `defun`, and `macroexpand` is a top-level contract only — it may expand the outermost macro into
a `progn` while leaving nested calls inside `let` untouched. Normalize to a canonical shape before asserting.

**Warning suppression has specific correct forms.** Under `byte-compile-error-on-warn`, a runtime
`(boundp 'other-package-var)` guard does **not** suppress the free-variable warning, because the compiler sees
the direct reference inside the guarded branch whether or not it runs. Read an optional late-bound global
through `(symbol-value 'other-package-var)` after the `boundp` check — but use `symbol-value` only for optional
*data access*; when the variable is a genuine cross-module mutation contract, declare it with a value-less
`(defvar other-package-var)` so the contract stays visible. On Emacs 29 a many-slot `cl-defstruct` generates a
constructor docstring exceeding the width limit, and `with-suppressed-warnings ((docstrings) ...)` does not
suppress it there — wrap only the offending form in `with-no-warnings`.

### Dynamic modules

**The loaded artifact is usually not the built artifact.** A module's load path is typically hardcoded to an
install prefix rather than the build output, so a successful build does not imply the running Emacs sees the
new code. A stale installed library keeps executing code deleted from the source, surfacing as doubled effects,
rendering artifacts, or empty output — **with no error anywhere.** Compare modification times of built and
installed artifacts as the *first* check, not the last.

**On macOS, copying a dynamic library invalidates its ad-hoc signature.** AMFI refuses the load and the kernel
kills the process: Emacs dies with **SIGKILL, exit 137, no Lisp error, no backtrace, nothing naming the
module.** Re-sign after any copy: `codesign --force --sign - /path/to/module.dylib`. Treat status 137 on module
load as a signature problem until proven otherwise.

## Autoload cookies

`loaddefs-generate` copies the form after a `;;;###autoload` cookie **verbatim** into loaddefs, *except* for a
fixed set it converts into safe `autoload` calls: `defun`, `defmacro`, `cl-defun`, `cl-defmacro`,
`define-overloadable-function`. Put a cookie before anything else — a custom macro invocation, a side-effecting
top-level form — and **the whole form is copied raw and executed at load time**, running side effects
unconditionally and possibly failing if the macro is not yet defined.

Place a bare cookie only before a recognized definition form. To autoload a name produced by a custom macro,
write the explicit form yourself:

```elisp
;;;###autoload (autoload 'my-command "my-file")
```

**Installing from source compiles your tests.** With `package-vc` or `use-package :vc`, the package manager may
traverse and byte-compile `test/`. On Emacs 30.x, `.elpaignore` and `:ignored-files ("test/")` do not reliably
stop `package--compile` descending into tests, so compilation fails on files requiring unavailable helpers. A
repo-side approach that has worked: `test/.dir-locals.el` binding `((emacs-lisp-mode . ((no-byte-compile . t))))`.
Verify against your target version rather than assuming.

## Lifecycle and error boundaries

The language-neutral rules for ownership, atomicity, and rollback ordering belong to
[state-transactions](../state-transactions/SKILL.md); these are the Emacs mechanisms those rules must be built
on.

### `quit` is not an error

`C-g` signals `quit`, and **`quit` is not a subtype of `error`.** A `condition-case` naming only `error` does
not run for a user interrupt: the non-local exit passes straight through and skips whatever the handler was
going to do. Because `C-g` is a routine user action, this supposedly rare path is common — and an adversarially
injected `quit` walks out of a cleanup helper instead of making it fail closed.

Every cleanup, teardown, or fault-isolating boundary handles **both `error` and `quit`**: capture the first
condition, keep running every remaining step, then re-signal the captured condition unchanged — same symbol,
same payload — so callers observe the original failure rather than a cleanup artefact.

```elisp
(defun my-run-cleanup-steps (steps)
  (let ((inhibit-quit t) primary)
    (dolist (step steps)
      (condition-case err
          (funcall step)
        ((error quit) (unless primary (setq primary err)))))
    (when primary (signal (car primary) (cdr primary)))))
```

Bind `inhibit-quit` around the whole restoration when a second pending `C-g` could interrupt it.

### A major-mode change erases buffer-local state

Changing a buffer's major mode calls `kill-all-local-variables`, which runs `change-major-mode-hook` **first**
and erases buffer-local bindings afterwards. A buffer-local minor mode recording its resources — overlays,
markers, timers, processes, registry entries — in buffer-local variables **loses the handle the moment the user
types `M-x fundamental-mode`.** Its disable command never runs, `kill-buffer-hook` never runs because the
buffer is still alive, and a global disable command can no longer discover the orphaned resources. Two
independent packages have hit this the same way.

Register a buffer-local `change-major-mode-hook` entry calling one shared teardown — the same one the disable
command and `kill-buffer-hook` call — so resources are released while local state still exists. Make it
idempotent, because all three paths can fire for the same buffer.

```elisp
(define-minor-mode my-feature-mode
  "Toggle My Feature in the current buffer."
  :lighter " MyF"
  (if my-feature-mode
      (progn
        (add-hook 'change-major-mode-hook #'my-feature--teardown nil t)
        (add-hook 'kill-buffer-hook #'my-feature--teardown nil t))
    (remove-hook 'change-major-mode-hook #'my-feature--teardown t)
    (remove-hook 'kill-buffer-hook #'my-feature--teardown t)
    (my-feature--teardown)))
```

### Isolate hook observers with `run-hook-wrapped`

**A hook variable is not a plain list.** Its buffer-local value may contain the sentinel `t`, which splices the
global value in at that position. Two mistakes follow: wrapping the aggregate `run-hooks` call in a
`condition-case` isolates the hook *as a whole*, so the first observer that signals prevents every later one
from running and can skip required follow-up such as cache invalidation; and hand-rolling the traversal with
`dolist` **loses the `t` splice and the local/global merge entirely**, so buffer-local or global observers
silently stop being called.

`run-hook-wrapped` performs the standard traversal while calling a wrapper around each individual function. Put
the `condition-case` inside the wrapper, return nil so traversal continues, demote only the errors the hook
documents as suppressible, and **never swallow `quit`.**

```elisp
(defun my-feature--call-observer (fn &rest args)
  (condition-case err
      (apply fn args)
    (error (message "observer %S failed: %S" fn err)))
  nil)

(run-hook-wrapped 'my-feature-after-change-hook #'my-feature--call-observer buffer)
```

### Variable watchers are not a stack

`add-variable-watcher` **prepends** while `remove-variable-watcher` deletes destructively, so replaying a saved
list front to back **reverses the effective order**. Batch-verified on 30.2 and 31, and checked against the 29
implementation. Snapshot `(copy-sequence (get-variable-watchers SYMBOL))` together with the bound-or-unbound
state and the value; on restore, remove every watcher currently present including any the body installed,
restore the value or unbound state, then re-add the saved watchers **in reverse order**.

Installing an anonymous lambda as a watcher makes it unremovable by identity, so every module reload
accumulates another copy — the same trap as a lambda in a hook, with no `remove-hook`-style escape. Always
install a stable named function and remove it before re-adding.

## Buffer state and editing contracts

### Overlays are owned; text properties are not

Text properties live in the buffer text, so they are **shared with every indirect buffer** made from the same
base: applying `read-only` or `cursor-intangible` in an indirect buffer makes the base read-only too, even
though the buffer-local variable tracking that decoration is not shared and the base has no record of it. Text
properties also have **no notion of an owner** — two features writing the same property over overlapping ranges
are indistinguishable — so a feature that captures the previous value and restores it later silently discards
whatever a concurrent writer added. That has been reproduced as real state loss.

Overlays are the opposite: each belongs to exactly one buffer, is not shared with indirect buffers, and is a
first-class object you delete by identity. Use overlays for decoration your feature owns and must remove
exactly. Reserve text properties for attributes that genuinely belong to the text. **Never implement ownership
as "record the old value of a shared property and put it back later".**

### Overlay hooks have endpoint gaps

An overlay's `modification-hooks`, `insert-in-front-hooks`, and `insert-behind-hooks` **do not fire for
insertions at absolute buffer endpoints** outside the overlay, and a zero-width overlay inherits the same hole
because there is no interior for a change to land in. A guard built only from overlay hooks therefore permits
edits at `point-min` and `point-max`, and cached marker bounds refreshed after a boundary edit drift out of
sync with overlays that were never repositioned.

Back an overlay-based guard with buffer-local `before-change-functions` and `after-change-functions`, keep the
authoritative bounds in **markers**, and resynchronize the overlays from those markers after each change.

### Region and atomic-change contracts

`(interactive "r")` supplies point and mark **whenever a mark exists at all** — it does not require the region
to be active — so a command declared with `"r"` will cheerfully transform a stale range the user does not
believe is selected. Guard with an explicit `use-region-p` check.

`atomic-change-group` reverts buffer *text* on a non-local exit but **does not restore point or mark**, so an
error or `C-g` partway through leaves the cursor and region somewhere the user did not put them. For a
destructive region edit: compute the replacement before touching the buffer, save point and mark including the
region's direction, perform the delete-and-insert inside `atomic-change-group`, and restore point and mark
yourself on **both** the error and the quit path.

### `insert` is not `self-insert-command`

A great deal of behaviour hangs off `post-self-insert-hook` — `electric-pair-mode`, `electric-indent-mode`,
auto-fill, abbrev expansion, many minor modes. **`insert` does not run that hook.** Code typing characters on
the user's behalf therefore disables all of it silently, with no error and no visible cause.

```elisp
(defun my-insert-char (char n)
  (let ((last-command-event char))
    (self-insert-command n char)))
```

`self-insert-command` and its observers read the character from `last-command-event`, not from an argument.

## Data structure hazards

**Mutating a stored key orphans the entry.** A hash table computes a key's hash once, at insertion. Under
`equal` the key is compared structurally, so a mutable key the caller destructively modifies after `puthash`
**no longer hashes to the bucket its entry sits in.** The entry becomes unreachable under both old and new key
while the physical entry remains — logical and physical size diverge, and repeated put-then-mutate grows the
table without bound. A reproducer with capacity one and twenty iterations ended with logical size one and
physical size twenty.

A public API accepting a caller-owned mutable value as a key must **detach it before storing**:
`copy-sequence` for a string or vector, a deep copy for a structured key. Where keys may be cyclic, register a
dedicated test with `define-hash-table-test` backed by `sxhash-equal` and a cycle-safe comparison — on Emacs 30
the built-in `equal` test can signal `circular-list` on cyclic cons keys. **Unbounded growth of a table whose
logical size stays small is the diagnostic signature.**

**Recursion bounded by collection length.** Elisp recursion is bounded by `max-lisp-eval-depth`, and that
ceiling is reached by ordinary data. A recursive walk whose depth tracks the **length of a collection** — the
characters of a long key, the entries of a bucket, the elements of a list being copied — passes every small
test and fails once real input arrives: a scale-dependent failure with no reproducible trigger. Recursion
bounded by *structural nesting* is a different case and is generally fine.

Rewrite length-tracking recursion as an explicit loop over a worklist. To copy a mutable object graph that may
contain cycles and shared structure, use a **two-phase traversal with an `eq`-keyed memo**: first walk
allocating and memoizing one empty shell per mutable object while discovering children, then walk the memo
connecting edges. This preserves cycles and shared identity in O(V+E) at constant call depth.

**An absent stamp is not a comparable value.** A cache validated by "the recorded stamp still equals the
current stamp" degenerates into "always valid" whenever the stamp function returns nil for an absent input. If
the stamp is a sentinel file's mtime from `file-attributes`, a missing sentinel yields nil, the entry stores
nil, and `(equal nil nil)` reports a hit **forever.** The scheme is correct exactly when the sentinel exists —
the case the author tested. Treat an unobtainable stamp as a miss and refuse to store an entry whose validity
token is nil.

## Processes and remote paths

**`with-timeout` does not bound a synchronous call.** It schedules a timer, and timers fire only when Emacs
reaches its event loop. A synchronous `call-process` never returns to the event loop, so Emacs simply stays
blocked — **the timeout appears to work only because the helper usually returns quickly.** The same call
accumulates unbounded stdout and returns an exit status easy to discard by accident.

Run any helper that could hang under `make-process`, with one decrementing wait budget shared by startup and
draining, a byte-counted cap on accumulated stdout, a separate non-accumulating destination for stderr, and an
explicit check that the exit status was zero before believing the output.

**The exit sentinel can precede pending output.** A sentinel reporting termination does not mean output has
been delivered — output the child already wrote may still be pending in the filter. A stress probe of a helper
writing 32 bytes **lost stdout in 12 of 20 runs**, and a test against it fails intermittently in a full suite
while passing alone. After the sentinel fires, keep draining with `accept-process-output` until the process is
no longer live *and* no further output arrives. Read an intermittent truncation that appears only under load as
a drain race, not as flakiness to retry away.

**The wait budget is shared by startup and drain.** `accept-process-output`'s TIMEOUT bounds a single slice, so
any aggregate bound is the caller's to maintain — as a decrementing budget, never an absolute deadline
recomputed from `float-time`.

```elisp
(defun my-drain (proc budget)
  (unless (and (numberp budget) (> budget 0) (< budget 1.0e+INF))
    (error "Invalid wait budget: %S" budget))
  (let ((remaining budget) (iterations 0))
    (while (and (process-live-p proc)
                (> remaining 0)
                (< (setq iterations (1+ iterations)) 10000))
      (let ((slice (min 0.05 remaining)))
        (accept-process-output proc slice nil t)   ; JUST-THIS-ONE: established process
        (setq remaining (- remaining slice))))))
```

Validate the budget and reject degenerate values — non-numeric, NaN, non-finite, zero, negative — rather than
clamping silently, and cap the iteration count so a slice returning immediately cannot spin.

**JUST-THIS-ONE is asymmetric.** It suppresses processing of other processes' events, which is what you want
while draining a response body — it stops unrelated filters running re-entrantly mid-read. It is **wrong**
while waiting for a `:nowait` connection to be established, because the connection-completion event goes
through the same machinery: pinning attention to that one process can prevent Emacs ever observing that it
connected. Leave it nil when awaiting establishment; pass it non-nil when reading from an established process.

**Process-tree cleanup needs identity.** A PID is not an identity — the OS reuses PIDs, so a routine recording
a PID and signalling it later can signal an unrelated process. Descendants make it worse: a helper that forks
and exits immediately leaves a `setsid` child reparented away before any process-table scan sees it,
**reproduced in 10 of 10 attempts.** And a scan bounded for memory reaches its cap as *saturation*, which is
not the same as having enumerated everything — treating the cap as completion silently orphans the remainder.

Identify by the pair of PID and immutable start time, re-verified immediately before and after stopping.
The safe sequence is SIGSTOP, re-verify identity, then SIGKILL only confirmed-stopped identities, so a recycled
PID can never be killed. Close the reparent race with a cryptographically opaque ownership token in the child's
environment, scanned for immediately after launch as well as at cleanup, rather than relying on parentage.
**Report a saturated scan and any signal-delivery failure as incomplete cleanup**; never fold either into a
success. Use a monotonic clock for the deadline, and spool large output to a bounded temporary file.

**Remote paths block; subprocesses do not follow.** Emacs file-name primitives are remote-transparent:
`file-exists-p`, `file-attributes`, and `directory-files` on a remote path go over the network and can block
for a full remote-access timeout, **freezing the UI during what looked like local bookkeeping.** Subprocess
primitives are *not* symmetrically transparent — `shell-command-to-string` and `call-process` run locally
regardless of a remote `default-directory` — so a helper invoked to inspect "the project" inspects the wrong
machine and returns confidently wrong metadata. Two unrelated packages have hit one side each.

Guard bulk filesystem work with `(and (file-exists-p path) (not (file-remote-p path)))`. When a subprocess must
run where the directory lives, use `process-file` and `start-file-process`. When the tool genuinely exists only
locally, detect `file-remote-p` and **decline** rather than returning local results for a remote tree.

## Untrusted input

The general discipline belongs to [trust-boundaries](../trust-boundaries/SKILL.md); these are Emacs APIs that
look inert and are not.

**Opening a file can execute it.** Visiting a file applies its file-local variables, and a file-local `eval:`
entry is code chosen by whoever wrote the file. Any package opening a path derived from outside the user's own
intent — a request handled by an in-Emacs server, a link inside rendered content, an entry from a search index
— **hands arbitrary code execution to whoever controls that file, at the moment of preview.**

```elisp
(let ((enable-local-variables nil)
      (enable-local-eval nil))
  (with-temp-buffer
    (insert-file-contents path)
    (my-render-preview)))
```

**Decode-then-validate is not enough.** `url-unhex-string` is a decoder, not a validator, and it fails open in
two directions: it normalizes some sequences — `%0d%0a` can emerge as spaces — so a check on the decoded string
never sees the CRLF that was in the input, which is the classic header-injection bypass; and it **preserves
malformed triplets like `%ZZ` verbatim** rather than rejecting them. Validate the raw encoded form first,
rejecting percent-encoded control characters and malformed triplets, then decode, then validate again. **The
pre-decode check is the one usually missing.**

**Strings carry text properties into the display.** An Elisp string is not a leaf value: it can carry
`display`, `keymap`, `local-map`, and `face`, any of which change what the user sees and what their keys do. A
string from a data file, a network response, or persisted state can carry any of them — and ordinary operations
preserve them: `concat` keeps them, `format` propagates them from a `%s` operand into its result, and
`propertize` **adds** a property rather than replacing the set, so applying your own outer face does not remove
an attacker-selected `keymap`. Copying does not help: **`copy-tree` does not copy strings**, so a deep copy
still shares the propertized objects inside.

At the presentation boundary, replace untrusted text with a clean copy via `substring-no-properties`, then
apply your own properties to that copy. Leave the original intact where semantic properties matter internally.
**This applies to every untrusted operand passed through the format call, not only the one you were thinking
about.**

## Testability

**Extract output across a macro boundary.** Output produced through `with-help-window` is awkward to test:
assertions have to stub the macro, coupling tests to expansion details. Extract the rendering into a helper
that writes into the current buffer and keep the public command a thin wrapper. Tests then call the helper
inside `with-temp-buffer`. **The seam is where side-effecting presentation meets pure content generation.**

**Isolate feature-local macros** into a sibling `*-macros.el`, with runtime functions in the original file
which `require`s it. This makes load order explicit and shrinks the feature file.

**A family of near-identical commands invites a parallel data table** describing them, which becomes a second
source of truth that drifts. Define the family with a declarative `defmacro` and delete the table once the
macro invocations are its only consumers.

## Contributing upstream

Discover a project's conventions from its own artifacts rather than guessing:

- **Commit style** — CONTRIBUTING plus `git log` for the actual norm.
- **Changelog** — the file and its format, including symbol-quoting conventions.
- **Naming** — the private/public prefix split (`pkg--` vs `pkg-`).
- **Test harness** — how tests run (`make test`, `eask`, `ert-runner`), the file layout, tags used to skip
  environment-specific cases, and the mocking idiom (commonly `cl-letf` on `symbol-function`).
- **Compatibility gate** — the minimum Emacs version, the CI matrix, and whether byte-compilation is treated as
  an error.
- **Formatting commits** — whether whitespace-only changes must be separate and recorded in
  `.git-blame-ignore-revs`.

MELPA submission specifics belong to [melpa-packaging](../melpa-packaging/SKILL.md).

## Related

- [org-ecosystem](../org-ecosystem/SKILL.md) — Org mode, babel, agenda, export
- [lisp-macro](../lisp-macro/SKILL.md) — writing and auditing macros
- [melpa-packaging](../melpa-packaging/SKILL.md) — recipe review and release gates
- [state-transactions](../state-transactions/SKILL.md) — the ownership and rollback rules these mechanisms serve
- [trust-boundaries](../trust-boundaries/SKILL.md) — the general untrusted-input discipline
- [test-integrity](../test-integrity/SKILL.md) — proving which implementation a test actually loaded
- [testing-patterns](../testing-patterns/SKILL.md) — seam design and fixture isolation
