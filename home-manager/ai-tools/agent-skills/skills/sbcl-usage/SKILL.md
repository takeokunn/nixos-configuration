---
name: sbcl-usage
description: Use for SBCL execution and debugging (--script usage, REPL workflows, backtraces, ASDF loading, save-lisp-and-die, profiling, SLY development, terminating an unresponsive SBCL, or sb-thread/sb-cover hazards). Complements common-lisp-ecosystem's runtime operations.
version: 3.0.0
---

Operational guidance for running, debugging, profiling, and shipping SBCL programs: invocation modes, ASDF
loading, the specific ways a compile or load can hang silently, subprocess/thread contracts that differ from
the textbook expectation, and coverage-measurement biases. Complements common-lisp-ecosystem, which covers
CLOS/ASDF/condition-system foundations.

## Invocation modes

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

## ASDF workflow

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

## Debugging workflow

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

## Compile/load hang triage

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

## ASDF plan-layer hang triage

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

## Headless verification harness

A sound, non-interactive harness is a prerequisite for diagnosing the stalls above: if the timeout mechanism
is unsound, a stalled form and a stalled harness are indistinguishable, producing false positives.

**Real subprocess timeout.** The timeout must run in a parent process that keeps the ability to kill the
child. A wrapper that arms an alarm and then `exec`s SBCL replaces itself with SBCL and cancels the alarm: the
timeout never fires, so a hang survives indefinitely and looks like a stalled form. Use fork + wait in the
parent, with the parent owning the alarm and the kill:
```perl
# Perl fork/wait timeout skeleton: the parent keeps the alarm and can signal the child.
# (exec-after-alarm in a single process would silently cancel the alarm.)
perl -e '
  my $pid = fork();
  if ($pid == 0) { setpgrp(0,0); exec @ARGV or die; }
  local $SIG{ALRM} = sub { kill "KILL", -$pid; exit 124; };
  alarm($ENV{TIMEOUT} || 60);
  waitpid($pid, 0);
  exit($? >> 8);
' -- sbcl --script run.lisp
```

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

**Fresh process per unit.** Run each file/test in a fresh SBCL process rather than many units in one
long-lived image. Whole-suite single-process runs have been observed to hang at function/test boundaries even
when each unit passes alone; per-unit fresh processes (chunk size 1) is the stable path. The isolation must be
complete: a bootstrap step that itself calls `compile-file` in the long-lived process defeats a per-file
subprocess strategy.

**Isolate the FASL cache.** Give each run a private, initialized output-translations/cache root before
`asdf:load-system`. Parallel processes sharing an inherited default FASL cache can race and fail with
`"Failed to find the TRUENAME of ...fasl"`. Initialize output translations in the launcher itself, and set a
fresh `HOME`/`XDG_CACHE_HOME` when reproducing in isolation.

**Bound timeout with a kill grace.** When using coreutils `timeout(1)`, always pass a kill grace:
`timeout --foreground -k 10s <limit>s <command>`. Plain `timeout` sends only TERM, and SBCL can remain alive
after its initial termination signal, so a nominally bounded run leaks past the job budget and the escaped
child keeps holding the FASL cache and any ports it opened, same root cause as the interrupt-disabled-region
issue above: the first signal is a request, not a guarantee. Set the grace long enough for an orderly exit
(a few seconds is usually ample) but budget the outer CI step timeout against `limit + grace`, not `limit`.
```bash
# bounded: TERM at the limit, KILL 10s later if the child is still alive
timeout --foreground -k 10s 300s \
  sbcl --no-sysinit --no-userinit --disable-debugger --script run-tests.lisp
```

**Timeout threshold vs. contention.** Distinguish a genuine per-file stall from ambient machine contention.
When many SBCL sessions run concurrently, baseline load latency can exceed a low per-file timeout and report
every file as a timeout. Raise the threshold or reduce concurrency before attributing blame to a single file.

## Form bisect and package preflight

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

## Subprocess process-group contract

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

## Threading contracts

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

## Coverage measurement bias

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

## Performance profiling

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
(declaim (optimize (speed 3) (safety 1) (debug 1)))
(defun hot (x y)
  (declare (type fixnum x y))
  (+ x y))
```
These are tool invocations: how to obtain a number from SBCL. They do not tell you whether the number means
anything. Paired A/B protocols, warmup and full-GC discipline, measuring the noise floor before claiming a
delta, gating on a confidence interval rather than a point estimate, and proving you are measuring your
working tree rather than a pre-registered store build all belong to
[performance-benchmarking](../performance-benchmarking/SKILL.md): consult it before reporting any
before/after comparison.

## Build and release

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

## Ecosystem integration

- **SLY**: prefer SLY over SLIME in this environment, assume sly/sly-asdf/sly-macrostep workflows for Emacs
  integration, and give SLY-compatible guidance when explaining editor actions.
- **Nix**: `nix shell nixpkgs#sbcl` for reproducible execution; pin project environments via `shell.nix` or
  `flake.nix`, and combine with Qlot for stronger dependency reproducibility.
- **Roswell**: `ros install sbcl`, `ros run`, `ros build app.ros` for implementation management and script
  execution.

## Related

- [common-lisp-ecosystem](../common-lisp-ecosystem/SKILL.md): CLOS/ASDF/condition-system foundations this
  skill builds runtime operations on top of.
- [nix-ecosystem](../nix-ecosystem/SKILL.md): pinned SBCL runtime environments with nix shell/flake.
- [investigation-patterns](../investigation-patterns/SKILL.md): evidence-driven root-cause methodology
  behind the debugging workflow above.
- [performance-benchmarking](../performance-benchmarking/SKILL.md): benchmark methodology (paired protocols,
  noise floor, interval-based gating) behind the profiling tools above.
- [test-integrity](../test-integrity/SKILL.md): false-green testing: suites that report success without
  exercising the contract.
