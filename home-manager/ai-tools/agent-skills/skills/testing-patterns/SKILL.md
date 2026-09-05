---
name: testing-patterns
description: Use when writing, structuring, or reviewing tests - test strategy, coverage, unit/integration/e2e split, mocks/fixtures/fakes, flaky-test isolation, async settlement. For whether a green suite actually proves anything, see test-integrity instead.
version: 3.0.0
---

Designing tests that hold up. Arrange-act-assert, given-when-then, the stub/mock/spy/fake vocabulary, and
descriptive naming are assumed; this file carries the decisions that go wrong and the traps in how the runner is
wired.

Whether a *passing* suite proves anything belongs to [test-integrity](../test-integrity/SKILL.md).

## Classify by the boundary crossed

Scope alone ("one function" versus "several components") does not settle the cases teams actually argue
about, because both readings are defensible for the same file. **The boundary crossed is decidable:** does this
test touch a socket, the filesystem, a subprocess, a daemon lifecycle, the program's standard output?

| Layer | Boundary |
|---|---|
| unit | Crosses none: everything runs inside the test process |
| integration | Crosses a real process, network, filesystem, or daemon lifecycle |
| e2e | Crosses the program's outermost entry point: a command invocation, standard output, a browser session |

A file mixing deterministic helper checks with process-boundary checks is **split along the boundary**, not
filed whole under whichever kind holds the majority: otherwise the fast suite inherits the slow file's
flakiness, or the slow suite hides fast checks nobody runs early.

Let exactly one mechanism route a file to its suite, and prefer the directory path over filename markers. When
the layer can be inferred from two mechanisms, a stray character in a manifest or a missed naming convention
silently routes a file into a suite that never executes it, **and the omission is invisible because nothing
failed.**

## Coverage

Line, branch, and function coverage answer "what executed". They do not answer "what is guaranteed". Aim high
but prioritize meaningful tests: 100% coverage does not guarantee correctness, and a coverage number is not a
substitute for asking what would have to break for a test to go red.

Check applicability against the quality characteristics too (functional suitability, performance,
compatibility, usability, reliability, security, maintainability, portability), not only executed lines.

## Designing the cases

### Rotate adversarial perspectives

Each perspective must leave at least one confirmation point; never trust "it should work".

- **Naive user**: intuitive misuse, unexpected operation order.
- **Heavy user**: rapid, bulk, or sustained input; behavior under load.
- **Adversarial input**: boundary values, invalid values, out-of-permission operations, injection.
- **Integrity auditor**: verify persisted state directly, not the return value or UI alone.
- **Compatibility**: existing data, legacy formats, missing or malformed input.
- **Regression sentinel**: side effects on neighboring features; existing behavior preserved.
- **Spec skeptic**: divergence from the primary source.

### Ground expected values in evidence

Base every expected value on a primary source (requirements, spec, or source code) and cite it. When a value
cannot be verified, **mark it explicitly rather than guessing.** A fabricated assertion makes a test look
complete while proving nothing.

### Drive families from a case table

Where several tests are identical except for input and expected classification, define a typed case record
carrying a stable id, and tag each assertion with it so a failure names the exact row.

```
interface Case { id: string; name: string; input: string; expect: Status; skip?: { reason: string } }
for (const c of cases) {
  if (c.skip) continue   // record intent; do not silently drop
  assert(run(c.input), `${c.id}: ${c.name}`).hasStatus(c.expect)
}
```

Carry a per-case skip *reason* in the record instead of commenting cases out, so intentionally-unrun cases stay
visible in the table.

### Property-based tests where an invariant holds

Where a property holds for all valid inputs (serialization round-trips, sorting invariants, parser
correctness), generate inputs rather than enumerating them. QuickCheck and Hedgehog for Haskell, fast-check for
JS/TS, Hypothesis for Python.

### Snapshots for complex output

Component rendering, serialized structures, CLI output. Vitest 4.x supports visual regression via Browser Mode.
Vitest is preferred for new JS/TS projects; Jest remains supported.

## Fixtures

### Choose values that keep derived values distinct

A legal but degenerate fixture value produces a test that appears to cover several paths while exercising one.
**It fails silently in both directions**: the collapsed path is never checked, and the surviving path passes,
so the suite reports coverage of a condition it never reached.

```
position.y = 64.3     // lower sample resolves to 64, upper to 65, not: 64
startAt(threshold + 2 * step)   // one step lands past the boundary, not exactly on it
```

An advance landing exactly on a boundary tests the wrong side of a strict comparison. And **do not feed an
already-normalized fixture constant back through its normalizer**: converting a converted value commonly
yields nothing, the fixture falls back to its default, and the scenario the test claims to arrange was never
built.

### Suspect the fixture before the implementation

Code behind a chain of preconditions returns its null result for *any* unsatisfied one, so the symptom names
none of them. When a test unexpectedly returns nothing, enumerate every gate on the path (session gate,
feature-flag gate, permission gate) and confirm the fixture satisfies all of them before touching production
logic. Prefer building the fixture from the same helper the passing tests use rather than assembling a
plausible-looking one by hand.

**The worst available outcome is changing correct production code to accommodate an inadequate fixture.**
Weakening a guard makes the test pass and removes the behavior the guard existed for.

### Scenario-scoped identifiers, not truncation

Give each scenario its own unique id for the data it creates, tag records with it, and delete by that id in
teardown. Truncating shared tables is a blunt reset that breaks the moment tests run concurrently, and it can
destroy seed data the suite did not create.

### Snapshot and restore global state

A fixture that mutates global state saves the prior value, runs the body, and restores it under an unwind
guarantee, covering the **complete write set** of the body.

- The classic bug is restoring one binding but not a second the same code path also mutates, such as a lookup
  table populated as a side effect of loading a mode. A later test inherits the leaked entries.
- **Take the copy at setup, before the body runs.** Copying the originals during cleanup is too late when the
  body mutates a shared structure destructively: cleanup then publishes copies of the already-mutated state,
  and any alias held outside the fixture still points at the damage.
- **Restore to the prior value, never reset to a default.** A blanket reset destroys legitimate pre-existing
  state and makes the suite order-dependent. Restoration must preserve identity-bearing structures (an index,
  a cache, a tail pointer), not merely equal values, or consumers holding references observe a different object
  than the one they registered against.
- Distinguish unbound from bound-to-nil. If a variable was originally unbound, cleanup unbinds it again, since
  code branching on boundness will otherwise take the wrong path for the rest of the run.
- **Make the restoration uninterruptible.** An unwind guarantee ensures cleanup is entered on an interrupt, but
  a second interrupt during cleanup can abandon it halfway, leaving exactly the partial restore this exists to
  prevent. Inhibit interrupts around the whole restoration, not each assignment.
- Never rely on a later test's fixture, or on runner ordering, to absorb leaked state. That coupling is
  invisible and breaks the first time the suite runs in a different order or in parallel.

For several globals, build a thin multi-binding wrapper over the single-binding helper rather than nesting many
restore forms.

## Doubles

### Substitute through a declared seam

Bind an indirection the production code declares (a dynamically-scoped variable, a strategy slot, a
constructor parameter) rather than overwriting a global function definition for the duration of a test.

**Overwriting a global binding is process-wide and unscoped.** Under a parallel runner it corrupts unrelated
tests non-deterministically, and the resulting flake is nearly impossible to attribute because the failing test
never mentions the test that did the overwriting.

Local function shadowing does not intercept calls compiled to direct global references. A helper compiled
against the global name keeps calling it, so **the stub appears to install successfully and simply has no
effect**: the test then passes or fails for reasons unrelated to the substitution it believes it made. Verify
the substitution took, or drive the real object's state and assert on the real path; test-integrity carries the
audit for proving which implementation actually ran.

Where no seam exists and none can be added, prefer mutating the real subject into the state that selects the
branch. Slower to set up, and far more honest than a stub whose effect was never applied.

### A double implements the full observable contract

Not only the return value: destructive and ordering semantics too.

```
// The real call deletes its input range before writing output; the double must too
(lambda (beg end &rest _) (delete-region beg end) (insert "output") 0)
// A double that only inserts leaves stale input in place and hides the bug
```

Under-modeled doubles are a common source of tests that are green yet meaningless: **they assert against a
fiction.**

### Derive a set of doubles from the production registry

When a test replaces every member of a dispatch chain, build the double set from the same list production
dispatches over, or assert the two sets are equal before running.

A hand-enumerated set quietly becomes a second registration list nobody knows they own. Adding a member to the
production chain leaves it undoubled, so the real implementation runs against empty stubs; **and the resulting
failures land in unrelated cases elsewhere in the file, naming neither the new member nor the file that needed
updating.**

This is completeness of the set, a different failure from fidelity of any single double: a set can be perfectly
faithful member by member and still be wrong because it is missing one.

### Guard variant-specific reads when enumerating a registry

A test that iterates a production registry and reads a property off each member must guard the read with a
predicate for the variant that carries it, and assert the guard matched at least once.

```
for (const member of registry) {
  if (!hasCommandForm(member)) continue
  assertValidCommand(member)
}
```

**A docstring narrowing a registry's contract is documentation, not enforcement.** A second variant registered
through a different path will eventually appear, and every consumer reading a property only the first variant
carries then silently degrades: the loop still runs, the assertion still passes, and it proves nothing about
the members it skipped. When the same inline presence check appears at several call sites, the duplication is
the signal that a named predicate is missing.

## Asynchrony

### Poll a definitive source; do not retry-on-throw

```
await waitForStatus(store, id, "COMPLETED", { intervalMs: 200, timeoutMs: 30000 })
```

The retry-on-exception form re-runs assertion machinery on every attempt, hides the actual terminal state
behind the last exception, and couples total wait to backoff math rather than a declared budget. Express
interval and timeout in **one explicit unit**: mixing string durations with numeric ones invites silent
coercion at the framework boundary.

### Settlement is layered, and a race is answered by strengthening

**A single read of a single source is not settlement.** Retry the stimulus until the first observable effect
appears, then poll a *different, authoritative* source for the consequence. A status published on the previous
tick reads as ready while the durable value is still mid-flight, so the flag alone samples a transient
intermediate value some fraction of the time.

```
await waitForFlag(subject, "settled")      // necessary, not sufficient
await waitForDurableRecord(store, id)      // second, independent source
assertUnchanged(preexistingEntities)       // the change disturbed nothing else
```

**When such a test proves flaky, the correct response is a stronger barrier, never a wider tolerance.**
Relaxing an exact expected value to a range, or dropping the durability check, converts a real race into a
permanently silent one. Loosening is the default reflex under time pressure and is almost always wrong here.

Include the negative half: assert that entities the change was not supposed to affect are still present. A
settlement bug frequently manifests as collateral loss rather than as a wrong value at the target.

### One transition per settle step

When a scenario drives update B whose input is the result of update A, split them into separate steps.
Batched-update semantics collapse the intermediate renders, so B is dispatched against the state captured when
the batch opened rather than against A's committed result. Independent updates may share a settle step.

### Restart round trips need the same profile directory

To prove data survives a restart, launch a persistent context bound to an explicit profile directory, poll for
the persisted record, close, and launch a second context against the same directory.

A default per-test context starts with empty storage regardless of whether the write ever succeeded. A test
asserting *presence* can then only fail, never falsely pass; but **the mirror-image test, asserting data is
gone after a reset, passes vacuously every time and will never detect a broken reset.**

Poll before closing the first session: closing immediately after the save races the storage layer's flush, and
the resulting failure looks like a durability bug rather than a missing barrier. Give each run its own
temporary profile directory, removed in a cleanup block that runs even when the body throws.

## Efficiency guarantees are operation counts, not wall clock

An efficiency fix has no natural test, because the observable output is identical before and after. A suite
that only checks output stays green when the optimization is silently undone by the next change to a data
relation or a call site.

```
for (const size of [81, 289, 1089]) { applyBatch(size); assertEqual(commitCount(), 1) }
assertEqual(queriesIssuedDuring(() => loadListPage()), 3)
```

Wall-clock assertions are machine-dependent, so **they get widened after each spurious failure until they no
longer detect anything.** An operation count is deterministic on any machine and names the regression directly.
Assert at several input sizes rather than one: a count constant at one size may be linear at another, and the
guarantee you care about is the shape of the curve.

## Skipping versus failing

A skipped test and a failed test carry different meanings. Reserve failure for a violated expectation about
code under your control; use skip for a missing precondition of the *environment*. **Conflating the two trains
readers to ignore red.**

```
setup: async (ctx) => { if (!(await dependencyReachable())) ctx.skip("dependency unavailable in this environment") }
```

## Backward compatibility

When adding an optional parameter, key, or field to a contract existing callers already use, write one test
that makes the old call and asserts the old result shape. **The new-feature tests all pass the new argument, so
none of them ever exercises the old call shape.**

Omitting the addition and passing it explicitly as null must produce identical behavior and identical output
shape; when they differ, callers acquire an invisible dependency on argument-passing style. Exactly one layer
owns the decision to forward a new key: when two layers each conditionally append it, the result carries it
twice and the winner depends on the consumer's parsing order. Prefer a stable output shape (key always present,
sometimes null) over one that appears and disappears: a varying shape forces every consumer to handle both
forms, and **the branch handling the absent form is the one that goes untested.**

## Declarative repositories

Where the product is configuration rather than a running program, the acceptance gate is successful **build**
of every affected output, plus a repository-wide search for what the change was supposed to remove.

Building is strictly stronger than evaluating: some errors (a duplicated module argument, a conflicting option
definition) surface only when the output is realized, so an evaluation-only check reports success on a
configuration that cannot build.

**The search half is the part people skip and the part that catches real defects.** A successful build proves
the new path works; it says nothing about whether the old path was fully removed, so a migration can leave both
installed and appear entirely healthy. State the gate as an enumerated list of commands that must exit zero:
"it builds" is not checkable by a reviewer; a list of targets is.

## Authoring test infrastructure

Model assertion outcomes as **structured failure values**, not bare booleans: expected, actual, location,
message. Where the host language has a condition system, signal a typed failure and expose named restarts: a
retry restart that reruns without consuming the retry budget, with cleanup under an unwind guarantee, keeps the
control flow predictable. Boolean assertions discard that structure and force the reader back to the source to
reconstruct intent.

Keep each matcher a **single deterministic transformation** from actual value to verdict, and compose rather
than branching inside one. Matchers that branch on external state or mutate as a side effect become
order-dependent.

Two hazards present as slowness or a hang rather than a failure, which is why they survive so long:

- **Trial-loop ownership.** A property-trial loop needs exactly one owner. Nesting iteration in both the runner
  and the property macro multiplies the trial count silently: fifty by fifty becomes two and a half thousand.
- **Bounded generator failure.** A generator constrained by a predicate must reject an impossible domain with a
  bounded failure after a finite, configurable number of attempts. An unsatisfiable predicate otherwise becomes
  an unbounded hang, indistinguishable in CI from an infrastructure stall.

When a macro generates globally-registered test names from a caller-supplied label, **namespace the label**:
the enclosing describe or context block is usually not part of the generated name, so two generators sharing a
derived name across files can silently shadow each other and one suite's cases quietly vanish.

Delete any test helper that only forwards to another with fixed arguments; call the canonical builder directly
so a reader sees the arguments at the call site. Do not lift a scenario setup into a shared module until
several specs genuinely share the same bootstrap: **a shared helper with one caller is a second place to read
before understanding the first.**

## Tooling traps

Not test-design mistakes: traps in how the runner, compiler, or server is wired. Each makes a green result
meaningless or a red one misleading.

**A stale compiled artifact shadows the source.** A source edit appears to have no effect, or a test keeps
failing against behavior that no longer matches the code. Default bundler resolution places `.js` ahead of
`.ts` for extensionless imports, so a stale compiled sibling is exercised instead of the edited source.
Regenerate or delete it, or import with an explicit extension. **When a change "does nothing", suspect a
shadowing artifact before suspecting the test.**

**A global coverage gate fails on a subset run.** Global thresholds are evaluated over the aggregate of only
the files measured in that run, and files imported but not exercised register as uncovered, so a subset run
can fail a gate the whole suite would pass. Run the full suite before trusting a global gate, or configure
per-file thresholds.

**A reused dev server breaks mock assumptions.** Runners configured to reuse an already-running server on the
target URL will reuse *any* server occupying that port and never launch the mock-backed command the test
assumes, so the test exercises the wrong backend, and only locally. Ensure no unrelated server holds the port,
or disable reuse for suites whose correctness depends on the launched command.

**A shared render helper leaves the tree in a preliminary state.** It mounts the root in its loading or splash
phase, so assertions written for the ready view target markup the component never reached; and the failure
names an element that does exist elsewhere in the codebase. Set the startup state explicitly rather than
assuming the helper leaves the tree usable.

**An indiscriminate failure monitor.** An E2E harness treating every logged error as a failure fires on routine
application logging, becomes noise the team mutes, and at that point detects nothing. Classify fatal signals
explicitly (uncaught exceptions, unhandled rejections, named fatal error classes) and let non-fatal logging
pass.

## Never

- Test implementation details instead of observable behavior.
- Over-mock: excessive mocking usually indicates a design problem. Use real implementations where practical.
- Leave a test non-deterministic. Control time, randomness, and async explicitly.
- Let tests depend on execution order or shared state.
- Use AI merely to *run* tests while neglecting case design: design coverage first, then run.

## Related

- [test-integrity](../test-integrity/SKILL.md): whether the resulting green is trustworthy
- [performance-benchmarking](../performance-benchmarking/SKILL.md): how fast something is, rather than how to test it
- [investigation-patterns](../investigation-patterns/SKILL.md): debugging a test failure or a flake
- [execution-workflow](../execution-workflow/SKILL.md): where tests sit in the delivery workflow
- [requirements-definition](../requirements-definition/SKILL.md): deriving acceptance criteria to test against
