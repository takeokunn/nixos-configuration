---
name: test-integrity
description: Use when a test suite is green but its value is in doubt — false greens, zero tests collected, vacuous assertions, dead guards, or mocks that make the code under test unnecessary. Contrast testing-patterns (designing tests); this skill asks whether a passing suite proves anything.
version: 3.0.0
---

One question and its many disguises: **the suite is green — is that evidence?**

Every trap here produces byte-identical output to a healthy suite, so none is caught by reading the result.
They are caught only by deliberately asking what would have to break for this test to go red.

Test *design* — the unit/integration/e2e split, arrange-act-assert, double selection, where a seam goes and how
a double is installed in it, fixtures, settlement barriers, coverage as a metric — belongs to
[testing-patterns](../testing-patterns/SKILL.md). This skill assumes those tests are written and asks only
whether their green result is trustworthy.

**Vacuity has degrees.** An assertion satisfiable by every outcome is fully vacuous; a test whose precondition
is never met is vacuous in practice while looking sound in source. **A seam exists only if the production call
path actually goes through it at the moment the test runs.**

## The test never ran

The most complete false green. Worth its own category because every mitigation has the same shape: stop
accepting a zero exit code as the signal, and start asserting a count of what executed.

### Zero collected is not zero failures

Every runner accepts a selector — a directory, a glob, a tag, a name filter — and every selector can match
nothing. A run over an empty set exits successfully and prints a summary differing from a full run only in
numbers nobody reads. A referenced directory that does not exist, a filter argument accepted at the command
line but never forwarded to the runner, and a build target never added to the check graph all produce this.

A gate must assert a **non-zero collected-and-executed count**, and assert every selected test's outcome is
*passed* rather than merely *not failed* — some runners report expected-failure and skipped states in ways that
satisfy a naive exit-code check. Designate exactly one canonical gate command; a narrower subset run must never
be cited as having satisfied it.

### Reconcile every discovery mechanism

A suite usually has more than one registry: files on disk, a manifest listing them, a build-system component
list, a package export list, a shard configuration. A test can be present in some and absent from others. It
then compiles, passes when invoked by hand, and is never scheduled. **This is green-on-green — nothing fails,
coverage just quietly shrinks** — and it is among the most frequently repeated operational failures in practice.

Write a meta-test reconciling the registries against the filesystem and against each other, failing when any
test source is unreachable from the canonical entry point. That test is the only thing standing between a suite
and slow invisible erosion; adding a file must fail loudly until every registry names it.

### Two naming conventions, one glob

Two conventions commonly coexist in one repository — a suffix for unit specs and a different one for
integration specs. A runner matching only one silently ignores every file using the other, and **the
configuration looks reasonable in review because the pattern it contains is a real, valid pattern.**

Compare the runner's reported test-file count against the count on disk, per runner, whenever a convention is
introduced or a runner added. Better: enforce a single convention with a lint rule.

### Assert the spawned process result

An integration harness that launches a binary or forks a worker and then inspects side effects reports success
when the process crashed before doing anything, provided the side effects it checks are absent-tolerant or
pre-existing. Logging a panic without counting it has the same effect: incomplete work exits successfully.

Assert the exit status of every spawned process and the completion count of every worker pool *before*
asserting anything about side effects. A non-zero status, a panic count above zero, or a completed count below
the dispatched count is a test failure in its own right, not context for a later assertion.

### Registration collisions delete tests

Frameworks with a global name-keyed registry replace an existing registration when the same name is registered
again. This is deliberate — it prevents accumulation across reloads — but **a duplicated name silently deletes
a test rather than reporting a conflict.** Two common sources: a shared helper file both loaded as a dependency
and picked up as a standalone file by a recursive loader; and a migration to a table-driven generator where the
legacy hand-written cases were left in place beside it.

Helper files must not define tests. After converting a cluster of cases to a generator, delete the cases it
replaced in the same change. Derive generated names from a namespaced prefix so two generators cannot collide,
and assert the post-registration count equals the expected case count.

## The assertion could not have failed

These read as real tests in review and count as covered lines in a coverage report.

### Name the input that would fail

An assertion whose accepted set covers every outcome the system can produce is a tautology. The most common
form is disjunctive: asserting a result is *either* the empty value *or* a well-formed object, when those two
exhaust the return type. The test appears to cover a rejection path while proving only that the function
returned. The unconditional true assertion is the degenerate case, often used as a placeholder to mark a
requirement "covered".

**For every assertion, name a concrete input that would make it fail.** If none exists, it is vacuous. Replace
a disjunction with the exact expected outcome — stub the collaborators to force the branch, then assert the
single value that branch must produce. A passing placeholder is worse than an absent test, because it
suppresses the gap: leave the case failing or explicitly pending instead.

### Never call the subject to compute the expected value

Deriving the expected value by invoking the system under test makes the assertion true by construction when the
subject is pure, and **flaky when it is stateful** — a second invocation of a stateful parser or builder can
return a differently shaped result than the first, so the test fails for reasons unrelated to the contract. The
failure presents as a genuine behavior regression, which is why this costs an investigation rather than being
spotted immediately.

Ground expected values in the specification, a fixture, or a previously captured artifact. Where the subject
must be invoked to build the comparison, invoke it exactly once and assert against that single captured result.

### The stimulus must not satisfy the assertion

When the input channel is visible in the output channel, an assertion can be satisfied by the stimulus rather
than by the system's response. A terminal echoing typed input satisfies any marker that is a literal substring
of the command *before the command executes*; a shell that traces commands, and a log-scraping test that greps
for the line it just wrote, have the same shape. Everything the test claims to prove — that the command
executed, that rendering occurred, that the stream advanced — is unproven.

Disable echo where possible; snapshot the output before the stimulus and search only the post-stimulus delta;
emit markers from encoded or split input so the marker bytes never appear in the stimulus; carry an output-only
nonce that also encodes the exit status. Make it mechanical: a helper should reject an expected literal or
pattern that matches the stimulus it is about to send.

### Substring matching across numbered siblings

A substring assertion on rendered text containing sequence numbers matches its own numbered siblings: a check
for entry one is satisfied by entry ten and entry eleven, which share its prefix. The test passes while
asserting nothing, and **only becomes visibly wrong once the sequence crosses a digit boundary** — long after
it was written and long after anyone remembers why.

Parse the rendered output into blocks, assert the block count, and assert the first and last blocks exactly.
Where a substring check is unavoidable, anchor it with a delimiter that cannot occur inside a sibling's
identifier.

### Observation must not move the metric

Probing a cache, counter, or rate-limited component to inspect its contents can itself register as an access. A
later assertion that the hit count is at least one is then satisfied by the probe. The test measures its own
instrumentation.

Read counters through a side-effect-free accessor, or snapshot immediately before the act step and assert on
the delta. When no non-mutating accessor exists, **that absence is a design defect in the subject**, not
something for the test to work around.

## The fixture removed the subject

The test ran and the assertion is meaningful, but the assertion is checking work the setup performed.

### A presence probe is satisfied by its own double

Any "is this dependency available?" check — a function-bound predicate, an attribute probe, a callable type
check, an interface conformance test — is satisfied by the double installed to replace that dependency. The
availability test becomes a tautology: it passes with the real component present and passes identically with it
absent. Pair this with a broad exception swallow around the call boundary and **signature drift becomes
invisible too** — a change adding a leading argument to every function at that boundary can leave a large body
of tests passing against doubles that still have the old arity.

Prove availability by observing behavior only the real component can produce, never by probing for a name.
Never wrap the boundary in a catch-all swallowing arity or signature errors — those are defects, not expected
conditions. Where a double stands in for a component with a known signature, assert the double's signature
against the real one so drift fails at fixture-construction time.

### A partial module double erases value exports

Guidance on doubles concentrates almost entirely on function behavior, so a partial module replacement
typically re-declares the functions and omits everything else. Non-function exports — enumerations,
allow-lists, category sets, thresholds — then resolve to undefined. Code validating against them does not
throw; it classifies every input as invalid and returns an empty result. **The symptom is a plausible empty
collection rather than an error**, which sends the investigation into the parser or the feature code instead of
the fixture.

A partial module double must re-export every symbol any consumer reads, not only the helpers the test calls
directly. Diagnostic heuristic: when a parsing or filtering test returns an unexpectedly empty result, inspect
the module double before touching the subject.

### Seeds establish preconditions, never the subject

A deterministic seed exists to reach a starting state cheaply. When it also creates the artifact the test
claims to verify — seeding the produced item, activating the resulting view, placing the actor already inside
the state the transition was supposed to reach — the test asserts a state the product may be incapable of
reaching on its own. **This is the most common way an end-to-end suite becomes decorative while its case list
still reads like full coverage.**

Seeds may establish world, actor, and inventory preconditions. The transition under test is performed through
the same interface a real user drives. State this as a boundary rule in the fixture layer, so it survives the
next person who finds the seed convenient.

### A fixture that misses a gate exercises nothing

When the path under test opens with a guard clause, a fixture failing to satisfy the guard turns the whole
interaction into a no-op. The resulting failure is misleading in an expensive way: it accuses a downstream
subsystem that is perfectly healthy, and in the worst case surfaces as a timeout whose message names neither
the real cause nor the right subsystem.

Enumerate the guards on the path and build the fixture to satisfy each explicitly. When an integration test
fails, confirm the code path actually ran before investigating the subsystem the failure appears to implicate —
an assertion that the entry point was reached is cheap and eliminates the entire class.

### Redirect every persistence path before anything can register

A harness redirecting only the obvious persistence path leaves the others pointing at real user state. The
suite then reads and rewrites the developer's real data, both damaging it and contaminating the suite's
assertions with values the test never created. **Ordering matters as much as coverage**: enabling a mode can
register an exit hook or load a file immediately, so a redirect applied after that point is too late.

Bind every persistence path — primary stores, learning or statistics files, caches, history — to per-run
temporary locations before any code path can load or register. Add a suite-level assertion that real user files
are unchanged after the run; without it this class of defect is entirely invisible.

## Guards nothing proves can fire

### Prove the guard trips

A safety control whose threshold nobody crosses is inert, and nothing reports it. Line and branch coverage are
fully satisfied — the guard is evaluated on every call — while the interesting branch is unreachable in
practice. **This is the general shape of every silently dead safety control**: rate limiters, circuit breakers,
alert thresholds, kill switches, validation bounds.

Write a test that drives each guard to *trip*, not merely one that calls the enclosing function. Prefer
thresholds expressed relative to a measured baseline over absolute constants, because an absolute constant
silently dies when the underlying scale changes. Keep measurement outside any gate whose behavior it feeds — a
counter inside a conditionally-executed body measures the gate, so a threshold expressed in units of that
counter can never be reached.

### Delete impossible branches rather than test them

Under a strict branch-coverage gate an unreachable defensive branch leaves three moves: fabricate a test
constructing an impossible input, suppress the branch with a pragma, or remove it. **The first is the worst** —
it encodes a state the system cannot produce, and future readers treat it as a real case. If a helper is only
ever called under a structural invariant, the fallback is not defensive; it is noise the coverage tool is
correctly refusing to ignore.

The discriminator: is the fallback behavior user-visible, or unreachable because an upstream invariant
guarantees the input? Test the first; delete the second and assert the invariant instead. For an exhaustive set
of cases, prefer enumerating every case with an explicit terminal result over a catch-all arm, which
manufactures a branch no input can take.

## Something ate the failure

Cleanup code is the usual culprit: it runs on both the success and failure paths and is written as if it cannot
fail itself.

A cleanup that raises either replaces the real failure or is swallowed by a handler written to keep the suite
moving. Both lose information. **Swallowing is worse than replacing**, because a resource leak — an orphaned
process, an unreleased lock, a temporary directory outliving the run — then reports as a successful test, and
the leak accumulates across the suite until something unrelated fails.

When both the body and the cleanup fail, keep the body's condition primary and attach the cleanup's to it as
structured data under a distinct key. When only the cleanup fails, the test still fails. Never report a
cleanup-only failure as a pass, and never discard the body's condition to surface the cleanup's.

Teardown releasing many resources in one loop aborts at the first failure, leaving the remainder allocated —
the single reported error understates the leak by an unknown factor, and the next test starts in a state nobody
described. Wrap each release independently, collect every failure, and report them together after attempting
all of them. Where the resource is a process tree or external system, distinguish "the release call failed"
from "the release call succeeded but the resource is still present" — only the second is a leak, and only an
explicit post-condition check finds it.

## Which implementation actually loaded

This extends the stale-compiled-artifact trap to the multi-location case, which that mitigation does not reach.
When the same component exists in more than one location — a working tree and an installed copy, a local
package and a system-wide one — the loader picks one, and **newest-wins preference settings arbitrate only
within a single location, never between locations.** Deleting stale artifacts therefore does not help: both
candidates are legitimately current, and the wrong one is simply earlier in the search order. A test can run
entirely against an implementation the change never touched, producing both false greens and false reds, and
the usual mitigation would lead you to trust a run that is still wrong.

Before believing any result from a suite with this shape, query the runtime for the provenance of a symbol
under test and confirm it resolves to the tree being changed. Compare timestamps of the built artifact against
the loaded one when behavior contradicts the source. Make the harness put the working tree ahead of every
installed location explicitly rather than relying on newest-wins.

The same audit applies to any installed double: **give the substitute an observable trace, or make it fail
loudly when called with unexpected arguments, so a substitute that was never consulted cannot be mistaken for
one that agreed.**

## The non-vacuity audit

Run this when a suite is inherited, when a green result is about to be cited as proof a change is correct, or
when a defect escaped that the suite claimed to cover.

**1. Prove it can fail.** Mutate the implementation the test names — invert a condition, return a constant,
delete the effect — and confirm the test goes red. Restore immediately. This is the only check that subsumes
every trap here at once: a test that stays green under a mutation of its own subject was never testing that
subject, whatever the reason.

**2. Assert what executed.** Capture the runner's collected and executed counts for the canonical gate and
assert them against the number of test definitions in the tree. Assert the exit status of every spawned process
and the completion count of every worker. This distinguishes a green over the full set from a green over an
empty or truncated one — indistinguishable by exit code alone.

**3. Prove the precondition and forbid the fast path.** Before the act step, assert the state the test depends
on actually holds — the cache entry is warm, the gate is satisfied, the slow path is the one about to run. Then
make the alternative path an error for the duration, so taking it fails rather than passes. A test named for a
warm path that silently exercises a cold one passes for the wrong reason forever. Asserting the precondition
catches setup drift; forbidding the fast path catches the case where the subject stops using the path the test
names. *Deleting a cache entry tests a cold miss, not stale-entry rejection* — the distinction only survives if
the test states it.

**4. Discover the subjects rather than list them.** For a cross-cutting invariant, enumerate the population from
the code itself — an exported-symbol scan, a reflective walk, a filesystem traversal — and assert the invariant
over every member. Encode intentional exceptions as an explicit data set, each pinned by its own negative test
asserting that this member does not satisfy the invariant, deliberately. Strictly stronger than a maintained
allowlist: **a new subject defaults to enforced rather than to unchecked**, so "we added a handler and forgot
to apply the rule" becomes a red test rather than a silent gap. The negative test per exception makes each
exception a reviewed act, and fails when an exception stops being necessary.

**5. Differential-test a replacement.** Run the previous version and the new one in the same process against
the same generated inputs, and compare the *full* result — including failure metadata: error class, position,
line, column, expected value, context. Load the prior implementation directly from version control so both are
live simultaneously. Comparing only success values leaves the failure contract unverified, and the failure
contract is usually the part callers depend on most precisely. In practice a large success-only comparison can
report clean while a smaller run that also compares error metadata finds mismatches in the low percent range —
the ratio is the argument for comparing everything rather than for running more cases.

*Confirm the two arms are still two implementations.* Part-way through a migration the usual tidying move is to
make the old entry point delegate to the new one, and at that moment the comparison becomes an implementation
compared against itself. The difference is zero by construction, the test stays green forever, and **nothing
marks the instant it stopped testing anything** — a regression present in both paths passes. This is a specific
risk for agent-driven refactoring, because collapsing a superseded implementation into a wrapper around its
replacement is exactly the cleanup an agent reaches for. Read the old arm's body and check it still computes
rather than forwards; where it no longer does, replace the comparison with table-driven expected values or an
independent reference calculation.

**6. Audit the assertions.** For each assertion in the area, name an input that would make it fail. Flag every
disjunction exhausting the return type, every unconditional truth, every substring check against numbered
content, and every expected value computed by invoking the subject. Mutation testing catches most of these but
is expensive to run broadly and cannot be applied to a suite that will not run at all. This pass is cheap,
needs no infrastructure, and is where most vacuity is actually found.

## Related

- [testing-patterns](../testing-patterns/SKILL.md) — the design decisions whose results this skill audits
- [performance-benchmarking](../performance-benchmarking/SKILL.md) — benchmark methodology and measurement noise
- [investigation-patterns](../investigation-patterns/SKILL.md) — tracing *why* a test behaves as it does
- [quality-tools](../quality-tools/SKILL.md) — review scoring, and the shelf life of a stored verdict
- [execution-workflow](../execution-workflow/SKILL.md) — where the canonical gate runs and what counts as having run it
