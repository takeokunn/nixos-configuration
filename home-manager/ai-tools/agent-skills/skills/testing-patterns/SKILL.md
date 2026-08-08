---
name: Testing Patterns
description: This skill should be used when the user asks to "write tests", "test strategy", "coverage", "unit test", "integration test", or needs guidance on designing, structuring, or isolating tests. Covers the unit/integration/e2e split and classifying a suite by the boundary it crosses, arrange-act-assert and given-when-then, stub/mock/spy/fake selection and seam design over global rebinding, fixture isolation with snapshot-and-restore, scenario-scoped identifiers for parallel runs, polling and settlement barriers for asynchronous outcomes, operation- and query-count assertions instead of wall-clock thresholds, property-based and snapshot testing, matcher design, evaluation as the acceptance gate for declarative-configuration repositories, and runner, compiler, and server traps that silently invalidate a result. Keywords — flaky test, test double, test fixture, teardown, parallel isolation, settlement barrier, query count, tooling trap. For whether a green result proves anything, see test-integrity.
version: 2.5.0
---

<purpose>
  Provide testing patterns and strategies for comprehensive test coverage and maintainable test suites.
</purpose>

<tools>
  <tool name="vitest" version="4.x" status="preferred">
    <description>Current standard for JS/TS testing; preferred for all new projects</description>
    <features>Stable Browser Mode, visual regression testing, snapshot testing, ESM-native, Vite-powered</features>
  </tool>
  <tool name="jest" status="supported">
    <description>Widely used JS/TS test runner; still supported but Vitest is preferred for new projects</description>
  </tool>
  <tool name="eslint" version="10.x">
    <description>Linting for JS/TS codebases; flat config only (eslintrc format removed)</description>
  </tool>
  <tool name="quickcheck" ecosystem="haskell">
    <description>Property-based testing for Haskell</description>
  </tool>
  <tool name="hedgehog" ecosystem="haskell">
    <description>Property-based testing with integrated shrinking for Haskell</description>
  </tool>
  <tool name="fast-check" ecosystem="js/ts">
    <description>Property-based testing for JavaScript/TypeScript</description>
  </tool>
</tools>

<concepts>
<concept name="unit">
  <description>Test individual functions/methods in isolation</description>
  <scope>Single function, class, or module</scope>
  <characteristics>Fast, isolated, deterministic</characteristics>
  <when>Business logic, utility functions, transformations</when>
  <boundary>Crosses none: no socket, filesystem, subprocess, or daemon lifecycle; everything runs inside the test process</boundary>
</concept>

<concept name="integration">
  <description>Test interaction between components</description>
  <scope>Multiple components working together</scope>
  <characteristics>Slower, may use real dependencies</characteristics>
  <when>API endpoints, database operations, service interactions</when>
  <boundary>Crosses a real process, network, filesystem, or daemon-lifecycle boundary</boundary>
</concept>

<concept name="e2e">
  <description>Test complete user workflows</description>
  <scope>Full application stack</scope>
  <characteristics>Slowest, tests real user scenarios</characteristics>
  <when>Critical user journeys, smoke tests</when>
  <boundary>Crosses the program's outermost entry point: a command invocation, standard output, a browser session</boundary>
</concept>

<concept name="suite_classification">
  <description>Which suite a test file belongs to is decided by the boundary the test crosses, and exactly one mechanism is authoritative for routing it there</description>
  <guidance>Scope alone ("one function" versus "several components") does not settle the cases teams actually argue about, because both readings are defensible for the same file. The boundary crossed is decidable: does this test touch a socket, the filesystem, a subprocess, a daemon lifecycle, the program's standard output? Classify on that answer, and let the physical directory path carry it</guidance>
  <guidance>A file that mixes deterministic helper checks with process-boundary checks is split along the boundary, not filed whole under whichever kind holds the majority; otherwise the fast suite inherits the slow file's flakiness or the slow suite hides fast checks nobody runs early</guidance>
  <guidance>Prefer the directory path prefix over filename markers as the single source of truth. When the layer can be inferred from two mechanisms, a stray character in a manifest or a missed naming convention silently routes a file into a suite that never executes it, and the omission is invisible because nothing failed</guidance>
</concept>

<concept name="line_coverage">
  <description>Percentage of code lines executed during tests</description>
  <guidance>Measures which lines of code are exercised</guidance>
</concept>

<concept name="branch_coverage">
  <description>Percentage of code branches (if/else, switch) taken during tests</description>
  <guidance>More thorough than line coverage as it measures decision paths</guidance>
</concept>

<concept name="function_coverage">
  <description>Percentage of functions/methods called during tests</description>
  <guidance>Identifies untested functions</guidance>
</concept>

<concept name="quality_characteristic_coverage">
  <description>Coverage measured against ISO/IEC 25010 quality characteristics, not only executed lines or branches</description>
  <guidance>Check each characteristic for applicability: functional suitability, performance efficiency, compatibility, usability, reliability, security, maintainability, portability</guidance>
</concept>
</concepts>

<patterns>
<pattern name="arrange_act_assert">
  <description>Three-phase test structure for clear test organization</description>
  <decision_tree name="when_to_use">
    <question>Are you writing unit or integration tests?</question>
    <if_yes>Apply arrange-act-assert pattern for clear test structure</if_yes>
    <if_no>Consider given-when-then for BDD-style tests</if_no>
  </decision_tree>
  <example>
    <test_phase>Arrange: Set up test data and preconditions</test_phase>
    user = User.new(name: "John")
    cart = ShoppingCart.new(user)

    <test_phase>Act: Execute the code under test</test_phase>

    total = cart.calculate_total

    <test_phase>Assert: Verify expected outcomes</test_phase>

    assert_equal 0, total
  </example>
  <note>Separates setup, execution, and verification into distinct phases</note>
</pattern>

<pattern name="given_when_then">
  <description>BDD-style test structure focusing on behavior</description>
  <decision_tree name="when_to_use">
    <question>Is the test focused on business behavior rather than technical implementation?</question>
    <if_yes>Apply given-when-then pattern for BDD-style tests</if_yes>
    <if_no>Use arrange-act-assert for technical unit tests</if_no>
  </decision_tree>
  <example>
    <bdd_step>Given: Initial context (preconditions)</bdd_step>
    given_a_user_with_an_empty_cart

    <bdd_step>When: Action or trigger</bdd_step>

    when_the_user_calculates_total

    <bdd_step>Then: Expected outcome</bdd_step>

    then_the_total_should_be_zero
  </example>
  <note>Emphasizes business behavior over technical implementation</note>
</pattern>

<pattern name="stub">
  <description>Provide canned responses for dependencies</description>
  <decision_tree name="when_to_use">
    <question>Does the test need dependency responses but not interaction verification?</question>
    <if_yes>Apply stub pattern for canned responses</if_yes>
    <if_no>Use mock if interaction verification is needed</if_no>
  </decision_tree>
  <example>
    api_client = stub(
      fetch_user: { id: 1, name: "John" }
    )
  </example>
  <use_case>Replace slow/unreliable dependencies</use_case>
</pattern>

<pattern name="mock">
  <description>Verify interactions occurred with dependencies</description>
  <decision_tree name="when_to_use">
    <question>Does the test need to verify specific interactions occurred?</question>
    <if_yes>Apply mock pattern to verify method calls and arguments</if_yes>
    <if_no>Use stub if only canned responses are needed</if_no>
  </decision_tree>
  <example>
    email_service = mock()
    email_service.expect(:send_email, args: ["user@example.com", "Welcome"])
    user_service.register(email_service)
    email_service.verify
  </example>
  <use_case>Ensure methods called with correct arguments</use_case>
</pattern>

<pattern name="spy">
  <description>Record calls while using real implementation</description>
  <decision_tree name="when_to_use">
    <question>Does the test need real behavior plus interaction verification?</question>
    <if_yes>Apply spy pattern to record calls while using real implementation</if_yes>
    <if_no>Use stub for canned responses or mock for behavior replacement</if_no>
  </decision_tree>
  <example>
    logger = spy(Logger.new)
    service.process(logger)
    assert_called logger, :log, with: "Processing complete"
  </example>
  <use_case>Verify side effects without changing behavior</use_case>
</pattern>

<pattern name="fake">
  <description>Working implementation suitable for testing</description>
  <decision_tree name="when_to_use">
    <question>Does the test need a simplified but working implementation?</question>
    <if_yes>Apply fake pattern for lightweight working implementation</if_yes>
    <if_no>Use stub for simple canned responses</if_no>
  </decision_tree>
  <example>
    class FakeDatabase
      def initialize
        @data = {}
      end

      def save(key, value)
        @data[key] = value
      end

      def find(key)
        @data[key]
      end
    end
  </example>
  <use_case>In-memory database, fake file system</use_case>
</pattern>

<pattern name="descriptive_naming">
  <description>Test names that clearly describe scenario and outcome</description>
  <decision_tree name="when_to_use">
    <question>Is this a technical unit test for a specific method?</question>
    <if_yes>Apply descriptive naming with method-scenario-result format</if_yes>
    <if_no>Consider should naming for BDD-style tests</if_no>
  </decision_tree>
  <example>
    test_calculateTotal_withEmptyCart_returnsZero
    test_calculateTotal_withMultipleItems_returnsSumOfPrices
    test_calculateTotal_withDiscount_appliesDiscountCorrectly
  </example>
  <note>Format: test_[method]_[scenario]_[expected_result]</note>
</pattern>

<pattern name="should_naming">
  <description>BDD-style naming that reads like natural language</description>
  <decision_tree name="when_to_use">
    <question>Is this a behavior-focused test readable by non-technical stakeholders?</question>
    <if_yes>Apply should naming for natural language readability</if_yes>
    <if_no>Use descriptive naming for technical unit tests</if_no>
  </decision_tree>
  <example>
    calculateTotal_should_returnZero_when_cartIsEmpty
    calculateTotal_should_applyDiscount_when_couponIsValid
    calculateTotal_should_throwError_when_pricesAreNegative
  </example>
  <note>Format: [method]_should_[expected_behavior]_when_[condition]</note>
</pattern>

<pattern name="property_based_testing">
  <description>Generate random inputs to verify properties that should always hold</description>
  <decision_tree name="when_to_use">
    <question>Does the function have a property or invariant that holds for all valid inputs?</question>
    <if_yes>Apply property-based testing to generate random inputs and verify invariants</if_yes>
    <if_no>Use example-based tests with arrange-act-assert</if_no>
  </decision_tree>
  <example>
    <note>Haskell (QuickCheck/Hedgehog)</note>
    prop_reverse_involutive xs = reverse (reverse xs) == xs

    <note>JS/TS (fast-check)</note>
    fc.assert(
      fc.property(fc.array(fc.integer()), (arr) =>
        deepEqual(arr, reverse(reverse(arr)))
      )
    )
  </example>
  <tools>QuickCheck (Haskell), Hedgehog (Haskell), fast-check (JS/TS), Hypothesis (Python)</tools>
  <use_case>Serialization round-trips, sorting invariants, mathematical properties, parser correctness</use_case>
</pattern>

<pattern name="snapshot_testing">
  <description>Capture output and compare against a stored reference snapshot</description>
  <decision_tree name="when_to_use">
    <question>Is the output complex and best verified by comparing against a known-good reference?</question>
    <if_yes>Apply snapshot testing to detect unintended output changes</if_yes>
    <if_no>Use explicit assertions for specific values</if_no>
  </decision_tree>
  <example>
    expect(renderComponent()).toMatchSnapshot()
  </example>
  <note>Vitest 4.x supports visual regression testing via Browser Mode for comparing rendered UI screenshots against baseline images</note>
  <use_case>Component rendering, serialized data structures, CLI output</use_case>
</pattern>

<pattern name="adversarial_persona_lens">
  <description>Design test cases by rotating through adversarial reviewer perspectives so perspective-coverage gaps surface systematically</description>
  <decision_tree name="when_to_use">
    <question>Are you designing test cases and need to avoid missing an entire class of scenarios?</question>
    <if_yes>Rotate through every adversarial perspective; each must contribute at least one check</if_yes>
    <if_no>Use a single focused pattern (arrange-act-assert) for a known, isolated scenario</if_no>
  </decision_tree>
  <example>
    <note>V1 naive user: intuitive misuse, unexpected operation order</note>
    <note>V2 heavy user: rapid, bulk, or sustained input; behavior under load</note>
    <note>V3 adversarial input: boundary values, invalid values, out-of-permission operations, injection</note>
    <note>V4 integrity auditor: verify persisted state directly, not the return value or UI alone</note>
    <note>V5 compatibility/migration: existing data, legacy formats, missing or malformed data</note>
    <note>V6 regression sentinel: side effects on neighboring features; existing behavior preserved</note>
    <note>V7 spec skeptic: divergence from the primary source (requirements, spec, source code)</note>
  </example>
  <note>Rule: each perspective leaves at least one confirmation point; never trust "it should work"</note>
  <use_case>Test-case design reviews, coverage-gap detection, exhaustive scenario enumeration</use_case>
</pattern>

<pattern name="poll_for_completion">
  <description>Wait for an asynchronous outcome by polling a definitive source at a fixed interval until a terminal condition or timeout, instead of asserting-and-retrying via thrown exceptions</description>
  <decision_tree name="when_to_use">
    <question>Does the test depend on work that completes asynchronously (a job status, a persisted record, a downstream side effect)?</question>
    <if_yes>Poll a completion helper with an explicit interval and timeout budget, both expressed in one unit</if_yes>
    <if_no>Assert directly on the synchronous return value</if_no>
  </decision_tree>
  <example>
    <note>Poll the authoritative store until the terminal status appears, or fail once at timeout</note>
    await waitForStatus(store, id, "COMPLETED", { intervalMs: 200, timeoutMs: 30000 })
  </example>
  <note>Prefer a single polling helper over throw-to-retry with exponential backoff: the retry-on-exception form re-runs assertion machinery on every attempt, hides the actual terminal state behind the last exception, and couples total wait to backoff math rather than a declared budget</note>
  <note>Express interval and timeout in one explicit unit (for example milliseconds as numbers); mixing string durations with numeric durations invites silent coercion at the framework boundary</note>
  <use_case>End-to-end pipeline tests, eventual-consistency checks, queue or worker completion</use_case>
</pattern>

<pattern name="settlement_barrier">
  <description>Wait for the complete, ordered set of observable effects a change produces, rather than for a single readiness flag; and answer a race by strengthening the barrier, never by loosening the assertion</description>
  <decision_tree name="when_to_use">
    <question>Does the behavior under test settle through several stages, for example a transient in-flight state, then a published value, then a durable record?</question>
    <if_yes>Wait for each observable effect in order, poll a second independent source for the durable consequence, and assert that untouched neighbors are still present</if_yes>
    <if_no>A single-condition poll is sufficient; use poll_for_completion</if_no>
  </decision_tree>
  <example>
    <note>A readiness flag read immediately can still carry the value computed before the change</note>
    await waitForFlag(subject, "settled")      // necessary, not sufficient
    await waitForDurableRecord(store, id)      // second, independent source
    assertUnchanged(preexistingEntities)       // the change disturbed nothing else
  </example>
  <note>A single read of a single source is not settlement. The layered form is: retry the stimulus until the first observable effect appears, then poll a different, authoritative source for the consequence of that effect. A status published on the previous tick reads as ready while the durable value is still mid-flight, so the flag alone will sample a transient intermediate value some fraction of the time</note>
  <note>When such a test proves flaky, the correct response is a stronger barrier, not a wider tolerance. Relaxing an exact expected value to a range, or dropping the durability check, converts a real race into a permanently silent one. Loosening is the default reflex under time pressure and is almost always the wrong move here</note>
  <note>Include the negative half of the barrier: assert that entities and records the change was not supposed to affect are all still present. A settlement bug frequently manifests as collateral loss rather than as a wrong value at the target</note>
  <use_case>Simulation- or physics-backed E2E, write-then-persist flows, pipelines with an intermediate published state ahead of the durable one</use_case>
</pattern>

<pattern name="operation_count_assertion">
  <description>Express an efficiency guarantee as an assertion on the number of operations performed, not on elapsed wall-clock time</description>
  <decision_tree name="when_to_use">
    <question>Is the property you want to lock in "this now does less work" — fewer queries, one commit per batch, a single flush?</question>
    <if_yes>Instrument the operation, then assert the exact count for several known input sizes</if_yes>
    <if_no>Assert on the output value</if_no>
  </decision_tree>
  <example>
    <note>Batching guarantee: exactly one registry commit per batch, whatever the batch size</note>
    for (const size of [81, 289, 1089]) { applyBatch(size); assertEqual(commitCount(), 1) }

    <note>Query-count guarantee: the read path issues a fixed number of queries whatever the row count</note>

    assertEqual(queriesIssuedDuring(() =&gt; loadListPage()), 3)
  </example>
  <note>An efficiency fix has no natural test, because the observable output is identical before and after. A suite that only checks output stays green when the optimization is silently undone by the next change to a data relation or a call site, so the repeated-per-item-query problem returns unnoticed. The count assertion is the only thing that pins the fix in place</note>
  <note>Wall-clock assertions are machine-dependent, so they get widened after each spurious failure until they no longer detect anything. An operation count is deterministic on any machine and names the regression directly</note>
  <note>Assert at several input sizes rather than one: a count that is constant at one size may be linear at another, and the guarantee you care about is the shape of the curve, not one sample of it</note>
  <use_case>Batching and coalescing guarantees, collapsing repeated per-item queries in a read path, single-flush or single-commit invariants</use_case>
</pattern>

<pattern name="parameterized_case_table">
  <description>Drive many related scenarios from one typed array of case records rather than copy-pasting near-identical test bodies</description>
  <decision_tree name="when_to_use">
    <question>Are several tests identical except for input and expected classification (for example a family of boundary and invalid-input checks)?</question>
    <if_yes>Define a typed case record (id, name, input, expected, optional skip) and iterate, tagging each assertion with the case id</if_yes>
    <if_no>Write a single explicit test</if_no>
  </decision_tree>
  <example>
    <note>Each record carries a stable id so a failure names the exact case</note>
    interface Case { id: string; name: string; input: string; expect: Status; skip?: { reason: string } }
    for (const c of cases) {
      if (c.skip) continue   // record intent; do not silently drop
      assert(run(c.input), `${c.id}: ${c.name}`).hasStatus(c.expect)
    }
  </example>
  <note>Carry a per-case skip reason in the record instead of commenting cases out, so intentionally-unrun cases stay visible in the table and a failure message pinpoints the offending row</note>
  <use_case>Boundary-value matrices, validation-error families, cross-input contract checks</use_case>
</pattern>

<pattern name="skip_on_unavailable_dependency">
  <description>When an external dependency required by an integration or E2E test is not reachable, skip the test with a stated reason rather than letting it fail</description>
  <decision_tree name="when_to_use">
    <question>Does the test require an out-of-process dependency (database, message queue, emulated cloud service, remote endpoint) that may be absent in some environments?</question>
    <if_yes>Probe availability in a setup hook; on absence, skip with a reason so the signal reads "not exercised", not "broken"</if_yes>
    <if_no>Run unconditionally</if_no>
  </decision_tree>
  <example>
    <note>Setup hook gates the scenario on a reachability probe</note>
    setup: async (ctx) => { if (!(await dependencyReachable())) ctx.skip("dependency unavailable in this environment") }
  </example>
  <note>A skipped test and a failed test carry different meanings: reserve failure for a violated expectation about code under your control, and use skip for a missing precondition of the environment. Conflating the two trains readers to ignore red</note>
  <use_case>Integration suites that run both locally and in CI, optional emulator-backed paths</use_case>
</pattern>

<pattern name="scenario_scoped_identifiers">
  <description>Give each scenario its own unique identifiers for the data it creates, and clean up by those identifiers, instead of truncating shared tables between tests</description>
  <decision_tree name="when_to_use">
    <question>Do tests write to a shared persistent store that other tests, or parallel workers, also use?</question>
    <if_yes>Generate a unique id per scenario, tag created records with it, and delete by that id in teardown</if_yes>
    <if_no>Local in-memory state can be reset wholesale in teardown</if_no>
  </decision_tree>
  <example>
    <note>Unique per-scenario key isolates cleanup and enables parallel runs</note>
    const runId = `test-${scenario}-${uuid()}`
    // write records tagged with runId; teardown deletes where tag = runId
  </example>
  <note>Truncating shared tables is a blunt reset that breaks the moment tests run concurrently, and it can destroy seed or fixture data the suite did not create. Scenario-scoped ids keep cleanup surgical and order-independent</note>
  <use_case>Database- or queue-backed E2E suites, parallel execution against shared infrastructure</use_case>
</pattern>

<pattern name="state_snapshot_restore_fixture">
  <description>A shared fixture that snapshots every global or persistent binding it touches, runs the body, and restores the originals under an unwind guarantee</description>
  <decision_tree name="when_to_use">
    <question>Does a test mutate global state (a registry, a dynamic variable, a function binding, a hash table)?</question>
    <if_yes>Wrap the mutation in a fixture that saves the prior value, runs the body, and restores it in an unwind-protected cleanup, covering the full set of state the body can touch</if_yes>
    <if_no>Keep the test purely local</if_no>
  </decision_tree>
  <example>
    <note>Restore must run even on non-local exit; cover every binding the body mutates</note>
    with-restored (place-a place-b ...)   ; snapshot each, run body, unwind-protect the restore
  </example>
  <note>The classic pollution bug is a fixture that restores one binding but not a second one the same code path also mutates (for example a lookup table populated as a side effect of loading a mode or style). A later test then inherits the leaked entries. Enumerate the complete write set of the body and snapshot all of it, not just the obvious binding</note>
  <note>For several globals, build a thin multi-binding wrapper over the single-binding helper rather than nesting many restore forms; accept both bare bindings and generalized places (such as a function cell) so simple and complex cases share one abstraction</note>
  <note>Take the copy at setup, before the body runs, not at cleanup. Copying the originals during cleanup is too late when the body mutates a shared structure destructively: cleanup then publishes copies of the already-mutated state, and any alias held outside the fixture still points at the damage. The snapshot must be of the pre-body world, so it has to be made before the body exists</note>
  <note>Restore to the prior value rather than resetting to a known default. A blanket reset destroys legitimate state that existed before the test and makes the suite order-dependent, because a test now depends on whether an earlier test happened to establish the state it silently relies on. Restoration also has to preserve identity-bearing structures — an index, a cache, a tail pointer — not merely equal values, or consumers holding references observe a different object than the one they registered against</note>
  <note>Distinguish unbound from bound-to-nil. If a variable was originally unbound, cleanup must unbind it again rather than leaving it bound to a null value, since code that branches on boundness will take the wrong path for the rest of the run</note>
  <note>Make the restoration itself uninterruptible. An unwind guarantee ensures cleanup is entered on a user interrupt, but a second interrupt arriving during cleanup can abandon it halfway, leaving exactly the partial restore this pattern exists to prevent. Where the host language allows it, inhibit interrupts around the whole restoration, not around each individual assignment</note>
  <note>Do not rely on a later test's fixture, or on runner ordering, to absorb state this test leaked. That coupling is invisible and breaks the first time the suite runs in a different order or in parallel</note>
  <use_case>Registry mutations, dynamic-variable overrides, environment-variable tests, hash-table caches</use_case>
</pattern>

<pattern name="designed_injection_seam">
  <description>Substitute a dependency through an indirection point the production code declares — a dynamically-scoped variable, a strategy slot, a constructor parameter — rather than overwriting a global function definition for the duration of a test</description>
  <decision_tree name="when_to_use">
    <question>Does the test need to replace a collaborator that the code under test currently calls by its global name?</question>
    <if_yes>Add a declared seam and bind it for the scope of the test; if no seam can be added, drive the real collaborator's state instead of stubbing it</if_yes>
    <if_no>Pass the double as an ordinary argument</if_no>
  </decision_tree>
  <example>
    <note>The production module declares the seam; the test binds it, scoped and automatically unwound</note>
    with-binding (*rate-source* test-rate-source) (run-entrypoint ...)

    <note>Not: replacing the global definition of the rate-source function for the duration of the test</note>
  </example>
  <note>Overwriting a global function binding is process-wide and unscoped. Under a parallel runner it corrupts unrelated tests non-deterministically, and the resulting flake is nearly impossible to attribute because the failing test never mentions the test that did the overwriting. A declared seam is at worst thread-local and at best explicitly unwound</note>
  <note>Local function shadowing does not intercept calls that were compiled to direct global references. A helper compiled against the global name keeps calling it, so the stub appears to install successfully and simply has no effect — the test then passes or fails for reasons unrelated to the substitution it believes it made. Verify the substitution actually took, or drive the real object's state and assert on the real path; test-integrity carries the audit procedure for proving which implementation the running code actually consulted</note>
  <note>Where no seam exists and none can be added, prefer mutating the real subject into the state that selects the branch you want to exercise. That is slower to set up and far more honest than a stub whose effect was never applied</note>
  <use_case>Entry-point tests with a swappable strategy, parallel suites sharing one process image, code whose collaborators are referenced by global name</use_case>
</pattern>

<pattern name="registry_variant_guard">
  <description>A test that enumerates a production registry must guard every read of a variant-specific property, because a registry's members are not guaranteed to be homogeneous</description>
  <decision_tree name="when_to_use">
    <question>Does the test iterate a registry, plugin list, or handler table defined by production code and then read a property off each member?</question>
    <if_yes>Guard the read with a predicate for the variant that actually carries that property, and assert the guard matched at least once</if_yes>
    <if_no>Assert against a fixed, test-owned list</if_no>
  </decision_tree>
  <example>
    <note>Guard the variant-specific read; an unguarded iteration tests a meaningless scenario for members of the other variant</note>
    for (const member of registry) {
      if (!hasCommandForm(member)) continue
      assertValidCommand(member)
    }
  </example>
  <note>A docstring or comment that narrows a registry's contract ("members must be created through this constructor") is documentation, not enforcement. A second variant registered through a different path will eventually appear, and every consumer that reads a property only the first variant carries then silently degrades: the loop still runs, the assertion still passes, and it proves nothing about the members it skipped over</note>
  <note>When the same inline property-presence check appears at several call sites across tests and production alike, the duplication is the signal that a named predicate is missing. Introduce the predicate once and let both sides discriminate through it, so a third variant has one place to teach</note>
  <note>Pair the guard with a count assertion so a registry whose members all fail the guard cannot pass as a fully-skipped loop; see the non-vacuity guidance in test-integrity</note>
  <use_case>Plugin and checker registries, handler tables, tests that sweep every registered member of a production list</use_case>
</pattern>

<pattern name="namespaced_generated_test_names">
  <description>When a macro generates globally-registered test names from a caller-supplied label, namespace the label so names cannot collide across files</description>
  <decision_tree name="when_to_use">
    <question>Does a table- or case-generating macro derive globally-registered test names from a bare label?</question>
    <if_yes>Prefix the label with the module or subject, because the enclosing describe or context block is usually not part of the generated name</if_yes>
    <if_no>No action needed for inline, locally-named tests</if_no>
  </decision_tree>
  <example>
    <note>Module-prefixed label avoids a silent collision with an identically-named table elsewhere</note>
    deftest-table server-strip-annotation-cases ...   // not: strip-annotation-cases
  </example>
  <note>Two generators sharing a derived name across files can silently shadow or overwrite each other's registrations, so one suite's cases quietly vanish with no failure. Uniqueness must hold across the whole suite, not just within a file</note>
  <use_case>Data-driven test generators, table or case macros that register names globally</use_case>
</pattern>

<pattern name="contract_complete_test_double">
  <description>A mock or fake must implement the full observable contract of what it replaces, including destructive or ordering semantics, not only the return value</description>
  <decision_tree name="when_to_use">
    <question>Are you replacing a dependency whose real behavior has side effects beyond returning a value (it deletes a region, consumes input, mutates a buffer, removes a key)?</question>
    <if_yes>Reproduce those side effects in the double, or the test passes against behavior that cannot occur in production</if_yes>
    <if_no>A value-only stub is sufficient</if_no>
  </decision_tree>
  <example>
    <note>The real call deletes its input range before writing output; the double must too</note>
    (lambda (beg end &amp;rest _) (delete-region beg end) (insert "output") 0)
    <note>A double that only inserts leaves stale input in place and hides the bug</note>
  </example>
  <note>Under-modeled doubles are a common source of tests that are green yet meaningless: they assert against a fiction. When the contract includes deletion, consumption, or ordering, the double owns those semantics</note>
  <use_case>Process and IO shims, store deletion semantics, buffer-mutating calls</use_case>
</pattern>

<pattern name="registry_derived_double_set">
  <description>When a test replaces every member of a dispatch chain, derive the set of doubles from the production registry, or make an omission fail loudly; do not enumerate the members by hand</description>
  <decision_tree name="when_to_use">
    <question>Does a test file replace all members of a chain or pipeline in order to isolate dispatch ordering?</question>
    <if_yes>Build the double set from the same list production dispatches over, or assert the two sets are equal before running</if_yes>
    <if_no>Replace only the collaborator under examination</if_no>
  </decision_tree>
  <example>
    <note>Derive rather than list: a new production member is covered automatically</note>
    for (const handler of productionChain) install(doubleFor(handler))

    <note>Or make the gap explicit rather than letting the real implementation through</note>

    assertSetEqual(doubled.keys(), productionChain.map(id), "every chain member must be doubled")
  </example>
  <note>A hand-enumerated double set quietly becomes a second registration list that nobody knows they own. Adding a member to the production chain leaves that member undoubled, so the real implementation runs against empty stubs — and the resulting failures land in unrelated cases elsewhere in the file, naming neither the new member nor the file that needed updating</note>
  <note>This is about the completeness of a set of doubles, which is a different failure from the fidelity of any single one; contract_complete_test_double covers the latter. A set can be perfectly faithful member by member and still be wrong because it is missing one</note>
  <use_case>Handler and middleware chains, dispatch tables, plugin pipelines isolated for ordering tests</use_case>
</pattern>

<pattern name="distinguishing_fixture_values">
  <description>Choose fixture inputs that keep every value the code derives from them distinct, and that start strictly on the far side of any threshold under test</description>
  <decision_tree name="when_to_use">
    <question>Does the code under test derive several samples, indices, or positions from a single fixture input, or step toward a threshold?</question>
    <if_yes>Pick an input for which the derived values are provably distinct, and begin more than one step away from the boundary</if_yes>
    <if_no>Use the simplest legal value</if_no>
  </decision_tree>
  <example>
    <note>An integral coordinate collapses two derived samples onto one cell; a fractional one separates them</note>
    position.y = 64.3     // lower sample resolves to 64, upper to 65 -- not: 64

    <note>Start strictly beyond the threshold so one step lands past it rather than exactly on it</note>

    startAt(threshold + 2 * step)
  </example>
  <note>A legal but degenerate fixture value produces a test that appears to cover several paths while exercising one. It fails silently in both directions: the collapsed path is never checked, and the surviving path passes, so the suite reports coverage of a condition it never reached</note>
  <note>Do not feed an already-normalized fixture constant back through its normalizer. Converting a converted value commonly yields nothing at all, and the fixture then falls back to its default, so the scenario the test claims to arrange was never built — the assertion runs against an empty or default subject</note>
  <note>An advance that lands exactly on a boundary tests the wrong side of a strict comparison. Starting one step further out makes the crossing unambiguous regardless of whether the production comparison is strict or inclusive</note>
  <use_case>Coordinate and index fixtures, threshold-crossing and accumulation tests, fixtures built from enumeration-to-index conversions</use_case>
</pattern>

<pattern name="restart_round_trip_fixture">
  <description>To prove data survives a restart, restart against the same persistent profile directory; a fresh isolated context proves nothing about durability</description>
  <decision_tree name="when_to_use">
    <question>Is the behavior under test "state written in one session is still there in the next"?</question>
    <if_yes>Launch a persistent context bound to an explicit profile directory, poll for the persisted record, close it, and launch a second context against the same directory</if_yes>
    <if_no>An ordinary isolated context is fine and faster</if_no>
  </decision_tree>
  <example>
    <note>The same profile directory across two sessions is what makes the round trip real</note>
    ctx = launchPersistentContext(profileDir); save(); await waitForPersistedRecord(); await ctx.close()
    ctx2 = launchPersistentContext(profileDir); assertRecordPresent()
  </example>
  <note>A default per-test context starts with empty storage regardless of whether the write ever succeeded. A test asserting presence can then only fail, never falsely pass — but the mirror-image test, asserting that data is gone after a reset, passes vacuously every time and will never detect a broken reset</note>
  <note>Poll for the persisted record before closing the first session. Closing immediately after the save call races the storage layer's flush, and the resulting failure looks like a durability bug in the code under test rather than a missing barrier in the harness</note>
  <note>Give each run its own temporary profile directory so parallel runs do not share state, and remove it in a cleanup block that runs even when the body throws; otherwise a failed run leaves a populated profile that makes the next run pass for the wrong reason</note>
  <use_case>Client-side storage durability, session and cache persistence, reset and sign-out flows</use_case>
</pattern>

<pattern name="legacy_shape_regression_test">
  <description>When adding an optional element to an existing contract, pin backward compatibility with a test that makes the old call and asserts the old result shape</description>
  <decision_tree name="when_to_use">
    <question>Are you adding an optional parameter, key, or field to a contract that existing callers already use?</question>
    <if_yes>Write one test that calls the entry point without the addition and asserts the pre-existing output shape and decision fields are unchanged</if_yes>
    <if_no>Cover the new behavior with ordinary tests</if_no>
  </decision_tree>
  <example>
    <note>The new-feature tests all pass the new argument, so none of them ever exercises the old call shape</note>
    result = entryPoint(existingArgs)          // no new argument at all
    assertShapeEqual(result, legacyShape)
  </example>
  <note>Omitting the addition and passing it explicitly as null must produce identical behavior and identical output shape. When they differ, callers acquire an invisible dependency on argument-passing style, and the difference surfaces later as a bug in a caller that did nothing wrong</note>
  <note>Exactly one layer owns the decision to forward the new key. When two layers each conditionally append it, the result carries the key twice, and which one wins depends on the consumer's parsing order</note>
  <note>Prefer a stable output shape — the key always present, sometimes null — over a key that appears and disappears. A shape that varies forces every consumer and every test to handle both forms, and the branch that handles the absent form is the one that goes untested</note>
  <use_case>Optional-parameter additions to widely-called entry points, additive schema evolution, decision records gaining a new field</use_case>
</pattern>

<pattern name="evaluation_as_acceptance_gate">
  <description>In a declarative-configuration repository with no runtime test suite, successful evaluation and build of every affected output is the acceptance gate, supplemented by a repository-wide search for what the change was supposed to remove</description>
  <decision_tree name="when_to_use">
    <question>Does the repository describe desired state declaratively, with no place a unit test could meaningfully attach?</question>
    <if_yes>Build each affected output as the gate, then grep the whole repository for the identifier being removed or renamed</if_yes>
    <if_no>Write ordinary tests</if_no>
  </decision_tree>
  <example>
    <note>Build every output the change touches, not just the one you edited</note>
    build each affected target; a non-zero exit is the failing test

    <note>Then confirm the old path is gone, which the build cannot tell you</note>

    grep the repository for the removed identifier; zero hits is the second half of the gate
  </example>
  <note>Building is a strictly stronger gate than evaluating. Some errors — a duplicated module argument, a conflicting option definition — surface only when the output is realized, so an evaluation-only check reports success on a configuration that cannot build</note>
  <note>The search half is the part people skip and the part that catches real defects. A successful build proves the new path works; it says nothing about whether the old path was fully removed, so a migration can leave both installed and appear entirely healthy</note>
  <note>State the gate as an enumerated list of commands that must exit zero. "It builds" is not checkable by a reviewer; a list of targets is</note>
  <use_case>Declarative infrastructure and machine configuration, manifest-driven deployments, provisioning repositories, any codebase whose product is configuration rather than a running program</use_case>
</pattern>

<pattern name="structured_failure_payload">
  <description>Model assertion outcomes as structured failure values (or conditions), not bare booleans, so a failure reports what was expected, what was observed, and where</description>
  <decision_tree name="when_to_use">
    <question>Are you building assertion or matcher infrastructure rather than a single test?</question>
    <if_yes>Emit a structured outcome (expected, actual, location, message) and let the runner render or convert it; where the host language has a condition system, signal a typed failure and expose named restarts</if_yes>
    <if_no>Use the framework's existing assertions</if_no>
  </decision_tree>
  <example>
    <note>A structured outcome explains itself; a boolean cannot</note>
    fail({ expected, actual, at: location, message })
    <note>In a language with conditions and restarts, signal a typed failure and install continue, skip, and retry restarts around the body</note>
  </example>
  <note>A runner that converts typed failures to ordinary events when no outer handler intervenes stays deterministic in CI while still letting an interactive handler inspect the live condition before conversion. A retry restart that reruns the attempt without consuming the configured retry budget, and cleanup placed under an unwind guarantee, keep this control flow predictable. Boolean assertions discard that structure and force the reader back to the source to reconstruct intent</note>
  <use_case>Custom matchers, assertion libraries, runners with retry, skip, or interactive-restart support</use_case>
</pattern>

<pattern name="single_step_matcher">
  <description>Keep each matcher a single deterministic transformation from actual value to verdict; compose matchers rather than embedding branching or side effects in one</description>
  <decision_tree name="when_to_use">
    <question>Are you authoring a matcher or custom assertion?</question>
    <if_yes>Make it one pure step with no hidden state or ordering dependence, and build complex checks by composing simple matchers</if_yes>
    <if_no>Not applicable</if_no>
  </decision_tree>
  <example>
    <note>Deterministic single step: the same input always yields the same verdict and message</note>
    hasStatus(expected) => actual => actual.status === expected ? pass() : fail({ expected, actual: actual.status })
  </example>
  <note>Matchers that branch on external state or mutate as a side effect become order-dependent and hard to reason about; a matcher should be a referentially transparent verdict function</note>
  <use_case>Matcher libraries, fluent assertion DSLs</use_case>
</pattern>

<pattern name="one_transition_per_settle_step">
  <description>When a scenario drives several dependent state transitions, settle each one separately instead of batching them into a single flush; a batched update collapses the intermediate renders and the assertion reads the value from one transition ago</description>
  <decision_tree name="when_to_use">
    <question>Does the test perform update B whose input is the result of update A, inside one settle or flush boundary?</question>
    <if_yes>Split them into separate steps, so B is dispatched against A's committed result rather than against the state captured when the batch opened</if_yes>
    <if_no>Independent updates may share a settle step</if_no>
  </decision_tree>
  <note>The companion trap is a shared render or bootstrap helper that leaves the subject in a pre-ready state. Assertions then target markup that was never mounted, and the failure reads as a missing feature rather than a setup gap. When a scenario depends on a particular startup state, set that state explicitly instead of relying on the helper's default</note>
  <use_case>Hook and component tests with batched update semantics, reducer sequences, any framework where updates within one flush boundary are coalesced</use_case>
</pattern>

<pattern name="fixture_adequacy_before_logic">
  <description>Code guarded by a chain of preconditions returns its null result for any unsatisfied one, so a fixture that satisfies the obvious gate but not a second one produces a null indistinguishable from a logic bug</description>
  <decision_tree name="when_to_use">
    <question>Did a test unexpectedly return nothing, or return the empty or default result?</question>
    <if_yes>Enumerate every gate on the path and confirm the fixture satisfies all of them before touching the implementation. Prefer building the fixture from the same helper the passing tests use rather than assembling a plausible-looking one by hand</if_yes>
    <if_no>Proceed with ordinary triage</if_no>
  </decision_tree>
  <note>The outcome this prevents is the worst one available: changing correct production code to accommodate an inadequate fixture. Weakening a guard makes the test pass and removes the behavior the guard existed for. This is the triage-order rule; the authoring-side rule — assert in the test that the precondition holds, so a silently-unsatisfied gate cannot masquerade as a pass — belongs to test-integrity</note>
  <use_case>Strategy and rule engines, request pipelines with authorization and validation gates, any function whose null return is overloaded across several rejection reasons</use_case>
</pattern>

<pattern name="test_framework_authoring_hazards">
  <description>Hazards that appear when building or extending a test framework rather than using one. Each presents as slowness or a hang rather than as a failure, which is why they survive so long</description>
  <hazard name="trial_loop_ownership">A property-trial loop needs exactly one owner. Nesting iteration in both the runner and the property macro multiplies the trial count silently — fifty by fifty becomes two and a half thousand — and presents as an inexplicably slow suite, never as an error</hazard>
  <hazard name="bounded_generator_failure">A generator combinator constrained by a predicate must reject an impossible or empty domain with a bounded failure after a finite, configurable number of attempts. An unsatisfiable predicate otherwise becomes an unbounded hang, which in CI is indistinguishable from an infrastructure stall</hazard>
  <note>Name re-registration is the third hazard in this family, and it is a test-integrity concern rather than a design one — see that skill's registration_collisions_remove_tests, which owns both the helper-file and legacy-cases-beside-a-generator sources. namespaced_generated_test_names above covers the distinct case of two generators deriving the same name</note>
  <use_case>Custom property-testing DSLs, in-house runners, table macros, framework extensions</use_case>
</pattern>

<pattern name="delete_the_forwarding_test_helper">
  <description>Test helper layering earns its place by carrying orchestration, not by pre-binding arguments. An alias that only forwards to a canonical builder is pure indirection, and shared setup extracted for a single caller is premature</description>
  <decision_tree name="when_to_use">
    <question>Does the helper do anything beyond forwarding to another helper with some arguments fixed?</question>
    <if_yes>Keep it</if_yes>
    <if_no>Delete it and call the canonical builder directly, passing the arguments at the call site where a reader can see them</if_no>
  </decision_tree>
  <note>Where a taxonomy helps — transport doubles, shared assertions, scenario setup, fixed fixtures each in their own module — treat it as a convention rather than a rule. The restraint half is the durable part: do not lift a scenario setup into a shared helper until several specs genuinely share the same bootstrap, because a shared helper with one caller is a second place to read before understanding the first</note>
  <use_case>Test support directories, scenario builders, feature-local helper modules</use_case>
</pattern>

</patterns>

<best_practices>
  <practice priority="critical">
    <name>Test happy path first</name>
    <description>Start with the normal, expected flow before edge cases</description>
    <example>
      test_userLogin_withValidCredentials_succeeds
      test_userLogin_withInvalidPassword_fails
      test_userLogin_withLockedAccount_fails
    </example>
  </practice>

  <practice priority="critical">
    <name>Test edge cases</name>
    <description>Test boundary conditions and limits</description>
    <example>
      Empty inputs, maximum values, null values, zero values, negative numbers
    </example>
  </practice>

  <practice priority="critical">
    <name>Test error cases</name>
    <description>Verify error handling paths work correctly</description>
    <example>
      Invalid inputs, network failures, permission errors, timeout scenarios
    </example>
  </practice>

  <practice priority="critical">
    <name>Ground expected values in evidence</name>
    <description>Base every expected value on a primary source (requirements, spec, or source code); when a value cannot be verified, mark it explicitly instead of guessing</description>
    <example>
      <note>Good: expectation traceable to its basis</note>
      test_calculateTotal_appliesTenPercentDiscount_perSpecSection4

      <note>Unverifiable: state it, do not fabricate</note>

      expected total: unverified -- requires-analysis
    </example>
  </practice>

  <practice priority="high">
    <name>Isolate tests</name>
    <description>Each test should be independent</description>
    <example>
      <note>Use setup/teardown to reset state</note>
      def setup
        @database = TestDatabase.new
        @service = UserService.new(@database)
      end

      def teardown
        @database.clear
      end
    </example>
  </practice>

  <practice priority="high">
    <name>Make tests readable</name>
    <description>Tests serve as documentation</description>
    <example>
      <note>Good: Clear and descriptive</note>
      test_userRegistration_withExistingEmail_returnsError

      <note>Bad: Unclear purpose</note>

      test_user_reg_1
    </example>
  </practice>

  <practice priority="high">
    <name>One assertion per concept</name>
    <description>Each test should verify one logical concept</description>
    <example>
      <note>Good: Single concept</note>
      test_userCreation_setsDefaultRole
        user = create_user
        assert_equal "member", user.role
      end

      <note>Avoid: Multiple unrelated assertions</note>

      test_userCreation
        user = create_user
        assert_equal "member", user.role
        assert_not_nil user.email
        assert_true user.active
      end
    </example>
  </practice>

  <practice priority="medium">
    <name>Use test fixtures and factories</name>
    <description>Extract common test data setup</description>
    <example>
      <note>Create reusable test data</note>
      def create_test_user(overrides = {})
        defaults = {
          name: "Test User",
          email: "test@example.com",
          role: "member"
        }
        User.new(defaults.merge(overrides))
      end
    </example>
  </practice>

  <practice priority="high">
    <name>Suspect the fixture before the implementation</name>
    <description>When a test unexpectedly gets nothing back — a null result, an empty collection, no side effect — verify the fixture satisfies every gate on the path before changing production logic</description>
    <example>
      <note>Code behind a chain of preconditions returns its null result for any unsatisfied one, so the symptom names none of them</note>
      enumerate the guards on the path: session gate, feature-flag gate, permission gate
      confirm the fixture satisfies all of them, not only the obvious first

      <note>A generic fixture can satisfy the visible precondition and miss a second one; prefer the same builder the neighboring passing tests use</note>

      <note>Worst outcome of skipping this check: "fixing" correct production code to accommodate an inadequate fixture</note>
    </example>
  </practice>

  <practice priority="medium">
    <name>Layer test helpers, and delete the ones that add nothing</name>
    <description>Group test support by role — transport doubles, shared assertions, scenario setup, fixed fixtures — and delete any helper that only pre-binds arguments to another helper</description>
    <example>
      <note>Delete: a wrapper that forwards to a canonical builder with fixed arguments</note>
      def setup_ready_session = build_session(state: :ready)   // callers should call build_session directly

      <note>Keep: a helper that performs real orchestration callers would otherwise repeat</note>

      def setup_ready_session = { s = build_session(state: :ready); attach_transport(s); await_ready(s); s }
    </example>
    <example>
      <note>Do not lift a scenario setup into a shared module until several specs genuinely share the same bootstrap</note>
      <note>Prefer a small helper local to the file over broader shared plumbing extracted speculatively</note>
    </example>
  </practice>

  <practice priority="medium">
    <name>Avoid magic numbers</name>
    <description>Use named constants for test values</description>
    <example>
      <good_example>Good</good_example>
      VALID_USER_AGE = 25
      MINIMUM_AGE = 18
      test_userValidation_withValidAge_succeeds
        user = User.new(age: VALID_USER_AGE)
        assert user.valid?
      end

      <bad_example>Bad</bad_example>

      test_userValidation_withValidAge_succeeds
        user = User.new(age: 25)
        assert user.valid?
      end
    </example>
  </practice>

  <practice priority="medium">
    <name>Test corner cases</name>
    <description>Test unusual combinations and scenarios</description>
    <example>
      Concurrent access, timezone edge cases, leap years, DST transitions
    </example>
  </practice>
</best_practices>

<anti_patterns>
  <avoid name="testing_implementation">
    <description>Testing implementation details instead of behavior</description>
    <instead>Focus on testing observable behavior and outcomes, not internal implementation details. Test what the code does, not how it does it.</instead>
  </avoid>

  <avoid name="excessive_mocking">
    <description>Over-mocking dependencies throughout test suites</description>
    <instead>Use real implementations where practical; excessive mocking often indicates poor design. Only mock external dependencies or slow operations.</instead>
  </avoid>

  <avoid name="flaky_tests">
    <description>Tests that sometimes pass and sometimes fail</description>
    <instead>Ensure tests are deterministic by controlling time, randomness, and async operations. Use fixed timestamps, seeded random generators, and proper async handling.</instead>
  </avoid>

  <avoid name="slow_tests">
    <description>Tests that take too long to run</description>
    <instead>Use unit tests for fast feedback; reserve slow integration/e2e tests for critical paths. Unit tests should run in milliseconds, not seconds.</instead>
  </avoid>

  <avoid name="test_interdependence">
    <description>Tests that depend on execution order or shared state</description>
    <instead>Make each test independent with proper setup/teardown and isolated state. Each test should create its own test data.</instead>
  </avoid>

  <avoid name="fabricated_expectations">
    <description>Filling in expected values by guessing when they cannot be verified against a source</description>
    <instead>State unknowns explicitly (mark as unverified or requires-analysis) and cite the primary source for known expectations. Do not fabricate assertions to make a test look complete.</instead>
  </avoid>

  <avoid name="execution_over_design">
    <description>Using AI merely to run tests while neglecting exhaustive test-case design and perspective coverage</description>
    <instead>Use AI as a test designer first: enumerate scenarios across adversarial perspectives before execution. Design coverage, then run.</instead>
  </avoid>

  <avoid name="throw_to_retry_waiting">
    <description>Waiting for an async outcome by repeatedly asserting and catching the failure until it eventually passes</description>
    <instead>Poll a definitive completion source at a fixed interval with an explicit timeout budget; reserve the exception for the final timeout, not for every intermediate attempt.</instead>
  </avoid>

  <avoid name="truncate_between_tests">
    <description>Resetting a shared persistent store by truncating tables between tests</description>
    <instead>Tag created data with a scenario-unique id and delete by that id, so cleanup is surgical, order-independent, and safe under parallel execution.</instead>
  </avoid>

  <avoid name="fail_on_missing_dependency">
    <description>Letting a test fail when a required external dependency is simply unavailable in the current environment</description>
    <instead>Probe availability and skip with a stated reason; keep failure meaning "expectation violated", not "precondition absent".</instead>
  </avoid>

  <avoid name="partial_state_restore">
    <description>A fixture that restores some but not all of the global state its body mutates</description>
    <instead>Enumerate the complete write set of the body and snapshot/restore every binding under an unwind guarantee; a single leaked binding pollutes later tests.</instead>
  </avoid>

  <avoid name="under_modeled_double">
    <description>A mock or fake that returns the right value but omits the real dependency's side effects such as deletion, consumption, or ordering</description>
    <instead>Implement the full observable contract in the double, or the test verifies behavior that cannot occur in production.</instead>
  </avoid>

  <avoid name="loosened_assertion_for_race">
    <description>Answering a settlement race by widening a tolerance, accepting a range instead of an exact value, or dropping the durability check</description>
    <instead>Strengthen the barrier: wait for every observable effect in order and poll a second, authoritative source. Loosening converts an intermittent failure into a permanent blind spot, and the race it was hiding stays in production.</instead>
  </avoid>

  <avoid name="wall_clock_performance_assertion">
    <description>Guarding an efficiency property with an elapsed-time threshold</description>
    <instead>Assert the number of operations — queries, commits, flushes — at several input sizes. Time thresholds are machine-dependent, get widened after every spurious failure, and end up detecting nothing.</instead>
  </avoid>

  <avoid name="global_function_rebinding">
    <description>Substituting a collaborator by overwriting its global function definition for the duration of a test</description>
    <instead>Bind a seam the production code declares — a dynamic variable, a strategy slot, a constructor parameter. Global rebinding is process-wide and unscoped, so under a parallel runner it corrupts unrelated tests in ways nobody can attribute.</instead>
  </avoid>

  <avoid name="hand_enumerated_double_set">
    <description>A test file that lists every member of a production dispatch chain by hand in order to replace them all</description>
    <instead>Derive the double set from the production list, or assert the two sets are equal before running. A hand-written list is a second registration list nobody owns, and a new production member surfaces as failures in unrelated cases.</instead>
  </avoid>

  <avoid name="degenerate_fixture_value">
    <description>A fixture input that is legal but collapses several derived values onto one, or that starts exactly on the threshold the test means to cross</description>
    <instead>Choose an input whose derived samples are provably distinct and start strictly beyond the boundary. A degenerate value makes one path masquerade as several, and nothing about the result reveals it.</instead>
  </avoid>

  <avoid name="reset_instead_of_restore">
    <description>Cleaning up by resetting shared state to a known default, or by copying the originals at teardown after the body has already mutated them</description>
    <instead>Snapshot at setup and restore the prior values. A reset destroys legitimate pre-existing state and makes the suite order-dependent; a teardown-time copy publishes the damage rather than undoing it.</instead>
  </avoid>

  <avoid name="helper_alias_wrapper">
    <description>A test helper that exists only to call another helper with fixed arguments</description>
    <instead>Delete the wrapper and let tests call the canonical builder directly. Extract shared setup only once several specs genuinely need the same bootstrap, and keep single-use helpers local to their file.</instead>
  </avoid>

  <avoid name="indiscriminate_failure_monitor">
    <description>An E2E harness that treats every logged error as a test failure</description>
    <instead>Classify fatal signals explicitly — uncaught exceptions, unhandled rejections, named fatal error classes — and let ordinary non-fatal application logging pass. A monitor that fires on routine logs becomes noise the team mutes, at which point it detects nothing.</instead>
  </avoid>
</anti_patterns>

<tooling_traps>
  <description>Environment- and toolchain-level failure modes that silently invalidate otherwise well-designed tests. These are not test-design mistakes; they are traps in how the runner, compiler, or server is wired, and each can make a green result meaningless or a red result misleading.</description>

  <trap name="stale_compiled_artifact_shadowing">
    <symptom>A source edit appears to have no effect, or a test keeps failing or passing against behavior that no longer matches the code</symptom>
    <cause>A previously compiled output sits beside the source and the resolver picks it up first. A bundler-based runner resolving an extensionless relative import tries the compiled .js before the .ts by default, so a stale .js next to an edited .ts is exercised instead of the new source</cause>
    <mitigation>Regenerate or delete the stale artifact, or import the intended file with an explicit extension, before trusting a runner result. When a change "does nothing", suspect a shadowing artifact before suspecting the test</mitigation>
    <verified_note>Default bundler resolution order places .js ahead of .ts for extensionless imports, so identically-named siblings resolve to the compiled file first</verified_note>
  </trap>

  <trap name="global_coverage_gate_vs_subset_run">
    <symptom>A focused run of a few test files fails a coverage threshold even though the files under test are fully covered</symptom>
    <cause>A global coverage threshold is evaluated over the aggregate of only the files measured in that run. A subset run measures a distorted slice, because files imported but not exercised register as uncovered, so the aggregate can fall below a gate the full suite would satisfy</cause>
    <mitigation>Run the full suite before trusting a global gate, or configure per-file thresholds so each measured file is judged on its own coverage rather than on a distorted global average. Change global coverage exclusions only deliberately</mitigation>
    <verified_note>Global thresholds check the aggregate of measured files, so subset runs can fail gates the whole suite would pass unless per-file thresholds are used</verified_note>
  </trap>

  <trap name="reused_dev_server_breaks_mock_assumptions">
    <symptom>A browser E2E test that expects a mock-backed response gets real or stale responses instead, and only in local runs</symptom>
    <cause>The runner is configured to reuse an already-running server on the target URL, a common local-only setting. When any independently-started server occupies that port, the runner reuses it and never launches the mock-backed command the test assumes, so the test exercises the wrong backend</cause>
    <mitigation>Ensure no unrelated server holds the port before a mock-dependent run, or disable server reuse for suites whose correctness depends on the launched command; keep reuse enabled only where it is safe. In CI, start fresh so a stale process cannot make the suite pass for the wrong reason</mitigation>
    <verified_note>Server-reuse settings reuse an independently-running server on the URL and skip the configured start command; the typical guard enables reuse only outside CI</verified_note>
  </trap>

  <trap name="batched_updates_collapse_intermediate_state">
    <symptom>A multi-step component or hook test asserts a value from one transition ago, and the assertion fails against markup that looks correct in the running application</symptom>
    <cause>The render-batching wrapper that component-testing libraries provide (the act-style helper) flushes all updates enqueued inside it as one pass. Dependent state transitions written inside a single wrapper collapse their intermediate renders, so the second transition computes from the state the first one was replacing rather than from its result</cause>
    <mitigation>Give each dependent transition its own wrapper call, or its own step in a case sequence, so the rerender boundary between them is real. Independent updates may share one wrapper; dependent ones may not</mitigation>
    <verified_note>Updates enqueued within one batching wrapper are flushed together and intermediate renders are not observable, which is the intended semantic rather than a defect</verified_note>
  </trap>

  <trap name="shared_render_helper_leaves_preliminary_state">
    <symptom>An application-level test asserts against markup that was never mounted, and the failure names an element that does exist elsewhere in the codebase</symptom>
    <cause>A shared render helper mounts the root in its initial, pre-ready state — a loading or splash phase driven by a mocked startup value. Assertions written for the ready view then target markup the component never reached, because nothing in the helper advanced the startup state</cause>
    <mitigation>Set the startup state explicitly in the test when the assertions depend on the ready view, rather than assuming the shared helper leaves the tree in a usable phase. Read what a shared render helper actually produces before building assertions on top of it</mitigation>
  </trap>
</tooling_traps>

<rules priority="standard">
  <rule>Aim for high coverage but prioritize meaningful tests over coverage numbers</rule>
  <rule>80%+ coverage is a good target for critical code paths</rule>
  <rule>100% coverage does not guarantee bug-free code</rule>
  <rule>Focus on testing behavior, not achieving coverage metrics</rule>
  <rule>Prefer project conventions over generic defaults</rule>
</rules>

<error_escalation>
  <examples>
    <example severity="low">Minor coverage gap in non-critical path</example>
    <example severity="medium">Test flakiness detected</example>
    <example severity="high">Critical path lacks test coverage</example>
    <example severity="critical">Tests reveal security vulnerability</example>
  </examples>
</error_escalation>

<related_agents>
  <agent name="explore">Locate relevant code patterns</agent>
  <agent name="quality-assurance">Review output consistency</agent>
</related_agents>

<constraints>
  <must>Follow project test patterns</must>
  <must>Run tests after creation</must>
  <must>Cover critical paths first</must>
  <avoid>Creating tests without understanding implementation</avoid>
  <avoid>Writing flaky or non-deterministic tests</avoid>
  <avoid>Ignoring existing test conventions</avoid>
</constraints>

<related_skills>
  <skill name="requirements-definition">Use to define test requirements and acceptance criteria</skill>
  <skill name="execution-workflow">Use to implement tests as part of feature development workflow</skill>
  <skill name="investigation-patterns">Use when debugging test failures or flaky tests</skill>
  <skill name="performance-benchmarking">Use when the question is how fast something is rather than how to test it: benchmark methodology, measurement noise, warm-up, and statistical comparison of timings. The rule that a test asserts operation counts rather than elapsed time lives here, in operation_count_assertion; that skill points at it rather than restating it</skill>
  <skill name="test-integrity">Use when the concern is whether a passing test proves anything: tests that never ran, vacuous or always-true assertions, guards nothing crosses, teardown failures that mask body failures, and auditing a suite for non-vacuity. This skill covers test design, fixtures, isolation, settlement, and suite classification; that one covers whether the resulting green is trustworthy</skill>
</related_skills>
