---
name: Testing Patterns
description: This skill should be used when the user asks to "write tests", "test strategy", "coverage", "unit test", "integration test", or needs testing guidance. Provides testing methodology and patterns.
version: 2.1.0
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
</concept>

<concept name="integration">
  <description>Test interaction between components</description>
  <scope>Multiple components working together</scope>
  <characteristics>Slower, may use real dependencies</characteristics>
  <when>API endpoints, database operations, service interactions</when>
</concept>

<concept name="e2e">
  <description>Test complete user workflows</description>
  <scope>Full application stack</scope>
  <characteristics>Slowest, tests real user scenarios</characteristics>
  <when>Critical user journeys, smoke tests</when>
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
  <use_case>Registry mutations, dynamic-variable overrides, environment-variable tests, hash-table caches</use_case>
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
</tooling_traps>

<rules priority="standard">
  <rule>Aim for high coverage but prioritize meaningful tests over coverage numbers</rule>
  <rule>80%+ coverage is a good target for critical code paths</rule>
  <rule>100% coverage does not guarantee bug-free code</rule>
  <rule>Focus on testing behavior, not achieving coverage metrics</rule>
  <rule>Prefer project conventions over generic defaults</rule>
</rules>

<error_escalation inherits="core-patterns#error_escalation">
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
</related_skills>
