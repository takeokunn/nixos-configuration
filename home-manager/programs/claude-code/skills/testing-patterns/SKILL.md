---
name: Testing Patterns
description: This skill should be used when the user asks to "write tests", "test strategy", "coverage", "unit test", "integration test", or needs testing guidance. Provides testing methodology and patterns.
---

<purpose>
  Provide testing patterns and strategies for comprehensive test coverage and maintainable test suites.
</purpose>

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

<rules priority="standard">
  <rule>Aim for high coverage but prioritize meaningful tests over coverage numbers</rule>
  <rule>80%+ coverage is a good target for critical code paths</rule>
  <rule>100% coverage does not guarantee bug-free code</rule>
  <rule>Focus on testing behavior, not achieving coverage metrics</rule>
</rules>

<related_agents>
  <agent name="execute">Primary agent for implementing tests alongside feature code</agent>
  <agent name="feedback">Use for reviewing test quality and coverage</agent>
  <agent name="bug">Delegate to when tests reveal unexpected failures</agent>
</related_agents>

<related_skills>
  <skill name="requirements-definition">Use to define test requirements and acceptance criteria</skill>
  <skill name="execution-workflow">Use to implement tests as part of feature development workflow</skill>
  <skill name="investigation-patterns">Use when debugging test failures or flaky tests</skill>
</related_skills>

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
</anti_patterns>

<workflow>
  <phase name="analyze">
    <objective>Analyze testing requirements</objective>
    <step>1. Identify code to be tested</step>
    <step>2. Review existing test patterns in project</step>
    <step>3. Determine appropriate test types</step>
  </phase>
  <phase name="design">
    <objective>Design test strategy</objective>
    <step>1. Identify test scenarios and edge cases</step>
    <step>2. Plan unit, integration, and e2e coverage</step>
    <step>3. Design mocking strategy</step>
  </phase>
  <phase name="implement">
    <objective>Implement and verify tests</objective>
    <step>1. Write tests following project patterns</step>
    <step>2. Run tests to verify they pass</step>
    <step>3. Verify coverage meets requirements</step>
  </phase>
</workflow>

<error_escalation>
  <level severity="low">
    <example>Minor coverage gap in non-critical path</example>
    <action>Note in report, proceed</action>
  </level>
  <level severity="medium">
    <example>Test flakiness detected</example>
    <action>Document issue, use AskUserQuestion for clarification</action>
  </level>
  <level severity="high">
    <example>Critical path lacks test coverage</example>
    <action>STOP, present options to user</action>
  </level>
  <level severity="critical">
    <example>Tests reveal security vulnerability</example>
    <action>BLOCK operation, require explicit user acknowledgment</action>
  </level>
</error_escalation>

<constraints>
  <must>Follow project test patterns</must>
  <must>Run tests after creation</must>
  <must>Cover critical paths first</must>
  <avoid>Creating tests without understanding implementation</avoid>
  <avoid>Writing flaky or non-deterministic tests</avoid>
  <avoid>Ignoring existing test conventions</avoid>
</constraints>
