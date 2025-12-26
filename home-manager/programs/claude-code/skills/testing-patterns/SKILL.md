---
name: Testing Patterns
description: This skill should be used when the user asks to "write tests", "test strategy", "coverage", "unit test", "integration test", or needs testing guidance. Provides testing methodology and patterns.
version: 0.1.0
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
<example>
# Arrange: Set up test data and preconditions
user = User.new(name: "John")
cart = ShoppingCart.new(user)

# Act: Execute the code under test

total = cart.calculate_total

# Assert: Verify expected outcomes

assert_equal 0, total
</example>
<note>Separates setup, execution, and verification into distinct phases</note>
</pattern>

<pattern name="given_when_then">
<description>BDD-style test structure focusing on behavior</description>
<example>
# Given: Initial context (preconditions)
given_a_user_with_an_empty_cart

# When: Action or trigger

when_the_user_calculates_total

# Then: Expected outcome

then_the_total_should_be_zero
</example>
<note>Emphasizes business behavior over technical implementation</note>
</pattern>

<pattern name="stub">
<description>Provide canned responses for dependencies</description>
<example>
api_client = stub(
  fetch_user: { id: 1, name: "John" }
)
</example>
<use_case>Replace slow/unreliable dependencies</use_case>
</pattern>

<pattern name="mock">
<description>Verify interactions occurred with dependencies</description>
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
<example>
logger = spy(Logger.new)
service.process(logger)
assert_called logger, :log, with: "Processing complete"
</example>
<use_case>Verify side effects without changing behavior</use_case>
</pattern>

<pattern name="fake">
<description>Working implementation suitable for testing</description>
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
<example>
test_calculateTotal_withEmptyCart_returnsZero
test_calculateTotal_withMultipleItems_returnsSumOfPrices
test_calculateTotal_withDiscount_appliesDiscountCorrectly
</example>
<note>Format: test_[method]_[scenario]_[expected_result]</note>
</pattern>

<pattern name="should_naming">
<description>BDD-style naming that reads like natural language</description>
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
# Use setup/teardown to reset state
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
# Good: Clear and descriptive
test_userRegistration_withExistingEmail_returnsError

# Bad: Unclear purpose

test_user_reg_1
</example>
</practice>

<practice priority="high">
<name>One assertion per concept</name>
<description>Each test should verify one logical concept</description>
<example>
# Good: Single concept
test_userCreation_setsDefaultRole
  user = create_user
  assert_equal "member", user.role
end

# Avoid: Multiple unrelated assertions

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
# Create reusable test data
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
# Good
VALID_USER_AGE = 25
MINIMUM_AGE = 18
test_userValidation_withValidAge_succeeds
  user = User.new(age: VALID_USER_AGE)
  assert user.valid?
end

# Bad

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
