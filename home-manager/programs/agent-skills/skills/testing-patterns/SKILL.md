---
name: testing-patterns
description: "Use when asked to write tests, define a test strategy, measure coverage, create unit tests, integration tests, or end-to-end tests. Provides testing methodology, patterns for test doubles, and strategies for maintainable test suites."
---

Testing patterns and strategies for comprehensive test coverage and maintainable test suites. The agent should follow the project's existing test patterns, test happy paths first, then edge cases and error cases.

## Critical Rules

- Follow the project's existing test patterns and conventions
- Run tests after creation to verify they pass
- Cover critical paths first before edge cases
- Each test should be independent — no shared state or execution order dependencies

## Workflow

1. **Analyze** — Understand the code under test, identify critical paths and edge cases
2. **Design** — Choose test type (unit/integration/e2e), select naming convention and test doubles
3. **Implement** — Write tests following arrange-act-assert, one assertion per concept
4. **Validate** — Run tests, check coverage, fix any flaky behavior

## Test Types

| Type | Scope | Speed | When to Use |
|------|-------|-------|-------------|
| Unit | Single function/class/module | Fast, milliseconds | Business logic, utility functions, transformations |
| Integration | Multiple components together | Slower, may use real deps | API endpoints, database operations, service interactions |
| E2E | Full application stack | Slowest | Critical user journeys, smoke tests |

## Test Structure Patterns

### Arrange-Act-Assert

The standard three-phase test structure for unit and integration tests:

```ruby
# Arrange: Set up test data and preconditions
user = User.new(name: "John")
cart = ShoppingCart.new(user)

# Act: Execute the code under test
total = cart.calculate_total

# Assert: Verify expected outcomes
assert_equal 0, total
```

### Given-When-Then (BDD)

The agent should use this for behavior-focused tests readable by non-technical stakeholders:

```ruby
# Given: Initial context (preconditions)
given_a_user_with_an_empty_cart

# When: Action or trigger
when_the_user_calculates_total

# Then: Expected outcome
then_the_total_should_be_zero
```

## Test Doubles

### When to Use Each Type

| Double | Purpose | Use When |
|--------|---------|----------|
| Stub | Provide canned responses | Need dependency responses, not interaction verification |
| Mock | Verify interactions occurred | Need to verify specific method calls and arguments |
| Spy | Record calls, use real implementation | Need real behavior plus interaction verification |
| Fake | Working simplified implementation | Need a lightweight but working implementation |

### Examples

```ruby
# Stub: canned responses
api_client = stub(fetch_user: { id: 1, name: "John" })

# Mock: verify interactions
email_service = mock()
email_service.expect(:send_email, args: ["user@example.com", "Welcome"])
user_service.register(email_service)
email_service.verify

# Spy: record calls with real behavior
logger = spy(Logger.new)
service.process(logger)
assert_called logger, :log, with: "Processing complete"

# Fake: simplified working implementation
class FakeDatabase
  def initialize; @data = {}; end
  def save(key, value); @data[key] = value; end
  def find(key); @data[key]; end
end
```

## Test Naming Conventions

### Descriptive (Technical)

Format: `test_[method]_[scenario]_[expected_result]`

```
test_calculateTotal_withEmptyCart_returnsZero
test_calculateTotal_withMultipleItems_returnsSumOfPrices
test_calculateTotal_withDiscount_appliesDiscountCorrectly
```

### Should (BDD-Style)

Format: `[method]_should_[expected_behavior]_when_[condition]`

```
calculateTotal_should_returnZero_when_cartIsEmpty
calculateTotal_should_applyDiscount_when_couponIsValid
calculateTotal_should_throwError_when_pricesAreNegative
```

## Best Practices

### Test Priority Order

1. **Happy path first** — Normal, expected flow
2. **Edge cases** — Empty inputs, maximum values, null values, zero, negatives
3. **Error cases** — Invalid inputs, network failures, permission errors, timeouts
4. **Corner cases** — Concurrent access, timezone edge cases, leap years, DST transitions

### Test Quality

- **Isolate tests** — Use setup/teardown to reset state; each test creates its own data
- **One assertion per concept** — Each test verifies one logical behavior
- **Make tests readable** — Tests serve as documentation; use clear descriptive names
- **Use fixtures and factories** — Extract common test data setup into reusable helpers

```ruby
def create_test_user(overrides = {})
  defaults = { name: "Test User", email: "test@example.com", role: "member" }
  User.new(defaults.merge(overrides))
end
```

### Avoid Magic Numbers

```ruby
VALID_USER_AGE = 25
MINIMUM_AGE = 18

def test_userValidation_withValidAge_succeeds
  user = User.new(age: VALID_USER_AGE)
  assert user.valid?
end
```

## Coverage Metrics

| Metric | Measures | Notes |
|--------|----------|-------|
| Line coverage | % of code lines executed | Basic measure |
| Branch coverage | % of code branches (if/else) taken | More thorough than line |
| Function coverage | % of functions/methods called | Identifies untested functions |

Target 80%+ coverage for critical code paths. Focus on testing behavior over achieving coverage numbers — 100% coverage does not guarantee bug-free code.

## Anti-Patterns to Avoid

- **Testing implementation details** — Focus on observable behavior and outcomes, not internal implementation
- **Excessive mocking** — Use real implementations where practical; over-mocking often indicates poor design
- **Flaky tests** — Control time, randomness, and async operations; use fixed timestamps and seeded random generators
- **Slow tests** — Use unit tests for fast feedback; reserve integration/e2e for critical paths
- **Test interdependence** — Each test should create its own data and be runnable in isolation

## Error Escalation

- **Low:** Minor coverage gap in non-critical path — note in report, proceed
- **Medium:** Test flakiness detected — document issue, investigate root cause
- **High:** Critical path lacks test coverage — stop, present options to user
- **Critical:** Tests reveal security vulnerability — block operation, require acknowledgment
