---
name: Testing Patterns
description: This skill should be used when the user asks to "write tests", "test strategy", "coverage", "unit test", "integration test", or needs testing guidance. Provides testing methodology and patterns.
version: 0.1.0
---

<purpose>
Provide testing patterns and strategies for comprehensive test coverage and maintainable test suites.
</purpose>

<test_types>
<type name="unit">
<description>Test individual functions/methods in isolation</description>
<scope>Single function, class, or module</scope>
<characteristics>Fast, isolated, deterministic</characteristics>
<when>Business logic, utility functions, transformations</when>
</type>

<type name="integration">
<description>Test interaction between components</description>
<scope>Multiple components working together</scope>
<characteristics>Slower, may use real dependencies</characteristics>
<when>API endpoints, database operations, service interactions</when>
</type>

<type name="e2e">
<description>Test complete user workflows</description>
<scope>Full application stack</scope>
<characteristics>Slowest, tests real user scenarios</characteristics>
<when>Critical user journeys, smoke tests</when>
</type>
</test_types>

<patterns>
<pattern name="arrange_act_assert">
<phase name="arrange">Set up test data and preconditions</phase>
<phase name="act">Execute the code under test</phase>
<phase name="assert">Verify expected outcomes</phase>
</pattern>

<pattern name="given_when_then">
<phase name="given">Initial context (preconditions)</phase>
<phase name="when">Action or trigger</phase>
<phase name="then">Expected outcome</phase>
</pattern>
</patterns>

<naming>
<convention name="descriptive">
<format>test_[method]_[scenario]_[expected_result]</format>
<example>test_calculateTotal_withEmptyCart_returnsZero</example>
</convention>

<convention name="should">
<format>[method]_should_[expected_behavior]_when_[condition]</format>
<example>calculateTotal_should_returnZero_when_cartIsEmpty</example>
</convention>
</naming>

<coverage>
<category name="happy_path" priority="high">
<description>Test the normal, expected flow</description>
</category>

<category name="edge_cases" priority="high">
<description>Test boundary conditions</description>
<examples>Empty inputs, maximum values, null values</examples>
</category>

<category name="error_cases" priority="high">
<description>Test error handling paths</description>
<examples>Invalid inputs, network failures, permission errors</examples>
</category>

<category name="corner_cases" priority="medium">
<description>Test unusual combinations</description>
<examples>Concurrent access, timezone edge cases</examples>
</category>
</coverage>

<mocking>
<pattern name="stub">
<description>Provide canned responses</description>
<use_case>Replace slow/unreliable dependencies</use_case>
</pattern>

<pattern name="mock">
<description>Verify interactions occurred</description>
<use_case>Ensure methods called with correct arguments</use_case>
</pattern>

<pattern name="spy">
<description>Record calls while using real implementation</description>
<use_case>Verify side effects without changing behavior</use_case>
</pattern>

<pattern name="fake">
<description>Working implementation for testing</description>
<use_case>In-memory database, fake file system</use_case>
</pattern>
</mocking>

<organization>
<principle name="isolation">
<description>Each test should be independent</description>
<action>Reset state between tests</action>
<action>Avoid test order dependencies</action>
</principle>

<principle name="readability">
<description>Tests serve as documentation</description>
<action>Clear naming</action>
<action>Minimal setup per test</action>
<action>One assertion per concept</action>
</principle>

<principle name="maintainability">
<description>Tests should be easy to update</description>
<action>Use test fixtures and factories</action>
<action>Avoid magic numbers</action>
<action>Extract common setup</action>
</principle>
</organization>

<metrics>
<metric name="line_coverage">Percentage of lines executed</metric>
<metric name="branch_coverage">Percentage of branches taken</metric>
<metric name="function_coverage">Percentage of functions called</metric>
<guidance>Aim for high coverage but prioritize meaningful tests over coverage numbers. 80%+ coverage is a good target for critical code.</guidance>
</metrics>

<anti_patterns>
<avoid name="testing_implementation">Test behavior, not implementation details</avoid>
<avoid name="excessive_mocking">Too many mocks indicate poor design</avoid>
<avoid name="flaky_tests">Tests that sometimes pass/fail erode trust</avoid>
<avoid name="slow_tests">Slow tests discourage running them</avoid>
<avoid name="test_interdependence">Tests that depend on other tests' state</avoid>
</anti_patterns>

