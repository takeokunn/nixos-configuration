---
name: test-integrity
description: This skill should be used when a test suite is green but its value is in doubt — auditing whether a passing test proves anything, investigating a "false green", a suite that silently collected zero tests, a runner glob or manifest that dropped a test file, an assertion that no outcome could violate, a mock or seed that makes the code under test unnecessary, a guard or threshold with no test proving it can fire, a teardown failure masking a real one, a substitution that was installed but never consulted, a test running against an implementation loaded from outside the tree being changed, or before trusting a green run as evidence that a change is correct. Keywords — false green, vacuous test, tautological assertion, test not registered, zero tests collected, test discovery, dead guard, unreachable branch, mutation check, teardown masking, implementation provenance, test double, exit status not asserted.
version: 2.1.0
---

<purpose>
  Establish whether a passing test proves anything. This skill is about one question and its
  many disguises: the suite is green — is that evidence? It catalogues the mechanisms by which
  a test reports success while exercising nothing, asserting nothing, or measuring something
  other than the behavior it names, and gives the mechanical checks that distinguish a real
  green from a decorative one. The emphasis is on why each failure mode is invisible: every
  trap here produces the same output as a healthy suite, so none of them is caught by reading
  the result. They are caught only by deliberately asking what would have to break for this
  test to go red.
</purpose>

<scope>
  <focus>
    Test integrity: false greens, vacuity, tests that never ran, doubles and fixtures that
    remove the subject, guards nothing proves can fire, failures masked during teardown,
    substitutions the running code never consulted, and the audit procedures that prove
    non-vacuity.
  </focus>
  <defer_to skill="testing-patterns">
    Test design and strategy: the unit/integration/e2e split, arrange-act-assert and
    given-when-then structure, stub/mock/spy/fake selection, seam design — where the
    injection point goes, how a double is installed in it, and why a declared seam beats
    overwriting a global name — naming, fixtures and isolation, property-based and snapshot
    testing, polling and settlement barriers, and coverage as a metric. This skill assumes
    those tests are already written and asks only whether their green result is trustworthy;
    on seams it asks only whether the substitution was actually consulted.
  </defer_to>
  <defer_to skill="performance-benchmarking">
    Benchmark methodology, measurement noise, warm-up, and statistical comparison of timings.
    Where this skill discusses comparing an old and a new implementation, it is comparing
    observable behavior, not throughput.
  </defer_to>
  <defer_to skill="investigation-patterns">
    General evidence-gathering and root-cause methodology. Use it to trace why a test behaves
    as it does; use this skill to decide whether the test's verdict means anything.
  </defer_to>
  <unique_coverage>
    Zero-collection detection, assertion vacuity, subject-as-oracle, seed-the-subject,
    unsatisfied precondition gates, dead guards, impossible-branch deletion, teardown failure
    reporting, proving which implementation actually loaded, and the mutate-and-confirm-red
    audit.
  </unique_coverage>
</scope>

<concepts>
  <concept name="false_green">
    A test result that reports success without having established the claim its name makes.
    The defining property is indistinguishability: a false green and a true green produce
    byte-identical output, so no amount of reading the result detects one.
  </concept>
  <concept name="vacuity">
    A test is vacuous when no reachable behavior of the system could make it fail. Vacuity has
    degrees: an assertion satisfiable by every outcome is fully vacuous; a test whose
    precondition is never met is vacuous in practice while looking sound in source.
  </concept>
  <concept name="seam">
    The place where a test substitutes its own implementation for the real one — a function
    cell, an injected parameter, a module boundary, a configuration hook. A seam exists only
    if the production call path actually goes through it at the moment the test runs.
  </concept>
  <concept name="non_vacuity_proof">
    Positive evidence that a test can fail: a mutation of the implementation that turns it red,
    an asserted count of what actually executed, or an explicit assertion that the precondition
    the test depends on is in force before the act step.
  </concept>
  <concept name="collection_gap">
    The difference between the tests that exist in the tree and the tests a given command
    actually executes. Every discovery mechanism — a filesystem glob, a manifest file, a build
    component list, an export list, a shard configuration — can drop members silently, and a
    suite typically has more than one of them.
  </concept>
</concepts>

<never_ran>
  <description>
    The most complete false green is the test that did not run at all. It is worth treating as
    its own category because every mitigation is the same shape: stop accepting a zero exit
    code as the signal, and start asserting a count of what executed.
  </description>

  <principle name="zero_collected_is_not_zero_failures">
    <why>
      Every test runner accepts a selector — a directory, a glob, a tag, a name filter — and
      every selector can match nothing. A run over an empty set exits successfully and prints
      a summary that differs from a full run only in numbers nobody reads. A referenced
      directory that does not exist, a filter argument accepted at the command line but never
      forwarded to the runner, and a build target that was never added to the check graph all
      produce this outcome.
    </why>
    <implication>
      A verification gate must assert a non-zero collected-and-executed count, and must assert
      that every selected test's outcome is "passed" rather than merely "not failed" — some
      runners report expected-failure and skipped states in ways that satisfy a naive exit-code
      check. Designate exactly one canonical gate command; a narrower subset run must never be
      cited as having satisfied it.
    </implication>
  </principle>

  <principle name="reconcile_every_discovery_mechanism">
    <why>
      A suite usually has more than one registry: files on disk, a manifest listing them, a
      build-system component list, a package export list, a plan or shard configuration. A test
      can be present in some and absent from others. It then compiles, passes when invoked by
      hand, and is never scheduled by the suite. This is green-on-green — nothing fails,
      coverage just quietly shrinks — and it is among the most frequently repeated operational
      failures in practice.
    </why>
    <implication>
      Write a meta-test that reconciles the registries against the filesystem and against each
      other, and fails when any test source is not reachable from the canonical entry point.
      That test is the only thing standing between a suite and slow, invisible erosion; adding
      a file must fail loudly until every registry names it.
    </implication>
  </principle>

  <principle name="glob_versus_convention">
    <why>
      Two naming conventions commonly coexist in one repository (for example a suffix for unit
      specs and a different suffix for integration specs). A runner configured to match only
      one of them silently ignores every file using the other. The run is green over an empty
      or partial set, and the configuration looks reasonable in review because the pattern it
      contains is a real, valid pattern.
    </why>
    <implication>
      Compare the runner's reported test-file count against the number of test files on disk,
      per runner, whenever a naming convention is introduced or a runner is added. Better:
      enforce a single convention with a lint rule, so a mismatched glob cannot silently
      diverge from it.
    </implication>
  </principle>

  <principle name="assert_the_spawned_process_result">
    <why>
      An integration harness that launches a binary or forks a worker and then inspects side
      effects will report success when the process crashed before doing anything, provided the
      side effects it checks are absent-tolerant or pre-existing. Logging a panic without
      counting it has the same effect: incomplete work exits successfully.
    </why>
    <implication>
      Assert the exit status of every spawned process and the completion count of every worker
      pool, before asserting anything about side effects. Treat a non-zero status, a panic
      count above zero, or a completed count below the dispatched count as a test failure in
      its own right, not as context for a later assertion.
    </implication>
  </principle>

  <principle name="registration_collisions_remove_tests">
    <why>
      Frameworks with a global name-keyed registry replace an existing registration when the
      same name is registered again. This is deliberate — it prevents accumulation across
      reloads — but it means a duplicated name silently deletes a test rather than reporting a
      conflict. Two common sources: a shared helper file that is both loaded as a dependency
      and picked up as a standalone file by a recursive loader, and a migration to a
      table-driven generator where the legacy hand-written cases were left in place beside it.
    </why>
    <implication>
      Helper files must not define tests. After converting a cluster of cases to a generator,
      delete the cases it replaced in the same change. Derive generated names from a
      namespaced prefix so two generators cannot collide, and assert the post-registration
      count equals the expected case count.
    </implication>
  </principle>
</never_ran>

<vacuous_assertions>
  <description>
    The test ran, but no reachable outcome could have made it fail. These read as real tests in
    review and count as covered lines in a coverage report.
  </description>

  <principle name="name_the_input_that_would_fail">
    <why>
      An assertion whose accepted set covers every outcome the system can produce is a
      tautology. The most common form is disjunctive: asserting that a result is either the
      empty value or a well-formed object, when those two cases exhaust the return type. The
      test appears to cover a rejection path while proving only that the function returned.
      The unconditional true assertion is the degenerate case of the same shape, and it is
      often used as a placeholder to mark a requirement as "covered".
    </why>
    <implication>
      For every assertion, name a concrete input that would make it fail. If none exists, the
      assertion is vacuous. Replace a disjunction with the exact expected outcome — stub the
      collaborators so the branch is forced, then assert the single value that branch must
      produce.
    </implication>
  </principle>

  <principle name="never_call_the_subject_to_compute_the_expected">
    <why>
      Deriving the expected value by invoking the system under test makes the assertion true by
      construction when the subject is pure, and flaky when it is stateful: a second invocation
      of a stateful parser or builder can return a differently shaped result than the first, so
      the test fails for reasons that have nothing to do with the contract. The failure
      presents as a genuine behavior regression, which is why this costs an investigation
      rather than being spotted immediately.
    </why>
    <implication>
      Ground expected values in the specification, a fixture, or a previously captured
      artifact. Where the subject must be invoked to build the comparison, invoke it exactly
      once and assert against that single captured result.
    </implication>
  </principle>

  <principle name="the_stimulus_must_not_satisfy_the_assertion">
    <why>
      When the input channel is visible in the output channel, an assertion can be satisfied by
      the stimulus rather than by the system's response. A terminal that echoes typed input
      satisfies any marker that is a literal substring of the command before the command is
      even executed; a shell that traces commands, and a log-scraping test that greps for the
      line it just wrote, have the same shape. Everything the test claims to prove — that the
      command executed, that rendering occurred, that the stream advanced — is unproven.
    </why>
    <implication>
      Disable echo where possible; snapshot the output before the stimulus and search only the
      post-stimulus delta; emit markers from encoded or split input so the marker bytes never
      appear in the stimulus; and carry an output-only nonce that also encodes the exit status.
      Make the guardrail mechanical: a helper should reject an expected literal or pattern that
      matches the stimulus it is about to send.
    </implication>
  </principle>

  <principle name="substring_matching_across_numbered_siblings">
    <why>
      A substring assertion on rendered text containing sequence numbers or identifiers matches
      its own numbered siblings: a check for entry one is satisfied by entry ten and entry
      eleven, which share its prefix. The test passes while asserting nothing, and it only
      becomes visibly wrong once the sequence crosses a digit boundary — long after it was
      written and long after anyone remembers why.
    </why>
    <implication>
      Use boundary-aware assertions: parse the rendered output into blocks, assert the block
      count, and assert the first and last blocks exactly. Where a substring check is
      unavoidable, anchor it with a delimiter that cannot occur inside a sibling's identifier.
    </implication>
  </principle>

  <principle name="observation_must_not_move_the_metric">
    <why>
      Probing a cache, counter, or rate-limited component to inspect its contents can itself
      register as an access. A later assertion that the hit count is at least one is then
      satisfied by the probe rather than by the behavior under test. The test measures its own
      instrumentation.
    </why>
    <implication>
      Read counters through a side-effect-free accessor, or snapshot the counter immediately
      before the act step and assert on the delta. When no non-mutating accessor exists, that
      absence is a design defect in the subject, not something for the test to work around.
    </implication>
  </principle>
</vacuous_assertions>

<fake_pass>
  <description>
    The test ran and the assertion is meaningful, but the fixture, double, or seed removed the
    subject from the path. The assertion is checking work that the test setup performed.
  </description>

  <principle name="a_presence_probe_is_satisfied_by_its_own_double">
    <why>
      Any "is this dependency available?" check — a function-bound predicate, an attribute
      probe, a callable type check, an interface conformance test — is satisfied by the double
      installed to replace that dependency. The availability test becomes a tautology: it
      passes with the real component present and passes identically with it absent. Pair this
      with a broad exception swallow around the call boundary and signature drift becomes
      invisible too; a change that adds a leading argument to every function at that boundary
      can leave a large body of tests passing against doubles that still have the old arity.
    </why>
    <implication>
      Prove availability by observing behavior only the real component can produce, not by
      probing for a name. Never wrap the boundary in a catch-all that swallows arity or
      signature errors — those are defects, not expected conditions. Where a double stands in
      for a component with a known signature, assert the double's signature against the real
      one so drift fails at fixture-construction time.
    </implication>
  </principle>

  <principle name="a_partial_module_double_erases_value_exports">
    <why>
      Guidance on doubles concentrates almost entirely on function behavior, so a partial module
      replacement typically re-declares the functions and omits everything else. Non-function
      exports — enumerations, allow-lists, category sets, thresholds — then resolve to
      undefined. Code that validates against them does not throw; it classifies every input as
      invalid and returns an empty result. The symptom is a plausible empty collection rather
      than an error, which sends the investigation into the parser or the feature code instead
      of the fixture.
    </why>
    <implication>
      A partial module double must re-export every symbol any consumer reads, not only the
      helpers the test calls directly. Adopt the diagnostic heuristic: when a parsing or
      filtering test returns an unexpectedly empty result, inspect the module double before
      touching the subject.
    </implication>
  </principle>

  <principle name="seeds_establish_preconditions_never_the_subject">
    <why>
      A deterministic seed exists to reach a starting state cheaply. When it also creates the
      artifact the test claims to verify — seeding the produced item, activating the resulting
      view, placing the actor already inside the state the transition was supposed to reach —
      the test asserts a state the product may be incapable of reaching on its own. This is the
      most common way an end-to-end suite becomes decorative while its case list still reads
      like full coverage.
    </why>
    <implication>
      Seeds may establish world, actor, and inventory preconditions. The transition under test
      must be performed through the same interface a real user drives. State this as a boundary
      rule in the fixture layer so it survives the next person who finds the seed convenient.
    </implication>
  </principle>

  <principle name="a_fixture_that_misses_a_gate_exercises_nothing">
    <why>
      When the path under test opens with a guard clause, a fixture that fails to satisfy the
      guard turns the whole interaction into a no-op. The resulting failure is misleading in an
      expensive way: it accuses a downstream subsystem that is perfectly healthy, and in the
      worst case the rejection surfaces as a timeout whose message names neither the real cause
      nor the right subsystem.
    </why>
    <implication>
      Enumerate the guards on the path under test and build the fixture to satisfy each one
      explicitly. When an integration test fails, confirm the code path actually ran before
      investigating the subsystem the failure appears to implicate — an assertion that the
      entry point was reached is cheap and eliminates the entire class.
    </implication>
  </principle>

  <principle name="redirect_every_persistence_path_before_anything_can_register">
    <why>
      A harness that redirects only the obvious persistence path leaves the others pointing at
      real user state. The suite then reads and rewrites the developer's real data, which both
      damages it and contaminates the suite's own assertions with values the test never
      created. Ordering matters as much as coverage: enabling a mode can register an exit hook
      or load a file immediately, so a redirect applied after that point is too late to prevent
      either effect.
    </why>
    <implication>
      Bind every persistence path — primary stores, learning or statistics files, caches,
      history — to per-run temporary locations before any code path can load or register.
      Add a suite-level assertion that real user files are unchanged after the run; without it
      this class of defect is entirely invisible.
    </implication>
  </principle>
</fake_pass>

<dead_guards>
  <description>
    Code that cannot execute, and tests that document it without exercising it. Both directions
    matter: a guard that never fires needs a test proving it can, and a branch that genuinely
    cannot be reached should be deleted rather than tested.
  </description>

  <principle name="prove_the_guard_trips">
    <why>
      A safety control whose threshold nobody crosses is inert, and nothing reports it. Line and
      branch coverage are fully satisfied — the guard is evaluated on every call — while the
      interesting branch is unreachable in practice. This is the general shape of every silently
      dead safety control: rate limiters, circuit breakers, alert thresholds, kill switches,
      validation bounds. A related trap is a gated measurement: a counter placed inside a
      conditionally executed body measures the gate rather than the phenomenon, so a threshold
      expressed in units of that counter can never be reached.
    </why>
    <implication>
      For every guard, write a test that drives it to trip, not merely one that calls the
      enclosing function. Prefer thresholds expressed relative to a measured baseline over
      absolute constants, because an absolute constant silently dies when the underlying scale
      changes. Keep measurement outside any gate whose behavior it feeds.
    </implication>
  </principle>

  <principle name="delete_impossible_branches_rather_than_test_them">
    <why>
      Under a strict branch-coverage gate an unreachable defensive branch leaves three moves:
      fabricate a test that constructs an impossible input, suppress the branch with a coverage
      pragma, or remove it. The first is the worst — it encodes a state the system cannot
      produce, and future readers treat it as a real case. If a helper is only ever called
      under a structural invariant, the fallback is not defensive; it is noise the coverage
      tool is correctly refusing to ignore.
    </why>
    <implication>
      Apply the discriminator: is the fallback behavior user-visible, or is it unreachable
      because an upstream invariant guarantees the input? Test the first; delete the second and
      assert the invariant instead. For an exhaustive set of cases, prefer enumerating every
      case with an explicit terminal result over a catch-all arm, which manufactures a branch
      no input can take.
    </implication>
  </principle>
</dead_guards>

<masked_failures>
  <description>
    The test failed, and something ate the failure. Cleanup code is the usual culprit because it
    runs on both the success and failure paths and is written as if it cannot fail itself.
  </description>

  <principle name="cleanup_failures_are_first_class_results">
    <why>
      This is the same problem in every language that has a finally-style construct: a cleanup
      that raises either replaces the real failure or is swallowed by a handler written to keep
      the suite moving. Both directions lose information. Swallowing is worse than replacing,
      because a resource leak — an orphaned process, an unreleased lock, a temporary directory
      that outlives the run — then reports as a successful test, and the leak accumulates
      across the suite until something unrelated fails.
    </why>
    <implication>
      When both the body and the cleanup fail, keep the body's condition primary and attach the
      cleanup's condition to it as structured data under a distinct key. When only the cleanup
      fails, the test still fails. Never report a cleanup-only failure as a pass, and never
      discard the body's condition to surface the cleanup's.
    </implication>
  </principle>

  <principle name="isolate_bulk_teardown_per_item">
    <why>
      Teardown that releases many resources in one loop aborts at the first failure, leaving the
      remainder allocated. The single reported error then understates the leak by an unknown
      factor, and the next test starts in a state nobody described.
    </why>
    <implication>
      Wrap each release independently, collect every failure, and report them together after
      attempting all of them. Where the resource is a process tree or an external system,
      distinguish "the release call failed" from "the release call succeeded but the resource
      is still present" — only the second is a leak, and only an explicit post-condition check
      finds it.
    </implication>
  </principle>
</masked_failures>

<seams>
  <description>
    A test that replaces an implementation depends on the production call path actually routing
    through the point of replacement. Designing and installing that seam belongs to
    testing-patterns; the question here is the audit one — given a green result, can you show
    that the code which ran was the code the test meant to exercise?
  </description>

  <defer_to skill="testing-patterns">
    Where the seam goes and how the double is installed in it: declared injection points over
    global rebinding, keeping a helper resolvable through the indirection tests rely on rather
    than inlining it, and the process-wide damage a global redefinition does under a parallel
    runner. Those are design decisions made while the test is written. Apply the rule below
    afterwards, to establish that the substitution was consulted at all.
  </defer_to>

  <principle name="prove_which_implementation_loaded">
    <why>
      Extends the stale-compiled-artifact trap in testing-patterns to the multi-location case,
      which its mitigation does not reach. When the same component exists in more than one
      location — a working tree and an installed copy, a local package and a system-wide one —
      the loader picks one, and newest-wins preference settings arbitrate only within a single
      location, never between locations. Deleting stale artifacts therefore does not help: both
      candidates are legitimately current, and the wrong one is simply earlier in the search
      order. A test can run entirely against an implementation the change never touched,
      producing both false greens and false reds, and the existing mitigation would lead you to
      trust a run that is still wrong.
    </why>
    <implication>
      Before believing any result from a suite with this shape, query the runtime for the
      provenance of a symbol under test and confirm it resolves to the tree being changed.
      Compare timestamps of the built artifact against the loaded one when behavior contradicts
      the source. Make the harness put the working tree ahead of every installed location
      explicitly rather than relying on newest-wins. The same audit applies to any installed
      double: give the substitute an observable trace, or make it fail loudly when called with
      unexpected arguments, so a substitute that was never consulted cannot be mistaken for one
      that agreed.
    </implication>
  </principle>
</seams>

<non_vacuity_audit>
  <description>
    The procedure for converting "the suite is green" into evidence. Run it when a suite is
    inherited, when a green result is about to be cited as proof a change is correct, or when a
    defect escaped that the suite claimed to cover.
  </description>

  <step order="1" name="prove_it_can_fail">
    <action>
      Mutate the implementation the test names — invert a condition, return a constant, delete
      the effect — and confirm the test goes red. Restore the mutation immediately.
    </action>
    <why>
      This is the only check that subsumes every trap in this skill at once. A test that stays
      green under a mutation of its own subject was never testing that subject, whatever the
      reason.
    </why>
  </step>

  <step order="2" name="assert_what_executed">
    <action>
      Capture the runner's collected and executed counts for the canonical gate and assert them
      against the number of test definitions in the tree. Assert the exit status of every
      spawned process and the completion count of every worker.
    </action>
    <why>
      Distinguishes a green over the full set from a green over an empty or truncated set —
      indistinguishable by exit code alone.
    </why>
  </step>

  <step order="3" name="prove_the_precondition_and_forbid_the_fast_path">
    <action>
      Before the act step, assert that the state the test depends on actually holds — that the
      cache entry is warm, that the gate is satisfied, that the slow path is the one about to
      run. Then make the alternative path an error for the duration of the test, so taking it
      fails rather than passes.
    </action>
    <why>
      A test named for a warm path that silently exercises a cold one passes for the wrong
      reason forever. Asserting the precondition catches setup drift; forbidding the fast path
      catches the case where the subject stops using the path the test names. Deleting a cache
      entry tests a cold miss, not stale-entry rejection — the distinction only survives if the
      test states it.
    </why>
  </step>

  <step order="4" name="discover_the_subjects_rather_than_list_them">
    <action>
      For a cross-cutting invariant, enumerate the population from the code itself — an exported
      symbol scan, a reflective walk, a filesystem traversal — and assert the invariant over
      every member. Encode intentional exceptions as an explicit data set, each pinned by its
      own negative test asserting that this member does not satisfy the invariant, deliberately.
    </action>
    <why>
      Strictly stronger than a maintained allowlist: a new subject defaults to enforced rather
      than to unchecked, so the standard failure — "we added a handler and forgot to apply the
      rule" — becomes a red test rather than a silent gap. The negative test per exception
      makes each exception a reviewed act rather than an unexplained omission, and fails when an
      exception stops being necessary.
    </why>
  </step>

  <step order="5" name="differential_test_a_replacement">
    <action>
      When replacing an implementation, run the previous version and the new one in the same
      process against the same generated inputs, and compare the full result — including
      failure metadata: error class, position, line, column, expected value, context. Load the
      prior implementation directly from version control so both versions are live
      simultaneously.
    </action>
    <why>
      Comparing only success values leaves the failure contract unverified, and the failure
      contract is usually the part callers depend on most precisely. In practice a large
      success-only comparison can report a clean result while a smaller run that also compares
      error metadata finds mismatches in the low percent range — the ratio is the argument for
      comparing everything rather than for running more cases.
    </why>
  </step>

  <step order="6" name="audit_the_assertions">
    <action>
      For each assertion in the area under audit, name an input that would make it fail. Flag
      every disjunction that exhausts the return type, every unconditional truth, every
      substring check against numbered content, and every expected value computed by invoking
      the subject.
    </action>
    <why>
      Mutation testing catches most of these, but it is expensive to run broadly and cannot be
      applied to a suite that will not run at all. This pass is cheap, requires no
      infrastructure, and is where most vacuity is actually found.
    </why>
  </step>
</non_vacuity_audit>

<anti_patterns>
  <avoid name="green_exit_code_as_evidence">
    <description>Treating a zero exit status from a test command as proof that tests ran and passed.</description>
    <instead>Assert a non-zero executed count and a passed status for every selected test; name one canonical gate.</instead>
  </avoid>
  <avoid name="disjunctive_expectation">
    <description>Asserting a result is one of several values that together exhaust the reachable outcomes.</description>
    <instead>Force the branch with stubs and assert the single exact outcome that branch must produce.</instead>
  </avoid>
  <avoid name="subject_as_oracle">
    <description>Computing the expected value by calling the system under test.</description>
    <instead>Source expectations from the specification, a fixture, or one captured invocation.</instead>
  </avoid>
  <avoid name="placeholder_true_assertion">
    <description>An unconditional truth assertion standing in for a group of requirements, so the requirement list looks covered.</description>
    <instead>Leave the case failing or explicitly pending; a passing placeholder is worse than an absent test because it suppresses the gap.</instead>
  </avoid>
  <avoid name="presence_probe_as_availability_test">
    <description>Asserting a dependency is available by probing for a name that the installed double also provides.</description>
    <instead>Assert behavior only the real dependency can produce.</instead>
  </avoid>
  <avoid name="partial_module_double_missing_value_exports">
    <description>Replacing a module with only its functions, leaving enumerations and constants undefined.</description>
    <instead>Re-export every symbol any consumer of that module reads.</instead>
  </avoid>
  <avoid name="seed_the_thing_under_test">
    <description>A fixture that creates or activates the very artifact the test claims to verify.</description>
    <instead>Seed preconditions only; drive the transition through the real interface.</instead>
  </avoid>
  <avoid name="unsatisfied_precondition_gate">
    <description>A fixture that does not satisfy a guard clause on the path under test, so the interaction is a silent no-op.</description>
    <instead>Satisfy every guard explicitly and assert the entry point was reached.</instead>
  </avoid>
  <avoid name="swallowed_teardown_failure">
    <description>Catching cleanup errors so the suite keeps moving, converting resource leaks into passes.</description>
    <instead>Fail the test on a cleanup-only failure; attach the cleanup condition alongside a primary body failure.</instead>
  </avoid>
  <avoid name="impossible_branch_test">
    <description>Constructing an input the system cannot produce, purely to satisfy a branch-coverage gate.</description>
    <instead>Delete the unreachable branch and assert the upstream invariant that makes it unreachable.</instead>
  </avoid>
  <avoid name="substitute_assumed_consulted">
    <description>Treating a green result as proof that an installed double or replacement was the implementation the code actually ran.</description>
    <instead>Query the runtime for the provenance of the symbol under test, and give the substitute a trace or a loud failure so silence cannot be read as agreement.</instead>
  </avoid>
</anti_patterns>

<best_practices>
  <practice priority="critical">Before citing a green suite as evidence, mutate the implementation and confirm the relevant test goes red.</practice>
  <practice priority="critical">Assert a non-zero executed test count; never accept a zero exit code as proof that tests ran.</practice>
  <practice priority="critical">Maintain a meta-test reconciling every test-discovery registry against the filesystem and against each other.</practice>
  <practice priority="high">For every assertion, be able to name an input that would make it fail.</practice>
  <practice priority="high">Fixtures may establish preconditions; the transition under test is always driven through the real interface.</practice>
  <practice priority="high">Write a test that drives each guard to trip, and prefer thresholds relative to a measured baseline over absolute constants.</practice>
  <practice priority="high">Assert the exit status of every spawned process before asserting on its side effects.</practice>
  <practice priority="high">Redirect every persistence path before any code can load or register, and assert real user state is unchanged after the run.</practice>
  <practice priority="medium">Prove a substitution was consulted before crediting it — a double that is never called leaves the same trace as one that agreed.</practice>
  <practice priority="medium">Compare failure metadata, not only success values, when differential-testing a replacement.</practice>
  <practice priority="medium">Enumerate invariant subjects by discovery with an explicit, individually justified exception set.</practice>
  <practice priority="medium">Prove the provenance of the loaded implementation when the same component exists in more than one location.</practice>
</best_practices>

<related_skills>
  <skill name="testing-patterns">Test design, structure, doubles, seam design and injection, fixtures, settlement barriers, and coverage — the tests whose integrity this skill audits</skill>
  <skill name="performance-benchmarking">Benchmark methodology and measurement noise, including the case where an optimization changes observable behavior</skill>
  <skill name="investigation-patterns">Evidence-based root-cause methodology for tracing why a test behaves as it does</skill>
  <skill name="quality-tools">Review scoring and refactoring operations, including the shelf life of a stored verdict</skill>
  <skill name="execution-workflow">Where the canonical verification gate is run and what counts as having run it</skill>
</related_skills>

<related_agents>
  <agent name="verification">Adversarially attempts to break an implementation; the natural consumer of the non-vacuity audit</agent>
  <agent name="test">Designs the replacement coverage once a vacuous test has been identified</agent>
  <agent name="quality-assurance">Reviews assertion strength and flags vacuity during code review</agent>
  <agent name="explore">Locates test registries, runner configurations, and discovery globs across the repository</agent>
</related_agents>

<constraints>
  <must>Establish that a test can fail before treating its pass as evidence</must>
  <must>Assert what actually executed — test counts, process exit statuses, worker completions</must>
  <must>Report cleanup failures rather than allowing them to mask or be masked</must>
  <must>Keep the subject under test out of the fixture, the seed, and the expected value</must>
  <avoid>Assertions no reachable outcome could violate</avoid>
  <avoid>Tests constructed to reach branches the system cannot produce</avoid>
  <avoid>Crediting a substitution the running code was never shown to consult</avoid>
</constraints>
