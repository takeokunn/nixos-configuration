---
name: performance-benchmarking
description: Use when producing, gating, or reviewing performance numbers - before/after benchmarks, speedup/regression claims, CI benchmark gates, noise floor and confidence interval, wall-clock vs allocation-count metrics, and wording a performance claim.
version: 2.1.0
---

<purpose>
  Provide the methodology for producing performance numbers that mean something: how to
  measure so that a difference is attributable to the change rather than to the machine,
  how to decide whether an observed difference is large enough to claim, how to gate on it
  in CI without blocking correctness work, and how to word the resulting claim so it does
  not overreach. The organizing fact is that a benchmark comparing identical code against
  itself can report a confident-looking improvement; every rule here follows from taking
  that seriously.
</purpose>

<scope>
  <focus>Benchmark methodology and measurement protocol: noise-floor estimation, paired A/B design, metric selection, statistical gating, claim scoping, negative-result recording, and the evidence a throughput claim needs — including the arm where the change's premise does not hold. Strategies that a measurement evaluates are owned by the skill that specifies them.</focus>
  <defer_to skill="sbcl-usage">
    Profiler and timing tool invocation for that runtime (deterministic profiler, statistical profiler, timing macros) and its coverage-instrumentation caveats. This skill owns the protocol around those tools, not their syntax.
  </defer_to>
  <defer_to skill="test-integrity">
    Correctness tests that pass without proving anything — unconditional assertions, unregistered test files, ignored selector flags. This skill covers only the benchmark-specific analogue: a measurement that runs but measures the wrong thing.
  </defer_to>
  <defer_to skill="parallelization-patterns">
    The prescriptive work-scheduling strategy for data-parallel workloads — claiming units size-descending through a shared atomic cursor, and re-establishing deterministic output order through pre-claimed index slots — along with agent-level orchestration and concurrency policy. That skill owns the strategy and its implementation steps. This skill owns only the measurement evidence for it: how the speedup pair was obtained, and why the non-skewed arm must be measured before the change can be called a strict improvement.
  </defer_to>
  <unique_coverage>
    Measuring the noise floor by benchmarking identical code against itself; gating on a confidence interval's lower bound with graceful degradation when no interval is available; benchmarks as non-blocking information rather than a merge gate; choosing a deterministic metric (allocations, operation counts, syscalls) over wall-clock time; decrementing budgets instead of absolute deadlines; paired-seed comparison of stochastic arms; bounding a cross-implementation claim; recording rejected experiments with a retry precondition.
  </unique_coverage>
</scope>

<concepts>
  <concept name="noise_floor">The magnitude of difference a benchmark reports when nothing has changed. It is a property of the harness, machine, and workload — not of the code — and it is the threshold below which any measured effect is unmeasured rather than absent.</concept>
  <concept name="point_estimate_vs_interval">The point estimate (a mean or median difference) is the headline number; the confidence interval is the part that says whether the headline means anything. A point estimate read without its interval discards exactly the information that distinguishes a result from noise.</concept>
  <concept name="paired_measurement">Measuring both arms under the same conditions — same process and alternating order, or identical random seeds across arms — so that shared variance (machine load, cache state, scheduler behavior) cancels in the difference instead of accumulating in it.</concept>
  <concept name="gate_vs_signal">A gate blocks a merge and must therefore be near-zero false positive. A signal informs a reviewer and may be noisy. Benchmarks are signals; treating one as a gate makes the noise floor a correctness obstacle for unrelated work.</concept>
  <concept name="deterministic_contract">A property of an execution that does not vary with machine speed — number of allocations, number of commits to a data structure, number of subprocesses spawned, number of iterations. Asserting one turns a flaky timing test into a deterministic one.</concept>
</concepts>

<noise_floor>
  <description>
    Establish what your harness reports when the answer is known to be zero, before you
    believe anything it reports when the answer is unknown.
  </description>

  <principle name="measure_the_floor_by_benchmarking_identity">
    <why>
      A benchmark harness does not report "no difference" when there is no difference. In a
      measured example, benchmarking identical code against itself — same commit, no change
      at all — produced a +5.0% point estimate with a 95% interval of +0.5%..+10.3%. Both
      the headline number and the interval's lower bound cleared zero. Any workflow that
      would have accepted "+5% improvement, interval excludes zero" as evidence would have
      accepted a change that did not exist.
    </why>
    <implication>
      Run the benchmark twice on the same commit and record the spread. That spread is the
      floor for that harness on that machine for that workload. Every subsequent claim is
      measured against it, and the floor is re-established when the machine, the runner
      class, or the workload changes.
    </implication>
  </principle>

  <principle name="below_the_floor_is_unmeasured_not_absent">
    <why>
      "We measured no regression" and "the regression, if any, is smaller than our noise
      floor" are different statements, and only the second is supportable. Reporting the
      first converts a limitation of the instrument into a property of the code.
    </why>
    <implication>
      When an effect is smaller than the floor, say it is below the resolution of the
      measurement. If it matters, either raise the resolution (more samples, quieter
      machine, larger workload) or switch to a deterministic metric that has no floor.
    </implication>
  </principle>

  <principle name="an_unqualified_percentage_is_not_a_result">
    <why>
      A single number carries no information about how it was obtained, so it cannot be
      reproduced, contested, or compared against a later run. Two runs of the same harness
      under different sample counts are not comparable results even though both are
      percentages.
    </why>
    <implication>
      Report the protocol alongside the number: sample count, warmup, process topology,
      ordering scheme, and the interval. A number without its protocol should be treated as
      an anecdote by both the author and the reviewer.
    </implication>
  </principle>
</noise_floor>

<measurement_protocol>
  <description>
    The recurring elements of protocols that produced defensible numbers. They exist to
    make shared variance cancel and to keep the first samples — which measure the runtime
    warming up, not the code — out of the result.
  </description>

  <element name="process_topology">
    <options>
      <option name="independent_processes">Run each arm in its own fresh process, repeated N times, and compare across processes. Removes in-process state carryover (caches, heap shape, JIT/compilation state) at the cost of paying startup per sample. A concrete instance: five independent processes per arm, each with 200 warmup and 2,000 measured operations.</option>
      <option name="alternating_pairs_in_one_process">Run both arms in one process in alternating A/B, B/A order, and compare within each pair. Removes machine-level drift because the two halves of a pair are adjacent in time. A concrete instance: 6 warmup pairs then 21 alternating measured pairs, with a full collection before each sample.</option>
    </options>
    <note>Do not run all of A and then all of B in one process. Any drift over the session — thermal, heap growth, background load — is then indistinguishable from the effect.</note>
  </element>

  <element name="declared_warmup">
    <rule>State the warmup count explicitly and exclude it. Warmup is not a courtesy; the first samples measure compilation, page faults, and cold caches. An undeclared warmup makes two runs incomparable.</rule>
  </element>

  <element name="collector_state">
    <rule>Force a full garbage collection before each measured sample. Otherwise a collection triggered by arm A's allocation pattern lands inside arm B's sample and is attributed to B.</rule>
  </element>

  <element name="paired_statistic">
    <rule>Report the median of the paired differences, plus the win count out of N ("B was faster in 13 of 15 pairs"). The win count is robust to a single outlier pair in a way that a mean of ratios is not, and it is immediately interpretable.</rule>
  </element>

  <element name="protocol_disclosure">
    <rule>Publish the protocol with the number. A worked example of a complete disclosure: 15 paired samples, alternating order, warmup 2, full collection before each sample, with the output signature verified identical in all 120 samples.</rule>
  </element>

  <principle name="instrumented_runs_are_single_worker">
    <why>
      Coverage and allocation instrumentation typically maintains process-global mutable
      counters. Running instrumented work across concurrent workers therefore produces
      nondeterministic per-file undercounts even though every assertion still passes — the
      numbers are wrong in a way nothing in the run reports. A related trap: instrumentation
      binds counters to a source identity established at compile time, so manually compiling
      and loading copied sources can detach the counters from the identity the report keys
      on, yielding files that appear at exactly zero.
    </why>
    <implication>
      Run any instrumented measurement single-worker, and load the instrumented sources
      through the build system after resetting the instrumentation rather than compiling
      copies by hand. Parallelism is a property of the fast unmeasured run, not of the
      measured one.
    </implication>
  </principle>

  <principle name="bound_the_run_with_a_kill_grace">
    <why>
      A benchmark or measurement run that hangs will consume the entire job budget, and a
      plain timeout is not sufficient to prevent that: a runtime that installs a termination
      handler or blocks signals can remain alive after its first signal, so a nominal
      limit leaks past the budget it was supposed to enforce.
    </why>
    <implication>
      Bound the run with a kill grace — with coreutils, `timeout --foreground -k 10s
      &lt;limit&gt;s` rather than plain `timeout --foreground &lt;limit&gt;s` — so an escaped
      child is forcibly reaped. Apply the same reasoning to any in-harness timeout: the
      escalation from a polite signal to an unconditional kill must be part of the design,
      not an assumption about the child's cooperation.
    </implication>
  </principle>
</measurement_protocol>

<measuring_the_right_thing>
  <description>
    Three ways a benchmark runs cleanly, reports a plausible number, and measures something
    other than the change under test.
  </description>

  <principle name="prove_you_are_measuring_your_working_tree">
    <why>
      A development environment or dependency cache can pre-register a published build of
      the very package you are benchmarking. The harness then compiles and measures that
      build while your edits sit unread on disk. Benchmark metadata such as a reported
      source root does not prove which artifact the compiler actually consumed.
    </why>
    <implication>
      Before trusting an A/B result, force resolution to the working tree and verify it:
      reinitialize the source/module registry against the workspace, clear the registered
      system, load its definition by absolute workspace path, force recompilation, then
      assert that the resolved source file and the compiler's output paths point into the
      workspace rather than into a package store. This generalizes to any package manager
      that can silently substitute a published build for a local checkout.
    </implication>
  </principle>

  <principle name="the_fixture_must_pass_a_correctness_guard_first">
    <why>
      A malformed fixture frequently normalizes to the empty or trivial case rather than
      failing. In one instance a benchmark fixture intended to exercise a styled-output path
      was not interpreted as a style specification at all; after normalization it produced no
      style, the computed diff length was zero, and the benchmark measured the unchanged
      case while reporting fine numbers.
    </why>
    <implication>
      Assert the property the fixture is supposed to have before measuring it — non-zero
      diff length, non-empty result set, the expected branch actually taken. A benchmark
      without a fixture guard can silently become a measurement of the fast path you were
      trying to avoid.
    </implication>
  </principle>

  <principle name="assert_output_parity_on_every_sample">
    <why>
      A fast wrong answer is the default failure mode of an optimization. Checking the output
      once, before or after the timed run, does not cover the case where the optimized arm is
      correct on the first input and wrong on a later one, or where it is nondeterministic.
    </why>
    <implication>
      Compare a cheap signature of the output on every sample of every arm — a result hash,
      a record count, a byte-identical report — and report that the signature was constant
      across all samples as part of the result. A protocol that says "identical result hash
      in all 120 samples" has proved something a single spot-check has not.
    </implication>
  </principle>
</measuring_the_right_thing>

<choosing_a_metric>
  <description>
    Wall-clock time is the metric people want and the worst-behaved one available. When it
    is too noisy to support a claim, the answer is usually to claim a different metric
    rather than to keep collecting samples.
  </description>

  <principle name="claim_the_deterministic_metric_refuse_the_noisy_one">
    <why>
      Allocation counts, operation counts, and syscall counts are exact: two runs of the same
      code produce the same number. Wall-clock time is a distribution whose spread is set by
      the machine. When both are collected and only one is stable, the stable one is the
      result and the other is an indicator.
    </why>
    <implication>
      Publish the deterministic effect as the confirmed result and demote the timing to an
      observation. Worked examples: a roughly 5.1% wall-clock improvement recorded as
      "observation including noise" while the confirmed effect was stated as a bounded
      reduction of at most eight cons cells per operation; and a comparison where the timing
      was a single-sample indicator while the removed allocation slope was reported as the
      robust result. A wall-clock comparison was rejected outright in another case because
      all arms showed correlated load spikes and per-trial spreads of 3.6x to 7.1x — that
      spread is a noise floor larger than any plausible effect.
    </implication>
  </principle>

  <principle name="a_timing_assertion_measures_the_machine_not_the_change">
    <why>
      The same reasoning applies inside a test suite, where it is the measurement rationale
      that carries over: a timing assertion is a machine-speed assertion, so it fails on a
      loaded runner and passes on a fast laptop independently of whether the regression it was
      written to catch has occurred. Its threshold is therefore a noise floor with a pass/fail
      attached. The regression worth catching is usually structural — a per-item operation
      that should have been per-batch — and structure is exactly what a deterministic count
      measures and a duration does not.
    </why>
    <implication>
      When a benchmark's finding needs to be locked in by a test, hand it over as a
      deterministic contract rather than a threshold. testing-patterns owns how to write that
      assertion and carries the worked examples; this skill's contribution is only the
      argument for which metric to hand over.
    </implication>
  </principle>

  <principle name="decrementing_budgets_not_absolute_deadlines">
    <why>
      A loop that terminates by comparing the current clock against a stored deadline is
      exposed to system clock jumps, which can lengthen, shorten, or entirely prevent the
      intended timeout. A budget that is decremented by each slice depends only on elapsed
      slices, so a slow machine degrades into fewer iterations rather than into a
      hard failure or a hang.
    </why>
    <implication>
      Carry a remaining-budget value and subtract each wait from it. Validate the budget
      input, because a decrementing budget is finite only if its input is finite: positive
      infinity is a concrete nontermination counterexample, since infinity minus a slice is
      still infinity. Cover the full input set — NaN, both infinities, zero, negative, and
      non-numeric — when the budget is caller-supplied.
    </implication>
  </principle>
</choosing_a_metric>

<stochastic_comparison>
  <description>
    Comparing two arms that are each randomized — Monte Carlo simulations, randomized
    algorithms, load generators, sampling-based evaluations — needs the same discipline as
    a timing benchmark, plus the seed.
  </description>

  <principle name="pair_the_seeds">
    <why>
      Running arm A on one set of random draws and arm B on another means the comparison
      carries the variance of both draw sets. Using the identical seeded draws for both arms
      makes the comparison paired, so per-draw variance cancels in the difference and the
      remaining spread is attributable to the arms.
    </why>
    <implication>
      Use common random numbers: generate the scenario set once from a fixed seed and run
      every arm against that same set. Report the paired difference, not the two means.
    </implication>
  </principle>

  <principle name="size_the_effect_against_its_own_noise">
    <why>
      A bare gap between two means is uninterpretable without knowing how much the means
      themselves move between runs. When the standard error of the difference is of the same
      order as the gap, the gap is not evidence.
    </why>
    <implication>
      Report the difference relative to its standard error, not as a raw gap. If the standard
      error swamps the mean, the correct conclusion is "not significant at this sample
      count", not "A is slightly better".
    </implication>
  </principle>

  <principle name="re_run_at_higher_n_before_concluding">
    <why>
      Small samples under-represent rare, high-magnitude outcomes, so the arm that avoids
      them by luck looks better. In a measured case, a 75-sample comparison produced a
      confident winner; the same comparison at 300 paired samples showed the difference was
      not significant and that both arms moved in the same direction, because the small run
      had under-sampled the heavy tail. Acting on the 75-sample result would have been
      fitting the noise.
    </why>
    <implication>
      Treat a small-sample winner as a hypothesis, not a result. Re-run at several times the
      sample count before routing a decision through it, and record the sample count in the
      claim so a later reader can judge it.
    </implication>
  </principle>
</stochastic_comparison>

<adoption_criterion>
  <description>
    Decide what would make you adopt the change before you see the numbers, then honor it
    when the numbers arrive.
  </description>

  <principle name="declare_the_criterion_first">
    <why>
      A criterion chosen after the measurement is chosen to fit the measurement. Declaring
      thresholds in advance is what makes a rejection possible at all — without one, every
      result is an improvement in some metric.
    </why>
    <implication>
      Write the criterion as explicit thresholds on named metrics plus a no-regression
      clause, for example: primary metric improves by at least 2%, allocation improves by at
      least 3%, and neither metric regresses. Then apply it mechanically.
    </implication>
  </principle>

  <principle name="honor_the_criterion_when_it_rejects">
    <why>
      The value of a pre-declared criterion is realized only in the cases where it says no.
      A candidate that improves one metric while regressing the metric the criterion
      prioritizes is a rejection, however attractive the improved number looks.
    </why>
    <implication>
      Two worked rejections: a candidate that cut allocation by 30.6% but cut throughput by
      34.8% was rejected because the workload was throughput-focused; and a candidate whose
      median throughput moved -1.00% with allocation -0.76% was rejected on the throughput
      regression despite the allocation win. Record the rejection with its numbers — that
      record is what stops the candidate being re-proposed.
    </implication>
  </principle>
</adoption_criterion>

<gating>
  <description>
    How to turn a benchmark into an automated check without turning the noise floor into a
    build failure.
  </description>

  <principle name="gate_on_the_interval_lower_bound">
    <why>
      A gate that reads only the point estimate discards the part of the measurement that
      says whether the number means anything, and therefore fires at exactly the rate of the
      noise floor. The identity measurement above — +5.0% point estimate, interval reaching
      down to +0.5% — is what such a gate would have flagged as a regression on a
      no-op commit.
    </why>
    <implication>
      Fail only when the interval's lower bound also clears the threshold. That is the
      condition under which the harness is asserting the effect is at least that large,
      rather than merely that its best guess is.
    </implication>
  </principle>

  <principle name="print_noisy_instead_of_failing">
    <why>
      The interesting middle case — point estimate over the threshold, interval reaching back
      below it — is neither a clean pass nor defensible evidence. Suppressing it loses a real
      signal; failing on it makes the gate unusable.
    </why>
    <implication>
      Emit a distinct "noisy" status: report the number, name it as inconclusive, and do not
      fail. A reviewer can then decide whether to re-run at higher sample count or ignore it.
    </implication>
  </principle>

  <principle name="degrade_to_old_behavior_when_the_interval_is_missing">
    <why>
      An older or differently-configured harness may emit only a point estimate. A gate
      written to require an interval will then find no interval, treat every result as
      unverifiable, and silently pass everything — a check that has become a no-op while
      still reporting green.
    </why>
    <implication>
      Fall back to the point estimate when no interval is present, so the gate degrades to
      its previous, weaker behavior rather than to vacuous success. Whenever a check depends
      on an optional field, decide explicitly what its absence means; the default of
      "absent means pass" is almost never the intended one.
    </implication>
  </principle>

  <principle name="an_aggregate_percentage_needs_a_denominator_manifest">
    <why>
      A gate that accepts an aggregate percentage from a report is trusting the report's own
      choice of denominator. A file that was dropped from the report entirely — never
      instrumented, never loaded, excluded by a path pattern — does not lower the aggregate;
      it vanishes from it. So a report can show a perfect total precisely because the
      interesting components are missing from it.
    </why>
    <implication>
      Compare the report's normalized row identifiers against a declared manifest of the
      components that must appear, and reject the run when a manifest entry has no row, when
      a row is malformed, or when a row's total is zero — before evaluating the aggregate at
      all. The same shape applies to any gate reading a summary statistic: verify the set of
      things summarized, not just the summary.
    </implication>
  </principle>
</gating>

<benchmarks_inform_they_do_not_block>
  <description>
    The role a benchmark job should play in a review workflow, and why that role is
    deliberately weaker than a test suite's.
  </description>

  <principle name="a_performance_regression_is_information_not_a_veto">
    <why>
      A correctness fix that costs performance is still a correctness fix. Blocking it on a
      benchmark inverts the priority, and because benchmarks have a noise floor, it also
      blocks changes that cost nothing at all. The failure mode is predictable: the gate is
      routinely overridden, and once overriding is routine the signal is worthless.
    </why>
    <implication>
      Configure the benchmark job as non-blocking by design — continue-on-error, or an
      advisory status — and say in the configuration that this is intentional so nobody
      "fixes" it later. The output is addressed to a reviewer, not to the merge button.
    </implication>
  </principle>

  <principle name="red_means_diagnose_never_stop">
    <why>
      Because the job cannot block, a red result carries no procedural consequence, which
      makes it tempting to ignore. That is the opposite of the intent: the number is the only
      reason the job exists.
    </why>
    <implication>
      Treat a red benchmark as an obligation to investigate and to state a conclusion —
      real regression, noise above the floor, or workload change — but never as a reason to
      halt the change under review.
    </implication>
  </principle>
</benchmarks_inform_they_do_not_block>

<bounding_claims>
  <description>
    Wording a result so that it survives contact with a reader who has a different machine,
    a different workload, or a competing implementation.
  </description>

  <principle name="cross_implementation_results_are_not_rankings">
    <why>
      Implementations that appear comparable often differ in what they return. A parser that
      produces vectors and hash tables is not doing the same work as one that produces lists
      and association lists, so a raw timing comparison between them measures the
      representation choice as much as the implementation quality.
    </why>
    <implication>
      Bound every cross-implementation claim to the benchmarked implementations, their
      versions and environment, the payloads used, and the canonical output representation
      compared. Do not state an unqualified universal superiority claim; it is unsupportable
      by any benchmark you can actually run.
    </implication>
  </principle>

  <principle name="enumerate_your_own_caveats">
    <why>
      The caveats are known to the author at measurement time and to nobody else afterwards.
      Omitting them does not make the result stronger, it makes it unfalsifiable and
      therefore easy to dismiss when someone fails to reproduce it.
    </why>
    <implication>
      List what the measurement does not cover, in the same place as the number. A worked
      example enumerates: tool versions not pinned, trials not perfectly order-balanced
      across arms, each trial a fresh process but excluding startup and load time, and a
      single synthetic workload — concluding explicitly that this baseline cannot by itself
      substantiate a general claim.
    </implication>
  </principle>
</bounding_claims>

<negative_results>
  <description>
    An optimization that was tried and did not work is durable knowledge, and it is the
    knowledge most reliably lost between sessions.
  </description>

  <principle name="record_the_measurement_and_the_retry_precondition">
    <why>
      A reverted experiment leaves no trace in the source tree, so the same idea is near
      certain to be re-proposed and re-implemented later. A bare "we tried that" is not
      enough to stop it — without the measurement and the reason, re-trying looks
      reasonable.
    </why>
    <implication>
      Record three things: what was changed, what was measured, and the explicit precondition
      for retrying. Worked examples of the precondition form: "do not retry these no-op
      checks on this path without call-site frequency evidence and a profiler result"; "do
      not retry loop-wide scratch binding without first explaining the compiler and
      allocation interaction and proving improvement in the same isolated benchmark"; and,
      for a change measured at 0.006% difference, "do not re-attempt without new evidence"
      together with the mechanism that explained the null result — a large growable buffer
      grows by remapping pages rather than by copying into a second resident allocation.
    </implication>
  </principle>

  <principle name="a_null_result_needs_a_mechanism">
    <why>
      "It made no difference" is a measurement; "it made no difference because the cost is
      not where we assumed" is an explanation that also invalidates the next three variants
      of the same idea. Only the second prevents repeat work.
    </why>
    <implication>
      Pair every null or negative result with the mechanism that explains it, or state that
      the mechanism is unknown — which is itself a useful flag that the area is not
      understood well enough to optimize.
    </implication>
  </principle>
</negative_results>

<scheduling_and_throughput>
  <description>
    Measurement evidence for two throughput changes. The scheduling strategy itself is
    specified in parallelization-patterns (work_scheduling_on_skewed_inputs); what follows is
    only what was measured, and what a measurement has to cover before the claim stands.
  </description>

  <principle name="measure_the_arm_where_the_premise_does_not_hold">
    <why>
      A scheduling change motivated by skew will look good on the skewed workload that
      motivated it — that arm proves the mechanism works, not that the change is free. Whether
      it is a strict improvement or a tradeoff is decided entirely by the workload where its
      premise does not hold, and that is the arm people skip because it is expected to be
      boring.
    </why>
    <implication>
      Measure both. Replacing static contiguous chunking with size-descending claiming through
      a shared atomic cursor produced a 4.80x median speedup on a size-skewed workload of 240
      items, and 1.004x on an evenly-sized workload of the same size. The second number is
      what licenses the word "strict": it says the change costs nothing when its premise does
      not hold. Because a scheduling change also perturbs output order, pair the timing with a
      byte-identical output comparison against the pre-change implementation on both
      workloads — the ordering discipline that comparison is checking belongs to
      parallelization-patterns.
    </implication>
  </principle>

  <principle name="throttle_expensive_probes_independently_of_cheap_polling">
    <why>
      A polling loop usually has one interval, but not all of its probes cost the same. A
      file read at 50 ms is fine; a probe that spawns a child process at 50 ms is a process
      storm that dominates the cost of the thing being monitored.
    </why>
    <implication>
      Give each probe its own rate limit rather than sharing the loop interval, and where a
      grace period applies, do not begin the expensive probe until the grace period has
      elapsed. A measured instance reduced a ten-second wait from roughly 195 subprocess
      spawns to at most 40 by starting the expensive probe after a 250 ms grace period and
      running it at most once per 250 ms, while leaving the cheap poll at 50 ms.
    </implication>
  </principle>
</scheduling_and_throughput>

<anti_patterns>
  <avoid name="believing_a_first_run_difference">
    <description>Running the benchmark once before and once after, and reporting the difference.</description>
    <instead>Establish the noise floor on identical code first, then use a paired protocol with declared warmup and report the interval alongside the point estimate.</instead>
  </avoid>
  <avoid name="gating_on_the_point_estimate">
    <description>A CI check that reads the mean point estimate and fails when it crosses a threshold.</description>
    <instead>Fail only when the interval's lower bound clears the threshold; print a non-failing "noisy" status for the in-between case.</instead>
  </avoid>
  <avoid name="absent_field_means_pass">
    <description>A gate that requires an interval and silently passes every result when the harness does not emit one.</description>
    <instead>Fall back to the point estimate so the check degrades to weaker behavior rather than to a no-op.</instead>
  </avoid>
  <avoid name="blocking_merges_on_benchmarks">
    <description>A required benchmark job that prevents a correctness fix from merging.</description>
    <instead>Run it non-blocking by design and treat the result as reviewer information.</instead>
  </avoid>
  <avoid name="wall_clock_assertions_in_tests">
    <description>Handing a benchmark finding to the test suite as a duration threshold, which asserts machine speed rather than the change.</description>
    <instead>Hand over a deterministic metric instead; see testing-patterns for how to write the assertion.</instead>
  </avoid>
  <avoid name="absolute_deadline_loops">
    <description>Terminating a wait loop by comparing the current clock against a stored end time.</description>
    <instead>Decrement a validated remaining budget by each slice, rejecting non-finite inputs.</instead>
  </avoid>
  <avoid name="unpaired_stochastic_arms">
    <description>Running two randomized arms on independently drawn scenarios and comparing their means.</description>
    <instead>Use identical seeded draws for every arm and report the paired difference against its standard error.</instead>
  </avoid>
  <avoid name="small_sample_winner_as_decision_input">
    <description>Routing a design decision through the arm that won a short bake-off.</description>
    <instead>Re-run at several times the sample count; treat the short run as a hypothesis.</instead>
  </avoid>
  <avoid name="unqualified_superiority_claim">
    <description>Stating that an implementation is the fastest, without scoping to versions, payloads, environment, and output representation.</description>
    <instead>Bound the claim to what was benchmarked and enumerate the caveats in the same place as the number.</instead>
  </avoid>
  <avoid name="silent_revert_of_a_failed_experiment">
    <description>Reverting an optimization that did not pay off and leaving no record.</description>
    <instead>Record the change, the measurement, the mechanism, and an explicit precondition for retrying.</instead>
  </avoid>
  <avoid name="parallel_instrumented_run">
    <description>Running coverage- or allocation-instrumented work across concurrent workers because the uninstrumented suite runs that way.</description>
    <instead>Run instrumented measurements single-worker, loading instrumented sources through the build system after resetting the instrumentation.</instead>
  </avoid>
  <avoid name="aggregate_percentage_without_a_manifest">
    <description>Accepting a headline percentage from a report without checking which components appear in it.</description>
    <instead>Verify every declared component has a non-zero, well-formed row before evaluating the aggregate.</instead>
  </avoid>
  <avoid name="timeout_without_a_kill_grace">
    <description>Bounding a measurement run with a plain timeout and assuming the child dies on the first signal.</description>
    <instead>Add a kill grace so an escaped child is forcibly reaped within the job budget.</instead>
  </avoid>
  <avoid name="unverified_benchmark_artifact">
    <description>Assuming the harness compiled your working tree because the benchmark ran and the numbers moved.</description>
    <instead>Force resolution to the workspace and assert that the resolved source and compiler output paths are inside it.</instead>
  </avoid>
</anti_patterns>

<best_practices>
  <practice priority="critical">Measure the noise floor by benchmarking identical code against itself before believing any before/after result.</practice>
  <practice priority="critical">Report the protocol — samples, warmup, process topology, ordering, interval — with every number.</practice>
  <practice priority="critical">Gate on the confidence interval's lower bound; degrade to the point estimate when no interval exists.</practice>
  <practice priority="high">Prefer a deterministic metric (allocations, operation counts, commits, syscalls) over wall-clock time whenever the claim can be carried by one.</practice>
  <practice priority="high">Pair the measurement: alternating order in one process, or identical seeds across stochastic arms.</practice>
  <practice priority="high">Assert output parity on every sample, not once; a fast wrong answer is the default failure of an optimization.</practice>
  <practice priority="high">Declare the adoption criterion before measuring and honor it when it rejects.</practice>
  <practice priority="high">Run benchmark jobs non-blocking by design; diagnose a red result, never let it halt a correctness fix.</practice>
  <practice priority="high">Verify the set of components a summary statistic covers before trusting the statistic; a missing row does not lower an aggregate.</practice>
  <practice priority="medium">Guard the fixture: assert it has the property it is meant to exercise before measuring it.</practice>
  <practice priority="medium">Run instrumented measurements single-worker, and bound every measurement run with a kill grace so a hang cannot consume the job budget.</practice>
  <practice priority="medium">Bound cross-implementation claims to versions, payloads, environment, and output representation.</practice>
  <practice priority="medium">Record rejected experiments with the measurement, the mechanism, and the retry precondition.</practice>
  <practice priority="medium">Measure the non-skewed case as well as the skewed one, so a scheduling improvement can be stated as costing nothing when its premise does not hold.</practice>
</best_practices>

<related_skills>
  <skill name="test-integrity">Correctness tests that report success without exercising the contract; the counterpart to this skill's benchmarks that run without measuring the change</skill>
  <skill name="testing-patterns">Owns the deterministic assertions this skill recommends substituting for timing assertions, with the worked examples; this skill supplies only the argument for which metric to assert</skill>
  <skill name="sbcl-usage">Runtime-specific profiler and timing tool invocation, and coverage instrumentation caveats</skill>
  <skill name="parallelization-patterns">Owns the work-scheduling strategy for skewed data-parallel inputs and its output-ordering discipline; this skill holds the measurement evidence for it</skill>
  <skill name="investigation-patterns">Evidence-based tracing when a benchmark result contradicts the expected mechanism</skill>
  <skill name="quality-tools">Review gates and honesty rules for reported measurements</skill>
</related_skills>

<related_agents>
  <agent name="performance">Owns optimization work; consumes this protocol when producing or defending a number</agent>
  <agent name="test">Designs the deterministic contract assertions that replace timing assertions</agent>
  <agent name="devops">Wires the benchmark job as a non-blocking signal and implements the interval-aware gate</agent>
  <agent name="quality-assurance">Reviews whether a stated performance claim is supported by its protocol</agent>
</related_agents>

<constraints>
  <must>Establish the noise floor before claiming any before/after effect</must>
  <must>Publish the measurement protocol with the number</must>
  <must>Gate on the interval lower bound, with an explicit fallback when no interval is present</must>
  <must>Verify that the artifact measured is the artifact changed</must>
  <must>Bound every claim to the implementations, versions, payloads, and workload measured</must>
  <avoid>Wall-clock assertions in tests and absolute-deadline wait loops</avoid>
  <avoid>Blocking merges on a benchmark result</avoid>
  <avoid>Concluding from a small-sample winner or an unpaired stochastic comparison</avoid>
  <avoid>Reverting a failed experiment without recording its measurement and retry precondition</avoid>
</constraints>
