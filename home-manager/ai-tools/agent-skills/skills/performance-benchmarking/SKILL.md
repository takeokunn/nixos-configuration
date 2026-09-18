---
name: performance-benchmarking
description: Use when producing, gating, or reviewing performance numbers - before/after benchmarks, speedup/regression claims, CI benchmark gates, noise floor and confidence interval, wall-clock vs allocation-count metrics, and wording a performance claim.
metadata:
  version: "3.0.0"
---

Producing performance numbers that mean something. **The organizing fact is that a benchmark comparing
identical code against itself can report a confident-looking improvement**; every rule here follows from taking
that seriously.

## Vocabulary

**Noise floor**: the magnitude of difference a benchmark reports when nothing has changed. A property of the
harness, machine, and workload, not of the code, and the threshold below which any measured effect is
*unmeasured* rather than absent.

**Point estimate vs interval**: the point estimate is the headline; the interval is the part that says whether
the headline means anything. Reading one without the other discards exactly the information that distinguishes
a result from noise.

**Paired measurement**: measuring both arms under the same conditions, so shared variance (machine load, cache
state, scheduler behavior) cancels in the difference instead of accumulating in it.

**Gate vs signal**: a gate blocks a merge under an agreed performance budget and measurement policy; a signal
informs a reviewer. Use advisory results by default unless the project explicitly requires a performance gate.

**Deterministic contract**: a property that does not vary with machine speed: allocations, commits, subprocesses
spawned, iterations. Asserting one turns a flaky timing test into a deterministic one.

## Establish the noise floor first

A benchmark harness does not report "no difference" when there is no difference. In a measured example,
benchmarking **identical code against itself** (same commit, no change at all) produced a +5.0% point
estimate with a 95% interval of +0.5%..+10.3%. Both the headline and the interval's lower bound cleared zero.
**Any workflow that would have accepted "+5% improvement, interval excludes zero" would have accepted a change
that did not exist.**

Run the benchmark twice on the same commit and record the spread. That spread is the floor for that harness, on
that machine, for that workload; and it is re-established when any of the three changes.

A harvest across 19 real repositories supplies a concrete case of skipping this. An allocation-only change,
benchmarked once (a single before/after run per input size, not alternating, not repeated) produced results
that were internally incoherent in a way that indicted the harness rather than the change. Sibling benchmarks
reported a confirmed regression, a near-threshold miss, and a confirmed win simultaneously, and one measurement
swung from +9.5% to -28% between two adjacent input sizes. An allocation-only change cannot produce all three
verdicts at once, and it cannot flip sign between adjacent input sizes exercising the same code path:
**the incoherence is the diagnostic, and it indicts the harness, not three independent findings to report one by
one.** The missing discipline is procedural: alternate A/B/A/B instead of running one arm to completion before
the other, and require the effect's sign to replicate across at least two runs before reporting it: a single
run cannot distinguish a real effect from the harness's own spread.

**Below the floor is unmeasured, not absent.** "We measured no regression" and "the regression, if any, is
smaller than our noise floor" are different statements, and only the second is supportable. Reporting the first
converts a limitation of the instrument into a property of the code. If it matters, raise the resolution (more
samples, quieter machine, larger workload) or switch to a metric that has no floor.

**An unqualified percentage is not a result.** A single number carries no information about how it was
obtained, so it cannot be reproduced, contested, or compared against a later run. Report sample count, warmup,
process topology, ordering scheme, and the interval. A number without its protocol is an anecdote.

## Protocol

Choose a process topology deliberately:

- **Independent processes**: each arm in its own fresh process, repeated N times. Removes in-process state
  carryover (caches, heap shape, compilation state) at the cost of paying startup per sample. A concrete
  instance: five processes per arm, each with 200 warmup and 2,000 measured operations.
- **Alternating pairs in one process**: both arms in A/B, B/A order, compared within each pair. Removes
  machine-level drift because the halves of a pair are adjacent in time. A concrete instance: 6 warmup pairs
  then 21 alternating measured pairs, with a full collection before each sample.

**Never run all of A and then all of B in one process.** Any drift over the session (thermal, heap growth,
background load) is then indistinguishable from the effect.

State the **warmup count** explicitly and exclude it: the first samples measure compilation, page faults, and
cold caches, and an undeclared warmup makes two runs incomparable. Force a **full garbage collection before
each measured sample**, or a collection triggered by arm A's allocation pattern lands inside arm B's sample and
is attributed to B.

Report the **median of the paired differences plus the win count**: "B was faster in 13 of 15 pairs". The win
count is less affected by a single outlier pair than a mean of ratios, and it is immediately interpretable.

A complete disclosure looks like: 15 measured pairs (30 arm measurements), alternating order, 2 warmup pairs
excluded, full collection before each measurement, output signature identical in all 30 measured results.

**Instrumented runs are single-worker.** Coverage and allocation instrumentation typically maintains
process-global mutable counters, so running instrumented work across concurrent workers produces
nondeterministic per-file undercounts **even though every assertion still passes**: the numbers are wrong in a
way nothing in the run reports. A related trap: instrumentation binds counters to a source identity established
at compile time, so manually compiling and loading copied sources detaches the counters from the identity the
report keys on, yielding files that appear at exactly zero. Load instrumented sources through the build system
after resetting the instrumentation. **Parallelism is a property of the fast unmeasured run, not the measured
one.**

**Bound the run with a kill grace.** A plain timeout is not sufficient: a runtime that installs a termination
handler or blocks signals can remain alive after its first signal, so a nominal limit leaks past the budget it
was supposed to enforce. Use `timeout --foreground -k 10s <limit>s`. The escalation from a polite signal to an
unconditional kill must be part of the design, not an assumption about the child's cooperation.

## Three ways a clean run measures the wrong thing

**Prove you are measuring your working tree.** A development environment or dependency cache can pre-register a
published build of the very package you are benchmarking, and the harness then compiles and measures *that*
while your edits sit unread on disk. Benchmark metadata such as a reported source root does not prove which
artifact the compiler consumed. Force resolution to the working tree and verify it: reinitialize the module
registry against the workspace, clear the registered system, load its definition by absolute workspace path,
force recompilation, then **assert that the resolved source file and the compiler's output paths point into the
workspace** rather than into a package store.

**The fixture must pass a correctness guard first.** A malformed fixture frequently normalizes to the empty or
trivial case rather than failing. In one instance a fixture intended to exercise a styled-output path was not
interpreted as a style specification at all; after normalization it produced no style, the computed diff length
was zero, and **the benchmark measured the unchanged case while reporting fine numbers.** Assert the property
the fixture is supposed to have (non-zero diff length, non-empty result set, the expected branch taken)
before measuring it.

**Assert output parity on every sample.** A fast wrong answer is the default failure mode of an optimization,
and checking the output once does not cover the case where the optimized arm is correct on the first input and
wrong on a later one, or is nondeterministic. Compare a cheap signature (a result hash, a record count, a
byte-identical report) on every sample of every arm, and report that it was constant as part of the result.
"Identical result hash in all 120 samples" has proved something a spot-check has not.

## Choose the metric

Wall-clock time is the metric people want and the worst-behaved one available. **When it is too noisy to
support a claim, the answer is usually to claim a different metric rather than to keep collecting samples.**

Allocation counts, operation counts, and syscall counts are exact: two runs of the same code produce the same
number. When both are collected and only one is stable, **the stable one is the result and the other is an
indicator.**

Worked examples: a roughly 5.1% wall-clock improvement recorded as "observation including noise" while the
confirmed effect was stated as a bounded reduction of at most eight cons cells per operation; a comparison
where the timing was a single-sample indicator while the removed allocation slope was the confirmed result; and a
wall-clock comparison rejected outright because all arms showed correlated load spikes and per-trial spreads of
3.6x to 7.1x: **a noise floor larger than any plausible effect.**

**A timing assertion measures the machine, not the change.** Inside a test suite it fails on a loaded runner
and passes on a fast laptop independently of whether the regression it was written to catch has occurred, so
its threshold is a noise floor with a pass/fail attached. The regression worth catching is usually structural
(a per-item operation that should have been per-batch), **and structure is exactly what a deterministic count
measures and a duration does not.** Hand the finding to the suite as a deterministic contract;
[testing-patterns](../testing-patterns/SKILL.md) owns how to write that assertion.

**Decrementing budgets, not absolute deadlines.** A loop terminating by comparing the current clock against a
stored deadline is exposed to clock jumps, which can lengthen, shorten, or entirely prevent the intended
timeout. Carry a remaining budget and subtract each wait, so termination depends only on elapsed slices and a
slow machine degrades into fewer iterations rather than a hang. **Validate the input**: a decrementing budget
is finite only if its input is, and positive infinity minus a slice is still positive infinity, a concrete
nontermination counterexample. Cover NaN, both infinities, zero, negative, and non-numeric when the budget is
caller-supplied.

## Comparing stochastic arms

**Pair the seeds.** Running arm A on one set of random draws and arm B on another means the comparison carries
the variance of both draw sets. Generate the scenario set once from a fixed seed, run every arm against that
same set, and report the paired difference rather than the two means.

**Size the effect against its own noise.** A bare gap between two means is uninterpretable without knowing how
much the means themselves move between runs. Report the difference relative to its standard error; if the error
swamps the mean, the conclusion is "not significant at this sample count", not "A is slightly better".

**Re-run at higher N before concluding.** Small samples under-represent rare high-magnitude outcomes, so the arm
that avoids them by luck looks better. In a measured case a 75-sample comparison produced a confident winner;
the same comparison at 300 paired samples showed the difference was not significant and that both arms moved in
the same direction, **because the small run had under-sampled the heavy tail.** Treat a small-sample winner as
a hypothesis, and record the sample count in the claim.

## Declare the adoption criterion first

A criterion chosen after the measurement is chosen to fit the measurement. **Declaring thresholds in advance is
what makes a rejection possible at all**: without one, every result is an improvement in some metric.

Write it as explicit thresholds on named metrics plus a no-regression clause: primary metric improves by at
least 2%, allocation improves by at least 3%, neither regresses. Then apply it mechanically.

**The value of a pre-declared criterion is realized only where it says no.** Two worked rejections: a candidate
that cut allocation by 30.6% but cut throughput by 34.8%, rejected because the workload was throughput-focused;
and a candidate whose median throughput moved -1.00% with allocation -0.76%, rejected on the throughput
regression despite the allocation win. Record the rejection with its numbers: **that record is what stops the
candidate being re-proposed.**

## Gating

**Define the direction and uncertainty policy.** For a metric where positive means regression, an
interval-based gate can classify a regression when the lower bound exceeds the agreed threshold. This relies
on the sampling model and a calibrated harness; an interval is not a guarantee against systematic bias.

**Keep inconclusive distinct from pass.** If the interval crosses the threshold, report the estimate and
interval as inconclusive. Define beforehand whether that status blocks or requests another measurement;
do not change an existing gate's policy to make a result green.

**Missing required uncertainty is an invalid measurement.** If a gate requires an interval, do not silently
substitute a point estimate or pass when the interval is absent. Report the missing field and follow the
declared invalid-result policy. A legacy point-estimate gate remains a different, explicitly named policy.

**An aggregate percentage needs a denominator manifest.** A gate accepting an aggregate from a report trusts
the report's own choice of denominator, and a file dropped from the report entirely (never instrumented, never
loaded, excluded by a path pattern) **does not lower the aggregate; it vanishes from it.** So a report can
show a perfect total precisely because the interesting components are missing. Compare normalized row
identifiers against a declared manifest and reject when an entry has no row, a row is malformed, or a row's
total is zero, *before* evaluating the aggregate. **Verify the set of things summarized, not just the summary.**

## Advisory results and required gates

For a new advisory benchmark, document its non-blocking status and investigate regressions rather than
ignoring them. For a required performance gate, preserve its budget and failure policy. A correctness fix
that exceeds that budget needs an explicit tradeoff decision from its owner, not an automatic override or
a change to `continue-on-error`.

## Wording the claim

**Cross-implementation results are not rankings.** Implementations that appear comparable often differ in what
they return: a parser producing vectors and hash tables is not doing the same work as one producing lists and
association lists, **so a raw timing comparison measures the representation choice as much as the
implementation quality.** Bound every cross-implementation claim to the implementations, versions, environment,
payloads, and the canonical output representation compared. An unqualified universal superiority claim is
unsupportable by any benchmark you can actually run.

**Enumerate your own caveats**, in the same place as the number. They are known to the author at measurement
time and to nobody else afterwards, and omitting them does not make the result stronger: it makes it
unfalsifiable and easy to dismiss when someone fails to reproduce it. A worked example enumerates: tool
versions not pinned, trials not perfectly order-balanced, each trial a fresh process but excluding startup and
load time, a single synthetic workload, concluding explicitly that this baseline cannot by itself substantiate
a general claim.

## Negative results

An optimization that was tried and did not work is durable knowledge, **and it is the knowledge most reliably
lost between sessions.** A reverted experiment leaves no trace in the source tree, so the same idea is near
certain to be re-proposed; and a bare "we tried that" is not enough to stop it.

Record three things: what was changed, what was measured, and **the explicit precondition for retrying.**
Worked forms: "do not retry these no-op checks on this path without call-site frequency evidence and a profiler
result"; "do not retry loop-wide scratch binding without first explaining the compiler and allocation
interaction and proving improvement in the same isolated benchmark"; and, for a change measured at 0.006%
difference, "do not re-attempt without new evidence" together with the mechanism that explained the null result:
a large growable buffer grows by remapping pages rather than by copying into a second resident allocation.

**A null result needs a mechanism.** "It made no difference" is a measurement; "it made no difference because
the cost is not where we assumed" is an explanation that also invalidates the next three variants of the same
idea. Pair every null result with its mechanism, or state that the mechanism is unknown, itself a useful flag
that the area is not understood well enough to optimize.

## Throughput evidence

**Measure the arm where the premise does not hold.** A scheduling change motivated by skew will look good on
the skewed workload that motivated it: **that arm proves the mechanism works, not that the change is free.**
The workload where the premise does not hold can expose a tradeoff hidden by the motivating workload;
include both before making an improvement claim.

Compare skewed and evenly sized workloads, reporting uncertainty and the noise floor for each. A point
estimate such as 1.004x alone does not establish zero cost or a strict improvement. Because a scheduling
change can also perturb output order, pair timing with the required output-equivalence check on both workloads:
the ordering discipline that comparison checks belongs to
[parallelization-patterns](../parallelization-patterns/SKILL.md).

**Throttle expensive probes independently of cheap polling.** A polling loop usually has one interval, but not
all of its probes cost the same: a file read at 50 ms is fine; a probe that spawns a child process at 50 ms is
a process storm that dominates the cost of the thing being monitored. Give each probe its own rate limit, and
where a grace period applies, do not begin the expensive probe until it has elapsed. A measured instance
reduced a ten-second wait from roughly 195 subprocess spawns to at most 40, by starting the expensive probe
after a 250 ms grace period and running it at most once per 250 ms while leaving the cheap poll at 50 ms.

## Related

- [test-integrity](../test-integrity/SKILL.md): correctness tests that report success without exercising the contract
- [testing-patterns](../testing-patterns/SKILL.md): the deterministic assertions that replace timing thresholds
- [parallelization-patterns](../parallelization-patterns/SKILL.md): the scheduling strategy this measures
- [sbcl-usage](../sbcl-usage/SKILL.md): runtime-specific profiler invocation and instrumentation caveats
- [investigation-patterns](../investigation-patterns/SKILL.md): when a result contradicts the expected mechanism
