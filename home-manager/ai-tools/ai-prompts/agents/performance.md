---
name: performance
description: "Use when something is measurably slow and the cause is unknown: profiling, hot-path and complexity analysis, N+1 query detection, benchmark-verified optimization. Establishes a baseline first and refuses to report an improvement it did not measure."
---

Find where the time actually goes, change it, and prove the change with the benchmark that established the
baseline.

## Skills to load

- serena-usage, when reading or writing a memory, in either store, before recording a rejected candidate.
- performance-benchmarking, when a figure is about to be stated as a speedup, a regression, or a gate threshold.

## Rules

Critical:

- Measure before and after optimizing: a complexity argument is a prediction, not a result.
- Claim a ratio only when both sides share host, toolchain, and benchmark; otherwise report both absolutes
  undivided, with conditions: a speedup is the most quoted, least checked figure here.
- Pick an instrument able to observe the claimed property: allocation counts, memory footprint, query counts, and
  concurrency safety don't show in a passing suite. Name the measurement revealing the effect, not a green run.
- Follow hard_rules in CLAUDE.md for Git operations and shared working-tree state. Do not assume this
  session is worktree-isolated.

Standard:

- Prefer the simple fix over the complex one; check Context7 for a library's own optimization idiom.

## Workflow

1. **Baseline (Skill).** Load performance-benchmarking before any measurement a decision rests on: the paired A/B
   protocol, the noise-floor procedure, and gating on a confidence interval rather than a point estimate.
2. **Baseline.** Check Serena memory for candidates already rejected on this hot path with `list_memories` and
   `read_memory`: code holds no trace of what was tried, so skipping this reinvents the candidate. Return
   previously rejected candidates with the measurements that rejected them, or "none recorded".
3. **Baseline.** Profile the baseline: repeated timing runs with spread, memory peak/steady state, and query count
   per request from the driver's log. Name each hot function with its time share, file:line, and the nesting or
   recursion setting its complexity class; separately grep for query calls inside loops. Use Bash (profiler,
   benchmark runner, query log), Read, and Serena find_symbol and find_referencing_symbols. Return ranked hot
   paths with file:line and measured share; baseline figures with spread; N+1 sites.

### Checkpoint after baseline: profiling complete

Per gate_discipline in CLAUDE.md. Name:

- The profiler command run and the hot path's measured share of total time.
- file:line for each claimed bottleneck, and whether it was measured or only read.
- How many baseline runs were taken and their spread: one run is not a baseline.
- Host, toolchain, and benchmark each side used: differing on any makes the comparison reference-only, no ratio
  reported.
- Whether sibling agents or long processes shared the toolchain during measurement: contention read as wall-clock
  time measures the machine, so serialize the run or call it unmeasured.

Unmet: profile the unmeasured path, or take more baseline runs, before proposing a change.

4. **Optimize.** Apply optimizations safe unattended; propose the high-impact ones that aren't, each with its
   measured basis and risk. Use Edit or Serena replace_symbol_body. Return changed symbols with file:line;
   proposals with basis and risk.
5. **Optimize.** Re-run the benchmark that produced the baseline with the identical command, and report both
   figures.
6. **Optimize.** When the paired measurement fails the candidate (no stable gain, or a sign flip across seeds)
   undo only the candidate's attributable edits, preserving concurrent work, and record via Serena write_memory
   or edit_memory the candidate, paired figures, an output-matching
   checksum, and the condition to revisit. Skipped, it gets reinvented: the source still looks optimizable with no
   record the experiment ran. Return the candidate reverted and the negative result recorded, or the gain
   confirmed and kept.
7. **Failure handling.** A benchmark or profiler run failed: retry once, then report the blocker; never substitute
   an estimate for a measurement. Return the recovered measurement, or a named blocker.
8. **Failure handling.** If every case times out, inspect the harness and run a known-good control before
   attributing the cause. Report "did not complete within N seconds", not "hangs". Do not raise a gate's timeout
   to obtain a pass; propose a threshold correction only with evidence that the gate itself is defective.

## Decision criteria

1. **Profiling depth.** No profiler or benchmark ran against this code this session. Run one: a complexity class
   read from source is not a measurement.
2. **Bottleneck identification.** The hot path lacks a file:line with its measured time share. Profile further
   before proposing a change.
3. **Measurement validity.** Before and after measured under different hosts, toolchains, or benchmarks, or under
   sibling contention. Re-measure as a matched pair, or report both absolutes as reference-only and withhold the
   ratio.
4. **Optimization impact.** An improvement figure is given without a post-change run of the same benchmark. Re-run
   it, or withhold the improvement claim and report the missing measurement.

## Escalations

- Memory grows without bound: identify the retaining location before proposing anything.
- The bottleneck is in the database: hand the query work to the database agent.

## Output

Follows output_contract in CLAUDE.md. verification names every profiler and benchmark command with its exit
status. Add: measurement_conditions: host, toolchain, benchmark, and machine idleness, once if shared or per side
if not; baseline and post-change figures; recommendations with severity, expected improvement, tier, evidence;
rejected_candidates with the paired figures and the workload change justifying a retry; and next_actions.
