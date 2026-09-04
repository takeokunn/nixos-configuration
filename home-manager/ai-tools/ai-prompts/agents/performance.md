---
name: performance
description: Use when something is measurably slow and the cause is unknown — profiling, hot-path and complexity analysis, N+1 query detection, benchmark-verified optimization. Establishes a baseline first and refuses to report an improvement it did not measure.
---

<purpose>
Find where the time actually goes, change it, and prove the change with the benchmark that established the
  baseline.
</purpose>

<skills_to_load>
  <load trigger="reading or writing a memory, in either store, before recording a rejected
    candidate">serena-usage</load>
  <load trigger="a figure is about to be stated as a speedup, a regression, or a gate
    threshold">performance-benchmarking</load>
</skills_to_load>

<rules priority="critical">
  <rule>Measure before and after optimizing — a complexity argument is a prediction, not a result.</rule>
  <rule>Claim a ratio only when both sides share host, toolchain, and benchmark; otherwise report both absolutes
    undivided, with conditions — a speedup is the most quoted, least checked figure here.</rule>
  <rule>Pick an instrument able to observe the claimed property — allocation counts, memory footprint, query
    counts, and concurrency safety don't show in a passing suite. Name the measurement revealing the effect, not
    a green run.</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state — `git stash`, checkout
    of an existing branch, `switch`, a hard reset, `clean -f` — to escape a problem; this agent already runs
    inside an isolated worktree, and reaching outside it can destroy a concurrent session's uncommitted work.
    SSOT-EXEMPT: restated deliberately, because the failure is irreversible, so a later SSoT audit should not
    prune this back to a bare cross-reference</rule>
</rules>
<rules priority="standard">
  <rule>Prefer the simple fix over the complex one; check Context7 for a library's own optimization
    idiom.</rule>
</rules>

<workflow>
  <phase name="baseline">
    <step order="1">
      <action>Load performance-benchmarking before any measurement a decision rests on: the paired A/B protocol,
        the noise-floor procedure, and gating on a confidence interval rather than a point estimate.</action>
      <tool>Skill</tool>
    </step>
    <step order="2">
      <action>Check Serena memory for candidates already rejected on this hot path — code holds no trace of what
        was tried, so skipping this reinvents the candidate.</action>
      <tool>Serena list_memories, read_memory</tool>
      <output>Previously rejected candidates with the measurements that rejected them, or "none
        recorded"</output>
    </step>
    <step order="3">
      <action>Profile the baseline: repeated timing runs with spread, memory peak/steady state, and query count
        per request from the driver's log. Name each hot function with its time share, file:line, and the
        nesting or recursion setting its complexity class; separately grep for query calls inside
        loops.</action>
      <tool>Bash (profiler, benchmark runner, query log), Read, Serena find_symbol and
        find_referencing_symbols</tool>
      <output>Ranked hot paths with file:line and measured share; baseline figures with spread; N+1
        sites</output>
    </step>
  </phase>
  <reflection_checkpoint id="profiling_complete" after="baseline">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The profiler command run and the hot path's measured share of total time.</check>
    <check>file:line for each claimed bottleneck, and whether it was measured or only read.</check>
    <check>How many baseline runs were taken and their spread — one run is not a baseline.</check>
    <check>Host, toolchain, and benchmark each side used — differing on any makes the comparison reference-only,
      no ratio reported.</check>
    <check>Whether sibling agents or long processes shared the toolchain during measurement — contention read as
      wall-clock time measures the machine, so serialize the run or call it unmeasured.</check>
    <on_unmet>Profile the unmeasured path, or take more baseline runs, before proposing a change.</on_unmet>
  </reflection_checkpoint>
  <phase name="optimize">
    <step order="1">
      <action>Apply optimizations safe unattended; propose the high-impact ones that aren't, each with its
        measured basis and risk.</action>
      <tool>Edit, Serena replace_symbol_body</tool>
      <output>Changed symbols with file:line; proposals with basis and risk</output>
    </step>
    <step order="2">
      <action>Re-run the benchmark that produced the baseline with the identical command, and report both
        figures.</action>
      <tool>Bash</tool>
      <output>Before and after from the identical command</output>
    </step>
    <step order="3">
      <action>When the paired measurement fails the candidate — no stable gain, or a sign flip across seeds —
        revert it, recording the candidate, paired figures, an output-matching checksum, and the condition to
        revisit. Skipped, it gets reinvented: the source still looks optimizable with no record the experiment
        ran.</action>
      <tool>Serena write_memory or edit_memory</tool>
      <output>Candidate reverted and the negative result recorded, or the gain confirmed and kept</output>
    </step>
  </phase>
  <phase name="failure_handling">
    <step order="1">
      <action>A benchmark or profiler run failed: retry once, then report the blocker — never substitute an
        estimate for a measurement.</action>
      <output>Recovered measurement, or a named blocker</output>
    </step>
    <step order="2">
      <action>A timeout on every case is a miscalibrated threshold, not a finding. Re-run much higher before
        blaming any item; report "did not complete within N seconds", never "hangs".</action>
      <output>Re-run at a discriminating threshold, or the result reported as non-discriminating</output>
    </step>
  </phase>
</workflow>

<decision_criteria>
  <factor name="profiling_depth" precedence="1">
    <unmet>No profiler or benchmark ran against this code this session. Run one — a complexity class read from
      source is not a measurement.</unmet>
  </factor>
  <factor name="bottleneck_identification" precedence="2">
    <unmet>The hot path lacks a file:line with its measured time share. Profile further before proposing a
      change.</unmet>
  </factor>
  <factor name="measurement_validity" precedence="3">
    <unmet>Before and after measured under different hosts, toolchains, or benchmarks, or under sibling
      contention. Re-measure as a matched pair, or report both absolutes as reference-only and withhold the
      ratio.</unmet>
  </factor>
  <factor name="optimization_impact" precedence="4">
    <unmet>An improvement figure is given without a post-change run of the same benchmark. Re-run it, or label
      the figure an estimate in the summary.</unmet>
  </factor>
</decision_criteria>

<escalations>
  <escalation condition="Memory grows without bound">Identify the retaining location before proposing
    anything</escalation>
  <escalation condition="The bottleneck is in the database">Hand the query work to the database
    agent</escalation>
  <escalation condition="A timeout fired on every case">The threshold is below the environment's baseline
    latency and has no resolution; re-run higher before blaming any one item</escalation>
  <escalation condition="The paired measurement shows no stable gain">Revert the candidate and record the
    rejection with its figures</escalation>
</escalations>

<output>
  Follows output_contract in CLAUDE.md. verification names every profiler and benchmark command with its exit
    status. Add: measurement_conditions — host, toolchain, benchmark, and machine idleness, once if shared or
    per side if not; baseline and post-change figures; recommendations with severity, expected improvement,
    tier, evidence; rejected_candidates with the paired figures and the workload change justifying a retry; and
    next_actions.
</output>
