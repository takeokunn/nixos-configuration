---
name: investigation-patterns
description: Use when tracing a symptom to its cause (debugging, bisecting, or working out how an unfamiliar implementation behaves). Covers evidence-based analysis, bisecting a symptom whose boundary moves between runs, a probe that measures its own gate rather than the phenomenon, and checking a completion claim against the artifact instead of the summary.
version: 3.0.0
---

How to reach a cause you can defend. The standard debugging arc (reproduce, isolate, investigate, hypothesize,
fix) is assumed. This file carries the places where that arc produces a confident wrong answer.

## Evidence

Every finding carries a file:line and a tier, never a score. A confidence number produced in the same pass that
did the investigation never contradicts that investigation, so nothing downstream ever reads a low score and
looks further.

- **verified**: a command was run, or the exact lines were read. The finding carries the command and its
  output, or the citation. Anyone can re-run it and get the same answer.
- **inferred**: derived from evidence actually read, but the conclusion itself was never observed. State the
  evidence *and the inferential step*, so the step can be disputed.
- **assumed**: from convention, prior knowledge, or the user's framing. Nothing here was checked. State what
  would confirm it.

Coverage is an observable boundary, not a percentage: name the files and symbols actually read, name what was
*not* examined and why, and name the query that established the boundary (the Grep pattern, the
`find_referencing_symbols` call, the directory walked) so a reader can re-run it and judge whether it was
exhaustive.

Prefer Serena's symbol tools (`find_symbol`, `get_symbols_overview`, `find_referencing_symbols`) to reading
whole files, and follow a Grep hit with symbol navigation rather than treating the hit as the answer.

## A moving boundary invalidates the observation, not the hypothesis

Bisection (over commits, over input size, over a file's forms) assumes a deterministic oracle. **Once the
boundary shifts under re-probing, every subsequent narrowing step is fitting noise.**

Before continuing to narrow, re-run the identical probe twice and require the identical boundary. If it moves,
the oracle is noisy and the next reduction target is meaningless. Check for interference first:

- Concurrent instances of the same toolchain competing for CPU: look for other long-lived processes of the
  tool under test before trusting any timing-sensitive result.
- A shared build or artifact cache being written by another session.
- Fixed-name temporary files colliding between parallel probe sessions; give probe artifacts a process-unique
  path.

**The failure signature is a long log of "the next reduction target is…" entries with no reproducibility
re-check.** The accumulated narrowing feels like progress and creates pressure to continue, but every entry
after the boundary first moved is unusable. A resolved-without-a-fix outcome confirms the symptom was
environmental: record that conclusion, so the old reduction notes are not later mistaken for live findings.

## A probe inside a guard measures the guard

A counter, sampler, or health check placed inside a conditionally-executed body measures the gate, not the
phenomenon.

Symptom: a metric reads flat zero, or is stuck far below threshold, while the thing it counts is obviously
occurring. Find where the instrumentation lives relative to the guard: if the increment sits inside a
rate-limited, coalesced, debounced, or sampled body, its ceiling is the *gate's* rate, not the event's. Move
the measurement outside the gate, or count gate-closed events explicitly as a separate signal.

**Two independent limiters in the same path compose into a dead zone.** A producer with its own rate limit
calling a consumer with its own coalescing gate: each is defensible alone, and their product makes a downstream
threshold unreachable. Neither component is wrong; the composition is.

Unit tests miss this because tests that call the detection function directly with synthetic counts bypass both
gates: they pass while the live path never reaches the threshold at all.

## A registry that gained a second member kind

A consumer iterating a registry works for most members and produces meaningless or empty results for some.
Check whether every member satisfies the property the consumer reads. **A docstring narrowing the contract
("members must be defined with X") is documentation, not enforcement, and is routinely violated.** Guard the
variant-specific read and give the discrimination a name.

The signal is duplication: when the same inline property-presence check appears at several call sites to tell
variants apart, the missing thing is a named predicate. The duplication is what makes the missing abstraction
visible. A registry with mixed member kinds is also a likely source of "passes but tests nothing" behavior:
see [testing-patterns](../testing-patterns/SKILL.md) for tests that enumerate a production registry.

## Verify a completion claim against the artifact, not the summary

This applies to your own prior work as much as to a claim arriving as a summary.

- When a claim has a machine-readable artifact behind it (a coverage report, a directory listing, a lockfile,
  a build output), read the artifact. "The temporary directories were removed" alongside a listing showing them
  present is exactly the gap a skeptical check exists to close.
- Read the number, not the rounding. A coverage figure reported as complete but measuring fractionally below it
  is hiding a small number of genuinely unexercised branches, and **those branches are where the untested
  behavior lives.**
- Before accepting that a passing test validates a fix, confirm that test's fixtures route through the changed
  path. A test whose doubles substitute the component that was fixed passes for reasons unrelated to the fix,
  and is evidence of nothing.

State which verification tier was actually reached rather than implying the highest; see
[execution-workflow](../execution-workflow/SKILL.md) for the reporting form.

## Before adding a feature to an unfamiliar codebase

Produce a written architecture analysis first. It is the deliverable of the investigation and precedes any
implementation.

- **Existing patterns**: the codebase's governing patterns (state management, event flow, layering, module
  boundaries) with file:line evidence.
- **Reference implementation**: the existing feature that most resembles the one to be added, read as the
  template to imitate. A near-neighbour already-solved feature is the strongest guide to the conventions.
- **Integration points**: the exact file:line locations where new code attaches, and what data each point
  already has in scope.
- **Edge cases and risks**: enumerated, ranked, each with a mitigation.
- **Change surface**: the files to create or modify *and* the files that need no change. Naming the
  no-change set bounds the blast radius and is as valuable as the change list.
- **Effort**: a phased plan with an estimate, stated with its evidence tier and the basis for that tier, never
  a numeric confidence.
- **Protected differences**: when the task is to align one project with a reference (a sibling service, a
  ported module, a second plugin in a family), enumerate the divergences that must survive *before starting*: a
  different auth scheme, a fixed rather than configurable endpoint, an extra handler the reference lacks, files
  the reference has that this project should not. **The failure mode of conformance work is over-normalization**:
  erasing a divergence that existed for a reason. Writing the protected list up front converts an implicit
  judgement call into a checkable constraint.

Prefer reusing an existing abstraction to inventing one. If the existing abstraction is fundamentally
incompatible with the new requirement, say so explicitly and justify a rewrite rather than forcing an
ill-fitting extension.

## Deferred decisions

When work is blocked on an external dependency maturing, record it rather than leaving an open loop or
re-investigating from scratch each time:

- The decision, its date, and the next review date.
- The conditions that must **all** hold to unblock, each with a target and a concrete way to check it: a
  release page, a changelog, a capability list.
- A cadence plus event triggers (on a dependency release, on renewed demand).
- The implementation outline and reference implementations to follow once unblocked.
- What to do if the dependency stalls: seek active forks, choose an alternative, or close with an explanation.
- An append-only review log: date, observed dependency state, outcome.

Make the revisit conditions checkable without re-investigation: name the exact capability (a required protocol
method) or version threshold, so a future review is a lookup rather than a fresh analysis.

## Removing dead code

- Confirm no references with a *semantic* reference search, not a raw occurrence count: package-qualified names
  and re-exported symbols make token counting produce false positives.
- Search source and tests together. Tests may reference private helpers directly, so a source-only search can
  wrongly mark a symbol dead.
- Treat build-system definitions (component manifests, barrel or index files) as the boundary of the removal.
  Deleting a file cleanly usually requires updating the manifest listing it.
- After removal, reload or build the affected unit to prove nothing dangles.
- **An unused-code warning on a symbol registered through an attribute macro, a plugin registry, or a
  foreign-function export is boundary noise, not proof.** The compiler does not treat the generated
  registration path as a call site, so the true caller lies outside the language's reference graph. Verify the
  registration or export path before deleting.

Two reliable candidates: thin compatibility barrels that only re-export concrete modules (point each consumer
at the concrete module, then delete the barrel), and single-use private helpers whose only call sites are local
(inline them, keeping the public entry point as the sole behavioral surface).

## Recurring shapes

| Symptom | Where to look |
|---|---|
| Intermittent, works sometimes | Shared mutable state, async ordering |
| Missing first or last element | Loop boundaries, inclusive vs exclusive index |
| Memory growth, connection exhaustion | Acquisition paths without a matching release on the error path |
| Garbled text | Encoding at each transformation step, not just at the ends |
| Null or undefined at use | Every path reaching the access, not only the one in the trace |

Drill with repeated "why" until the answer is a change someone can make: *server crashed → out of memory →
connection pool exhausted → connections not released → an exception bypasses cleanup*: the root cause is the
missing try/finally, not the memory. Where timestamps exist, order the events and find where the sequence
diverges from the expected one.

## Rules

- Complete the investigation before proposing a solution, and propose without implementing.
- Never confirm the user's assumption without independently verifying it.
- Never state a claim without a file:line, and never substitute a numeric confidence or coverage score for an
  evidence tier.
- Where a finding is still inferred, check a second source to see whether it can be raised to verified.
- Document the information gaps rather than closing them by plausibility.

## Related

- [serena-usage](../serena-usage/SKILL.md): memory operations and symbol-level navigation
- [execution-workflow](../execution-workflow/SKILL.md): implementing the fix once the cause is established
- [fact-check](../fact-check/SKILL.md): verifying external documentation and library behavior
- [testing-patterns](../testing-patterns/SKILL.md): adding the regression test afterwards
- [requirements-definition](../requirements-definition/SKILL.md): when the investigation reveals unclear requirements
