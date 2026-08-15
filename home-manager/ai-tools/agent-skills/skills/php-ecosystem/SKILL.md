---
name: php-ecosystem
description: Use for modern PHP, covering Composer, PHPUnit or Pest, PHPStan or Psalm, PSR, and Eloquent. Also covers eager-loading pitfalls (polymorphic relations, N+1 versus memory tradeoffs), read-shape design, and concurrent fan-out failure semantics.
version: 3.0.0
---

Traps and non-obvious behavior for modern PHP (8.5+) work: version-pin hygiene across a toolchain, Eloquent
read-shape and eager-loading failures that look correct in review, caching/concurrency failure semantics, and
tool-specific footguns in PHPStan/Rector. Skip anything a competent PHP developer would already know — this is
where correct-looking code is wrong and nothing errors.

## PHP version bumps are cross-cutting, not a one-line edit

The language version is pinned independently in several places. If any one lags, static analysis and CI
validate against a different language level than the code actually runs on — a silent, confidence-eroding
split. Bump every pin in one pass and verify with the full check suite:

- `composer.json`'s `"php"` constraint
- the local toolchain pin (Nix devenv, asdf/mise, or equivalent)
- Docker base images, both CLI and FPM variants
- CI's setup-php (or equivalent) version input
- PHPStan's `phpVersion` (e.g. `80500`)
- Rector's `->withPhpSets()` target

After upgrading the static-analysis toolchain itself (a PHPStan major, or its framework extension),
**regenerate `phpstan-baseline.neon` rather than hand-editing it** — the new version surfaces and reclassifies
errors differently, and a stale baseline hides real regressions. A new rule that would require broad mechanical
changes across legacy code (e.g. banning inline ignore comments) can be disabled deliberately and tracked as
separate debt rather than blocking the upgrade.

`composer audit` may report a low-severity advisory pinned by a transitive dependency you don't control. Record
the advisory and why it's deferred instead of silently ignoring the audit, so the decision is auditable and
gets revisited when the constraint lifts.

PHP 8.5 (Nov 2025) added the pipe operator (`|>`), `clone $obj with {...}`, and the `#[\NoDiscard]` attribute
(warns when an important return value is ignored) — recent enough to be worth naming if you're checking
whether code could use them.

## Rector's LevelSetList is dead

`LevelSetList` (e.g. `UP_TO_PHP_85`) has been deprecated since Rector 0.19.2. Use `->withPhpSets(php85: true)`
instead — the old API still runs without error, so nothing forces the migration until you notice the deprecation
warning yourself.

## PHPStan level 10 changes what "mixed" means

Level 10 (PHPStan 2.0+) treats *implicit* mixed (a missing type) as strictly as explicit `mixed` — code that
passed at level 9 can fail at 10 purely from omitted annotations, not new bugs. Pair it with
`phpstan-strict-rules` for additional checks like `===` enforcement. Start existing projects at level 5-6 and
new projects at 9-10; jumping an existing codebase straight to 10 produces a wall of noise that has nothing to
do with the change you're making.

## Eloquent / ORM read-shape traps

Push persistence behind a Repository rather than calling the ORM directly from controllers or services — it's
the seam for testing and for swapping persistence, and it keeps query concerns out of domain code. Load
relations explicitly with `with()`/`loadMissing()` instead of touching a relation inside a loop, which triggers
one query per iteration — N+1 is the most common ORM performance regression and is invisible until the
collection grows.

**A dotted eager-load path does not survive a polymorphic hop.** `with('commentable.author')` reads like
ordinary nested eager loading, but the ORM has no single relation to resolve after the polymorphic segment —
the targets are heterogeneous types, each with its own relation definitions — so the nested part silently
degrades to a query per concrete type. The N+1 the change was supposed to remove is still there, and it passes
review precisely because the syntax is indistinguishable from the case that works:

```php
// Silently ineffective: nothing after the polymorphic hop is eager loaded
$comments = Comment::with('commentable.author')->get();

// Explicit: each concrete target type declares its own nested loads
$comments = Comment::with(['commentable' => static fn ($morphTo) => $morphTo->morphWith([
    Article::class => ['author'],
    Video::class   => ['author', 'channel'],
])])->get();
```

A convention-conformance review ("relations are eager loaded, not lazy") approves the broken line; only a
behavioral check — asserting the query count — catches it. See
[execution-workflow](../execution-workflow/SKILL.md) for when a conformance pass is insufficient evidence, and
[testing-patterns](../testing-patterns/SKILL.md) for how to write the query-count assertion.

**Eager loading breadth trades query count for memory, and that tradeoff is invisible once the query log is
clean.** Each added relation hydrates its rows into objects for every parent row, and growth is multiplicative
when a collection relation hangs off another collection relation — a page can end up holding several times the
rows it held before the N+1 "fix". Check whether the consumer actually renders the relation before adding it;
an unrendered relation is cheaper to delete than to load. For the ones you keep, constrain them (a closure that
filters, orders, or limits, or an explicit column list) instead of loading them whole.

**One read shape does not fit every consumer.** A list, picker, or autocomplete endpoint needs only identifiers
and labels. Routing it through the rich domain read pays for a search-engine round trip, entity hydration, and
relation loading whose results it immediately discards. Give that consumer its own repository method that goes
to the query builder with an explicit column list and an early limit, returning plain id/label pairs — this is
interface segregation at the read layer, not a shortcut. The cost: two query paths now exist over the same
data, so a filter, scope, or visibility rule added to one must be added to the other. Keep them adjacent in the
same repository and cover both with tests so the divergence is caught rather than discovered.

Represent exact monetary amounts as integers in minor units, never floats — floating point cannot represent
decimal currency exactly. A workable rule set: prices are strict integers, discounts round down (consistent
with consumption-tax handling), and cost figures may permit float where they're estimates rather than charged
amounts. Back enums with descriptive string values and store those strings, not integers — string values keep
stored data self-describing and stable across enum reordering, where integer backing couples the schema to
declaration order.

## Caching

Read-heavy, non-critical endpoints (dashboards, homepages, rankings) are the highest-leverage caching target:
same expensive read on every request, tolerant of slightly stale data. The design work is in invalidation and
failure behavior, not the cache call itself.

Wrap the read in a `remember()`-style helper with an explicit TTL, gate it behind a config feature flag (so it
can be disabled without a code change), and namespace the keys by prefix — the flag and namespace make the
cache observable and reversible in production:

```php
return Cache::remember(
    "homepage:daily_ranking",
    300, // seconds
    fn () => $this->repository->dailyRanking(),
);
```

A stampede happens when a hot key expires and many concurrent requests all recompute it at once. Evaluate the
risk along four axes before choosing mitigations: **concurrency** (how many requests hit the key within the
recompute window), **data size** (cost of a single recomputation), **TTL** (shorter means fresher but more
frequent expiry events), and **criticality** (presentational data tolerates brief staleness; transactional data
does not). Mitigations: tune the TTL so expiry lands in lower-traffic periods; wrap the recompute in a lock
(`Cache::lock`) so only one request rebuilds while others wait or serve stale; warm the cache shortly before
expiry so the key never actually goes cold under load; wrap the cache backend in try/catch so a cache outage
degrades to a direct read instead of an error.

Monitor the cache hit ratio in production and alert when it drops below an expected threshold — a falling hit
ratio is the leading indicator of a misconfigured key, an unexpectedly short effective TTL, or a stampede,
visible before latency regresses.

## Concurrent fan-out changes failure semantics silently

Fanning out independent reads concurrently — via the framework's concurrency helper, or an async runtime built
on Fibers — turns the sum of their latencies into the max. What gets skipped is the second half: the change to
failure semantics is not visible in the diff.

Fan out only when all four hold: the operations are independent of one another, I/O-bound rather than
CPU-bound, touch disjoint state (no shared connection, transaction, or mutable accumulator), and the response is
bounded by the slowest branch rather than the sum of work. If any one fails, sequential execution is both
simpler and correct — fanning out over a shared database connection or inside an open transaction is a
correctness bug, not a slower optimization.

**Concurrent fan-out fails fast by default: one branch throwing discards the whole response, including branches
that already succeeded.** Decide which behavior you want instead of inheriting the default. For a search box,
fail-fast is right — a silently incomplete result set is worse than an error, because the user reads absence as
"no matches" and acts on it. For a dashboard of independent widgets, per-branch rescue is right — degrade the
failed section to its own empty-with-error state and render the rest. Whichever you pick, make it visible in
the code: wrap each branch in its own rescue, or state in the method's contract that the aggregate is
all-or-nothing. The decision hinges on whether a partial result is distinguishable from a complete one by the
person reading it — when it isn't, degrade nothing.

## OPcache in production

Set `opcache.validate_timestamps=0` in production and clear the cache on deploy — with timestamp validation on
(the default, useful in dev), OPcache still checks file mtimes on every request, and turning it off without a
deploy-time cache clear serves stale bytecode after a deploy indefinitely. JIT (`opcache.jit=tracing`) gives its
biggest wins to CPU-intensive work; for a typical I/O-bound web app it's not the lever to reach for first.

Fibers are low-level primitives — prefer an async library (ReactPHP, AMPHP, Revolt) that uses Fibers internally
rather than hand-rolling coroutine control flow on top of them directly.

## Related

- [execution-workflow](../execution-workflow/SKILL.md) — when a conformance-only review pass is not enough
  evidence that a query-shape fix actually works.
- [testing-patterns](../testing-patterns/SKILL.md) — test strategy, including how to write a query-count
  assertion for eager-loading fixes.
- [context7-usage](../context7-usage/SKILL.md) — fetch current PHP/library documentation instead of relying on
  possibly-stale training data.
- [serena-usage](../serena-usage/SKILL.md) — symbol-level navigation for class and interface definitions.
- [sql-ecosystem](../sql-ecosystem/SKILL.md) — query and index design once you're past the PDO/ORM boundary.
