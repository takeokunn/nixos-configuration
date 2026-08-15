---
name: effect-ts
description: Use when writing or reviewing Effect (Effect-TS) code — Effect.Service definitions, Layer composition, Effect.scoped resource handling, converting try/catch/async to the Effect error channel, Schema-as-SSOT type derivation, or testing with @effect/vitest and TestClock.
version: 3.0.0
---

Design principles and reference patterns for Effect (Effect-TS): service definitions, disciplined Layer
composition, escaping the Effect runtime as late as possible, converting imperative code to the error
channel, deriving types from Schema, and testing with @effect/vitest. Each rule leads with the mechanism
that makes it necessary, then the fix.

This skill covers only the Effect-specific surface built on top of TypeScript — base compiler configuration,
generics, and utility types belong to [typescript-ecosystem](../typescript-ecosystem/SKILL.md).

## Version

Verified against Effect 3.19.x, @effect/vitest 0.25.x, vitest 3.2.x. Effect.Service, Layer.provide /
provideMerge / merge / mergeAll, Effect.scoped, TestClock, and Schema.Struct are stable across the 3.x line.
Import everything from the single `"effect"` package (`import { Effect, Layer, Schema, Ref } from "effect"`);
test bindings come from `"@effect/vitest"`. Effect 2.x and pre-3.x tutorials predate Effect.Service and the
current Schema location (Schema moved into the core `effect` package) — do not mix guidance across major
versions. Verify version-specific claims against current docs via [context7-usage](../context7-usage/SKILL.md)
before asserting them.

Two terms recur below: the requirement channel `R` in `Effect<A, E, R>` lists what a program still needs to
run — a program is runnable only when `R` is `never`, and Layers are what discharge it. A Schema is the single
source of truth for a value type: the static type is derived from it, so validation and typing cannot drift
apart.

## Service definition

Effect.Service is sugar over a tag plus a default Layer: it generates the tag from the identifier string,
builds the implementation from a constructor (`effect` / `scoped` / `sync` / `succeed`), and exposes a
`.Default` Layer. Reserve manual `Context.Tag` + `Layer.effect` for services where a default implementation
must not be assumed.

```ts
import { Effect } from "effect"

// The class value doubles as the tag; the string is the stable identifier.
class Database extends Effect.Service<Database>()("app/Database", {
  // `effect` runs when the layer is built and returns the implementation.
  effect: Effect.gen(function* () {
    const config = yield* Config
    return {
      query: (sql: string) => Effect.succeed(`result of ${sql} @ ${config.url}`),
    } as const
  }),
  dependencies: [Config.Default], // deps needed to BUILD this service
}) {}

// Generated layers:
//   Database.Default                    -> Layer<Database>                   (deps baked in)
//   Database.DefaultWithoutDependencies  -> Layer<Database, never, Config>    (deps external)
```

Consumers write `const db = yield* Database` regardless of how the service was defined — migrating between
Context.Tag and Effect.Service does not change call sites. Use a stable, namespaced identifier string
(`"app/Database"`); it participates in tag identity.

For services that acquire resources needing release (connections, listeners, timers), use `scoped` and tie
acquisition to the Scope with `Effect.acquireRelease` — this is the general replacement for manual
add/removeEventListener or setInterval/clearInterval pairs, guaranteeing cleanup symmetric with construction:

```ts
class EventBus extends Effect.Service<EventBus>()("app/EventBus", {
  scoped: Effect.gen(function* () {
    const handler = yield* makeHandler
    yield* Effect.acquireRelease(
      Effect.sync(() => source.subscribe(handler)),
      () => Effect.sync(() => source.unsubscribe(handler)),
    )
    return { publish: (e: Event) => Effect.sync(() => source.emit(e)) } as const
  }),
}) {}
```

Use `Context.Tag` instead when the caller must supply the implementation and no default should be assumed
(library-facing or inherently contextual services):

```ts
import { Effect, Context, Layer } from "effect"

class Clock extends Context.Tag("app/Clock")<
  Clock,
  { readonly now: Effect.Effect<number> }
>() {}

const ClockLive = Layer.effect(
  Clock,
  Effect.sync(() => ({ now: Effect.sync(() => performance.now()) })),
)
```

**The four constructor keys differ in what they accept and when it runs**, not just in spelling: `succeed`
takes the finished implementation as a plain object; `sync` takes a thunk; `effect` takes an Effect; `scoped`
takes an Effect requiring a Scope. The last three run when the layer is built, so anything constructed per
service instance must live in one of them.

`succeed` does not call what you hand it — passing a factory function stores the function itself rather than
its result, so per-instance state is never created, and if that state was hoisted into a module-level closure
to make the types line up, every consumer silently shares one copy:

```ts
// WRONG: the factory is stored, not invoked.
// succeed: () => ({ counter: makeCounter() })

// RIGHT: the thunk runs when the layer is built, once per built instance.
class Counter extends Effect.Service<Counter>()("app/Counter", {
  sync: () => ({ next: makeCounter() }),
}) {}
```

For synchronous stateful construction use `sync: () => impl` or `effect: Effect.sync(() => impl)`. Reserve
`succeed` for a literal needing no construction step at all.

## Layer composition

Layers form a dependency graph. Sequence dependent layers with `provide` / `provideMerge`; reserve flat merge
for genuinely independent layers.

- **`Layer.provide`**: `inner.pipe(Layer.provide(outer))` builds `inner` using `outer`, and CONSUMES `outer` —
  it does not appear in the resulting output type. Use when the dependency is an implementation detail the
  rest of the app should not see.
- **`Layer.provideMerge`**: like `provide`, but also keeps the provided service in the output. Use when a
  lower-level service must remain visible to the final program in addition to satisfying a higher one.
- **`Layer.merge` / `Layer.mergeAll`**: combines layers side by side. The output is the union of outputs AND
  the union of requirements — no wiring happens. `mergeAll(a, b, c)` is the n-ary form.

```ts
// Layer<Database, never, never> — Config is satisfied and hidden
const DatabaseWired = Database.DefaultWithoutDependencies.pipe(Layer.provide(Config.Default))

// Layer<Database | Config> — Config both feeds Database and stays available
const Wired = Database.DefaultWithoutDependencies.pipe(Layer.provideMerge(Config.Default))

// Layer<Metrics | Tracer, never, ...both requirements unioned...>
const Observability = Layer.merge(Metrics.Default, Tracer.Default)
```

**Why flat merge leaks**: when layer B depends on layer A, `Layer.mergeAll(A, B)` does NOT feed A into B. It
merely places both in one layer whose requirement set is the union, so A's service leaks upward as an
unsatisfied requirement of the combined layer. The leak surfaces later — typically as an assignability error
at the top-level `Effect.provide`, far from the merge that caused it. Sequencing with provide/provideMerge
discharges the dependency at the point of composition, where the type is still local and legible. If B needs
A, wire them (`B.pipe(Layer.provide(A))`); use merge/mergeAll only for layers with no dependency relationship.

**Scoped placement**: keep `Effect.scoped` OUTSIDE the layer-provision pipeline and apply `Effect.provide`
before scoping. Wrapping a partially-provided program in `Effect.scoped` too early can narrow the inferred
requirement environment and produce spurious `Effect<..., R>` assignability errors even though every
requirement is eventually satisfied.

```ts
// Provide first, scope the fully-provided program last.
const runnable = program.pipe(Effect.provide(MainLayer), Effect.scoped)
```

**Memoization is per provision graph, not per layer value**: if the same layer is provided at two points —
once inside a sub-composition and again at the top level — it is BUILT TWICE, producing two distinct service
instances that both satisfy the same tag. Invisible for a pure, stateless service; severe for one that owns a
singleton — two sockets open, two caches diverging, two subscriptions delivering every event twice. Provide a
shared layer at exactly one point in the composition; if an inner composition needs it, leave it as an unmet
requirement there and discharge it once at the outer provide.

**`Effect.provide` is not free per iteration**: it is not a type-level annotation, it builds the layer graph.
Applying it inside a per-iteration handler (a frame callback, a per-message handler, a per-request path)
reconstructs every layer in the graph on every iteration — the same mistake as a hot-path `runPromise`, wearing
different clothes. Resolve services ONCE at the program edge and forward the resolved values into the handler
as an explicit record, so the handler's type is `Effect<void, E, never>` and needs no provide of its own:

```ts
const main = Effect.gen(function* () {
  const services = { renderer: yield* Renderer, clock: yield* Clock } as const
  const onFrame = makeFrameHandler(services) // Effect<void, never, never>
  yield* driveLoop(onFrame)
}).pipe(Effect.provide(MainLayer))
```

## Escape the Effect runtime late

Each `Effect.runPromise` / `Effect.runSync` crosses the boundary out of Effect: it forfeits typed errors,
interruption, structured concurrency, and scheduling. Entering that boundary once per hot iteration (render
frame, stream item, request) multiplies the cost and fragments error handling into many independent `.catch()`
sites.

```ts
// BAD: N runtime entries per tick, N separate error paths, no interruption.
const tick = () => {
  Effect.runPromise(stepA()).catch(console.error)
  Effect.runPromise(stepB()).catch(console.error)
  Effect.runPromise(stepC()).catch(console.error)
  scheduleNext(tick)
}
```

Bridge the external callback into Effect ONCE by enqueuing, then process on a single forked fiber that stays
inside Effect:

```ts
import { Effect, Queue } from "effect"

const makeLoop = Effect.gen(function* () {
  const commands = yield* Queue.unbounded<number>()

  // Single processing fiber: one runtime, unified error channel.
  yield* Effect.forever(
    Effect.gen(function* () {
      const ts = yield* Queue.take(commands)
      yield* stepA(ts)
      yield* stepB(ts)
      yield* stepC(ts)
    }),
  ).pipe(Effect.forkScoped)

  // The ONLY escape: hand each external event to the queue.
  const onTick = (ts: number) => Effect.runFork(Queue.offer(commands, ts))
  return { onTick } as const
})
```

Prefer `Effect.runFork` over `Effect.runPromise` at the boundary when firing-and-forgetting; it returns a
Fiber you can interrupt and creates no unhandled-rejection surface. Attach
`Effect.catchAllCause(cause => Effect.logError(...))` to the forked program so failures land in Effect's
logger instead of a bare `.catch(console.error)`. One processing fiber gives a single pause/resume/interrupt
control point the multi-escape version cannot.

**Enumerate/resolve/replay** is the inbound mirror of the queue bridge: a pure algorithm takes a SYNCHRONOUS
predicate, but the data it must consult has become asynchronous. Running the effect synchronously inside the
callback, or forking the algorithm into an async copy, trades correctness or duplication for convenience. A
third shape avoids both:

1. Run the existing algorithm with a predicate that always answers "no", using it purely as an ENUMERATOR of
   the candidates it would have consulted, in order.
2. Resolve those candidates through the async source in that order, caching each one exactly once and
   short-circuiting at the first hit.
3. Re-run the same algorithm against the now-populated synchronous cache to recover the canonical result and
   its metadata.

The algorithm is never copied, so its semantics — ordering, first-hit short-circuit, single-read-per-candidate
— are preserved by construction rather than by review. The cost is one extra traversal over an in-memory cache,
almost always cheaper than the I/O the pattern exists to batch. Applies to any pure search/scan whose data
source has moved behind an Effect: route lookup, permission resolution, dependency walks.

## Description vs. execution

An Effect is a description. Everything evaluated OUTSIDE the description — in the builder function, before
`Effect.gen`, in a default argument — runs once, at construction time, and its value is frozen into every
subsequent execution. Everything inside the description re-runs per execution. This is the Effect analogue of
the classic closure-capture bug, and it is easy to write accidentally the moment a handler is refactored into
a "build once, invoke later" shape.

```ts
// BAD: `dirty` is read once, when flush is constructed.
const makeFlush = Effect.gen(function* () {
  const dirty = yield* Ref.get(dirtyRef)          // ← evaluated at build time
  return Effect.if(dirty, { onTrue: () => save, onFalse: () => Effect.void })
})

// GOOD: the read is part of the description, so it re-runs on every execution.
const flush = Effect.gen(function* () {
  const dirty = yield* Ref.get(dirtyRef)
  if (dirty) yield* save
})
```

Symptom shape: the effect behaves correctly the first time and then appears "stuck" on a stale decision, with
no error anywhere. Test-honesty corollary: when an effect is fired at a lifecycle edge the test cannot outlive
(page unload, shutdown signal), assert that it was FORKED, not that its work completed — asserting completion
there either passes vacuously or forces production code to block a teardown path it must not block.

## Long-running fibers

A supervisory loop — a repeating daemon, a frame processor, a consumer fiber — is only as durable as its error
channel and only as responsive as its wake-up condition. The failure modes below are all silent: the loop
stops, stalls, or burns a core, and nothing is logged because the mechanism that would have logged it is the
thing that died.

**Repeated effects must be total.** `Effect.repeat` and `Effect.forever` terminate on the FIRST failure — the
schedule governs recurrence, not recovery. A daemon that repeats an effect which can fail stops permanently the
first time a transient error occurs, typically hours after start-up, with no trace. Type the repeated body as
`Effect<A, never>`: handle and log its failures inside the body so the daemon has nothing left to propagate.
Reserve the error channel for conditions that genuinely should stop the loop.

**`catchAll` does not catch defects.** It handles typed failures only. A defect — an unexpected exception
thrown inside a `sync`/`try` body, a bug rather than a modelled error — passes straight through it and kills
the fiber. A loop guarded only by `catchAll` dies silently on exactly the class of problem the guard was meant
to survive. Guard long-running loops with `Effect.catchAllCause`, and log the Cause; use `catchAll` only where
recovering a specific modelled failure, not where keeping a fiber alive.

**Backpressure choice is a queue choice.** A bounded queue applies backpressure by SUSPENDING the producer's
offer. Behind a real-time producer that cannot be slowed — a frame callback, an event listener, an inbound
socket — each suspended offer is a fiber that accumulates, so the memory the bound was meant to cap is
consumed by blocked fibers instead of queued items; under sustained overload the process degrades rather than
sheds. Choose the queue by what should happen when the consumer falls behind: `Queue.bounded` when the
producer can legitimately be slowed, `Queue.dropping`/`Queue.sliding` when it cannot and stale items are worth
less than liveness, `Queue.unbounded` only when the producer is provably finite.

A `start()` that forks a daemon and then calls `Fiber.join` on it never returns — the daemon runs forever by
design, so the caller blocks forever. In tests this surfaces as a mass timeout rather than a failure at the
offending line. Fork and return immediately, handing back the Fiber (or a `Scope`-bound handle) so callers can
interrupt it; joining is for fibers expected to complete.

Repeating an effect that waits on a LEVEL-triggered condition busy-spins: once the condition becomes true it
stays true until something external resets it, so every iteration returns immediately and the loop consumes a
core doing nothing. Wait on an edge rather than a level — take from a Queue, await a Deferred, or
clear/consume the condition as part of the same iteration that observes it — so the loop blocks until there is
genuinely new work.

## Concurrency

Widening `Effect.forEach` / `Effect.all` from sequential to concurrent looks like a one-word change
(`{ concurrency: n }`) and is in fact a semantics change. Four conditions decide whether it is
behavior-preserving:

1. Items are independent and their effects are read-only or touch disjoint state — safe on this axis.
2. Any item writes back into a target shared with other items — NOT safe; keep sequential, or collect
   concurrently and fold sequentially.
3. A downstream consumer depends on result order — only safe if ordering is restored explicitly; concurrent
   completion order is not input order.
4. The real bottleneck downstream (worker pool, connection pool, rate limit) is narrower than the chosen
   concurrency — widening buys nothing; raise the ceiling that actually binds, or match the concurrency to it.

The standard repair when an accumulator is involved: run the concurrent work as a pure map producing per-item
results, then fold those results into the shared accumulator on a single fiber after the batch completes.
Mutating a shared tracker from inside concurrent fibers races and drops updates.

```ts
const results = yield* Effect.forEach(items, computeOne, { concurrency: 4 })
// Fold happens on one fiber, after the batch — no shared mutable target during the batch.
yield* Ref.update(totals, current => results.reduce(applyOne, current))
```

Pick the concurrency number from the real downstream capacity (pool size, permitted request rate), not a round
number — an arbitrary bump adds scheduling overhead and hides the ceiling that actually limits throughput.
`concurrency: "unbounded"` on an input whose size comes from user data is a resource-exhaustion hazard; bound
it.

## Stateful services

A service that owns mutable state owns its invariants too. `Ref` gives atomicity for a single cell and a
single operation — everything beyond that (a decision spanning read and write, an invariant spanning two
cells, a restore that must not land half-applied) has to be arranged deliberately; the Ref API does not hint
at it.

**Fold the decision into the update.** `Ref.get`, then a decision, then `Ref.set` is three steps, and another
fiber can interleave between any two of them — the classic time-of-check-to-time-of-use race. Folding the
decision into `Ref.update` / `Ref.modify` makes the whole transition one atomic step, because the callback
runs inside the update.

```ts
// RACY: eviction decided on a value that may already be stale by the time we set.
// const cache = yield* Ref.get(cacheRef)
// yield* Ref.set(cacheRef, insertWithEviction(cache, key, value))

// ATOMIC: decision and write are one transition.
yield* Ref.update(cacheRef, cache => insertWithEviction(cache, key, value))

// Ref.modify when the transition must also RETURN something (e.g. the evicted entry).
const evicted = yield* Ref.modify(cacheRef, cache => {
  const [next, dropped] = insertWithEviction(cache, key, value)
  return [dropped, next] as const   // [returned value, new state]
})
```

Keep I/O OUT of the modify callback: compute the next state and RETURN the work to be performed, then perform
it after the update completes — a side effect inside the critical section is what makes people abandon
atomicity in the first place.

**Invariants across two Refs need explicit serialization.** `Ref.modify` is atomic per Ref. The moment a
single logical transition touches two Refs — a public queue and its private sidecar, a balance and its ledger
— the pair is no longer atomic, and interleaving fibers can observe or produce a state that satisfies neither
Ref's invariant on its own. Serialize the paired update under one `Effect.Semaphore` (a one-permit mutex) and
make the paired section `Effect.uninterruptible`, so no fiber can observe the intermediate state and
interruption cannot leave the pair half-updated. If the pairing is permanent, prefer collapsing the two Refs
into one Ref holding a single record.

**Restore is all-or-nothing.** Rehydrating a service from a persisted snapshot is a validation step and a
state transition; running them interleaved is how a corrupt snapshot leaves the service in a
partially-restored state worse than either the old state or a clean failure. Validate the exact persisted
shape and reconstruct every owned value FIRST, then commit with a single Ref update. An invalid snapshot must
leave the existing state completely unchanged.

**Copy on accept.** A value handed in from outside the service — a candidate from a caller, a payload from a
host — remains reachable by that caller. Storing it by reference means later mutation outside the service
silently rewrites state the service believes it owns. Copy accepted values on the way in; reject invalid
candidates without disturbing current state.

**Widen the error channel rather than defect.** Converting a storage read failure or a decode failure into a
defect (a thrown exception, `Effect.die`, a `decodeSync` at a boundary) removes it from the type and from
every caller's ability to recover — these are expected conditions at an I/O boundary, not bugs. Widen the
service's `E` (and `R`, when the recovery needs a capability) to name the failure, so callers see it and
choose. Reserve defects for genuine invariant violations no caller could sensibly handle.

**Attempt the fallible operation before flipping state.** When a state transition is paired with a resource
transfer, ORDER decides what a partial failure costs. Perform the fallible transfer first and commit the state
change only on success: the failure mode becomes "the transition did not happen, retry is available" instead
of "the state changed and the resource is gone." Applied symmetrically — acquire before entering, return
before leaving — every failure becomes a no-op, and snapshot-and-rollback machinery turns out unnecessary.

```ts
// Return the resource FIRST; flip the state only if the return succeeded.
yield* returnResource(item)                 // fallible: may reject (full, closed, …)
yield* Ref.set(engagedRef, false)           // reached only on success
```

Rollback is the fallback for transitions whose steps genuinely cannot be ordered — try ordering first.

These are the Effect-shaped expressions of general state-ownership rules. For the framework-neutral versions —
who owns a piece of state, request/response correlation, idempotency keys, durability ordering, and
three-state reads — see [state-transactions](../state-transactions/SKILL.md) rather than restating them here.

## Hot-path allocation

Every combinator that takes a callback allocates a closure per invocation — irrelevant almost everywhere,
measurable in a 60 Hz loop or a per-request path under load. Apply the rewrites below only where a profile or
an explicit frame/latency budget justifies it, never as house style:

| From | To | Why |
|---|---|---|
| `Effect.map(() => CONSTANT)` | `Effect.as(CONSTANT)` | Drops the inline callback allocation for a result that does not depend on the input |
| `Effect.gen` wrapping a single downstream step | Direct composition: `flatMap`/`map`/`as`/`tap` | A generator wrapper allocates an iterator and a frame to express one step |
| `Effect.option(e)` consumed immediately as present/absent | Direct null bailout or `catchAll` returning null | Allocates an Option destructured on the next line |
| Chained `Ref.update` callbacks for a trivial set | One `Ref.get`, compute locally, one `Ref.set` | Each update callback allocates; a single-writer path does not need the atomicity |
| `Array.from(...)`/map/filter chains over known-length input | A pre-sized array with an indexed loop | Iterator helpers allocate intermediates per stage |

The `Ref.update` → get/compute/set substitution trades atomicity for allocation and is valid ONLY where a
single writer is guaranteed — where concurrent writers exist, "fold the decision into the update" wins
outright, correctness before allocation. Measure before and after on the actual budget being defended; an
allocation rewrite that does not move the number is a readability regression with no payoff. Do not apply
these to cold paths — start-up, configuration, error handling — where `Effect.gen` is more readable and that
matters more.

## Imperative-to-Effect correspondence

| Imperative | Effect equivalent |
|---|---|
| `try/catch` around sync code | `Effect.try({ try, catch })` — models the failure as a typed error `E` instead of swallowing it |
| `try/catch` around a Promise / async fn | `Effect.tryPromise({ try, catch })` |
| `throw new DomainError()` | `Effect.fail(new DomainError())` with a `Data.TaggedError` subclass |
| `setInterval` / `clearInterval` | `Effect.repeat(effect, Schedule.spaced(...))` on a forked fiber; interrupt to stop |
| `setTimeout(fn, ms)` | `Effect.delay(effect, Duration)` or `Effect.sleep` |
| `addEventListener`/`removeEventListener` pair | `Effect.acquireRelease` inside a scoped layer |
| `let mutable = ...; mutable = next` | `Ref.make` + `Ref.set`/`Ref.update` for shared state that outlives a single expression |
| `console.log`/`warn`/`error` | `Effect.log`/`logWarning`/`logError` (structured, testable) |
| `new Promise` + resolve/reject callback API | `Effect.async(register)` to lift a callback source into an interruptible Effect |
| `throw` to abort a batch | `Effect.fail` short-circuits `Effect.gen`; recover with `catchTag`/`catchAll` |

An `async` function passed to `Effect.tryPromise` is already correctly wrapped — rewriting its internal await
chain into Promise combinators is a style change, not a correctness fix; leave it unless there is a reason.
Not every callback must be lifted: event-emitter integrations with third-party libraries whose API is
fundamentally callback-based are legitimate boundaries, so wrap the surface in an Effect interface rather than
abstracting the whole library. Local loop counters and accumulators inside a single function are fine as plain
`let`; reserve `Ref` for state shared across effects or that must survive between fiber steps.

## Schema as SSOT

Define data with Schema and DERIVE the static type from it, so a value type and its validator can never
diverge. This is the default for domain/application value types.

```ts
import { Schema } from "effect"

export const PositionSchema = Schema.Struct({
  x: Schema.Number,
  y: Schema.Number,
  z: Schema.Number,
})
export type Position = Schema.Schema.Type<typeof PositionSchema>

// Branded identifiers via Schema (nominal typing over primitives):
export const EntityIdSchema = Schema.String.pipe(Schema.brand("EntityId"))
export type EntityId = Schema.Schema.Type<typeof EntityIdSchema>

// Tagged unions:
export const CommandSchema = Schema.TaggedStruct("Tick", { at: Schema.Number })
```

Universal pattern: `export const XSchema = Schema.Struct({...})` then
`export type X = Schema.Schema.Type<typeof XSchema>` — never hand-write a parallel interface. Decode at
boundaries with `Schema.decodeUnknown` (Effect-returning) or `decodeUnknownSync`; keep structural validation in
the schema and push value-range clamping into an explicit follow-up step when bounds are business rules rather
than shape. For JS `Date` instances use `Schema.DateFromSelf`; for ISO-string dates use `Schema.Date` —
choosing the wrong one silently changes the decoded representation.

Two Schema misuses do not announce themselves: one type-checks into nonsense, the other moves a validation
failure out of the error channel and into a defect.

`Schema.filter` in Effect 3.x is **curried** — it takes the predicate and returns a transformation to be
piped. Calling it in the uncurried, "obvious" shape (`Schema.filter(schema, predicate, options)`) passes the
SCHEMA where the predicate is expected. It can typecheck and produce a refinement meaning something entirely
different from what was written.

```ts
// WRONG: schema is being treated as the predicate.
// const Positive = Schema.filter(Schema.Number, (n) => n > 0)

// RIGHT: pipe the schema through the curried filter.
const Positive = Schema.Number.pipe(
  Schema.filter((n) => n > 0, { message: () => "must be positive" }),
)
```

`Schema.decodeUnknownSync` (and `decodeSync`) THROW a `ParseError` on invalid input. Inside an `Effect.gen`
body that throw is a defect, not a typed failure: it bypasses `catchAll`, is absent from the effect's `E`, and
can kill a supervising fiber. This bites hardest when a later step was meant to sanitize the value — clamping,
normalization, and range repair all run AFTER decode, so a non-finite or out-of-range input never reaches
them. Use the Effect-returning `Schema.decodeUnknown` at boundaries so the parse failure lands in the typed
error channel; keep `decodeUnknownSync` for construction-time literals and test fixtures, where a throw is the
intended outcome.

Types that must NOT be forced into Schema — converting them costs more than it returns or is impossible:

- **Hot-path binary buffers**: TypedArrays (Uint8Array, Float32Array, …) and structs on a per-iteration hot
  path — Schema decode overhead is unacceptable there.
- **Opaque external class instances**: physics bodies, GPU handles, DB driver objects, browser objects like
  IndexedDB — not plain value types.
- **Mutable internal machinery**: structures whose identity is a mutable Map/Set/LRU cache with a dirty flag —
  Schema models values, not mutable containers.
- **Service contracts**: capability interfaces a Layer produces — these are behavior, not data.
- **Pure helpers**: pure functions need neither Effect wrapping nor Schema.

## Testing

Test Effect code with @effect/vitest, composing dependencies as Layers and injecting mocks as Layers. Prefer
the Effect-native `it.effect` family so the test body is itself an Effect and the test environment (including
a controllable clock) is provided automatically.

```ts
import { describe, it } from "@effect/vitest"
import { expect } from "vitest"
import { Effect } from "effect"

describe("Database", () => {
  // it.effect: body returns an Effect; TestContext (incl. TestClock) auto-provided.
  it.effect("returns a result", () =>
    Effect.gen(function* () {
      const db = yield* Database
      const out = yield* db.query("SELECT 1")
      expect(out).toContain("SELECT 1")
    }).pipe(Effect.provide(TestLayer)),
  )
})
```

`describe`/`it` come from `"@effect/vitest"`; `expect` still comes from `"vitest"`. `it.effect` provides the
Test services (TestClock, TestRandom, …) automatically; use `it.scoped` when the body opens a Scope, and
`it.live` when deliberately wanting the real clock/services.

Assemble the unit-under-test with its real dependency layers once and reuse across cases:

```ts
const TestLayer = Database.DefaultWithoutDependencies.pipe(Layer.provide(Config.Default))
```

Replace a dependency with a fixed implementation using `Layer.succeed` keyed by its tag:

```ts
const NoiseMock = Layer.succeed(NoiseSource, {
  sample: (_x: number) => 0.5,
  reseed: (_seed: number) => Effect.void,
})

const program = subjectUnderTest.pipe(Effect.provide(
  Subject.DefaultWithoutDependencies.pipe(Layer.provide(NoiseMock)),
))
```

For an Effect.Service you can alternatively construct a mock instance (`new Subject({ ...methods })`) and
inject it with `Effect.provideService`. Keep mocks synchronous where possible (`Effect.sync`, `Effect.void`,
`Effect.succeed`) so tests stay deterministic.

Drive throttles, schedules, and delays with TestClock instead of real waiting:

```ts
import { Effect, TestClock } from "effect"

it.effect("fires after the interval", () =>
  Effect.gen(function* () {
    const before = yield* pollOutput
    yield* TestClock.adjust("60 minutes") // advance virtual time
    const after = yield* pollOutput
    expect(before).not.toEqual(after)
  }),
)
```

`TestClock.adjust` advances the virtual clock and runs any effects scheduled within that window — recurring or
delayed effects become synchronous and deterministic. To flush forked handlers inside a test, prefer an
Effect-based pause such as `Effect.yieldNow()` over a raw Promise sleep; wall-clock sleeps are both flaky and,
in strict Effect codebases, flagged as domain-layer violations.

## Related

- [typescript-ecosystem](../typescript-ecosystem/SKILL.md) — base TypeScript compiler config, generics, and
  utility types that Effect builds on
- [testing-patterns](../testing-patterns/SKILL.md) — general test strategy that @effect/vitest patterns plug
  into
- [context7-usage](../context7-usage/SKILL.md) — verify current Effect / @effect/vitest APIs and
  version-specific behavior
- [investigation-patterns](../investigation-patterns/SKILL.md) — evidence-based tracing of requirement leaks
  and runtime-boundary issues
- [state-transactions](../state-transactions/SKILL.md) — framework-neutral rules for state ownership,
  atomicity, request/response correlation, idempotency, durability ordering, schema evolution, and
  three-state reads; this skill covers only the Effect-shaped expression of them
- [trust-boundaries](../trust-boundaries/SKILL.md) — discipline for untrusted input crossing into a service;
  Schema decoding here is the mechanism, not the policy
- [test-integrity](../test-integrity/SKILL.md) — whether a test genuinely exercises the code it claims to;
  pairs with the fork-not-completion honesty rule above
