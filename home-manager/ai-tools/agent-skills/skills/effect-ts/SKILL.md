---
name: effect-ts
description: This skill should be used when the user works with Effect (Effect-TS) — writing or reviewing Effect.Service definitions, Layer composition (Layer.provide/provideMerge/mergeAll), Effect.scoped resource handling, converting imperative try/catch/async code to the Effect error channel, deriving types from Schema (Schema-as-SSOT), or testing Effect code with @effect/vitest, TestClock, and Layer-based mocks. Also covers Layer memoization being per provision graph and the cost of Effect.provide inside an iteration, defects versus typed errors (catchAll does not catch a defect), atomic Ref updates that fold the decision into the update instead of get-decide-set, Queue backpressure choices, fiber lifecycle traps (a start that joins its own fiber, repeat over a level-triggered condition), choosing a safe concurrency width, and Schema traps such as the curried filter and decodeSync throwing before sanitization. Generalized design principles and minimal reference patterns for Effect 3.x.
version: 2.3.0
---

<purpose>
  Provide generalized design principles and minimal reference patterns for building
  applications with Effect (Effect-TS): unifying service definitions, disciplined Layer
  composition, escaping the Effect runtime as late as possible, converting imperative code
  to the Effect error channel, deriving types from Schema, and testing with @effect/vitest.
  The focus is on the "why" of each rule (how dependency leaks arise, why hot-loop escapes
  hurt) followed by the smallest code that demonstrates the fix.
</purpose>

<scope>
  <focus>Effect (Effect-TS) design principles and reference patterns: Effect.Service definitions, Layer composition, scoped resources, Schema-as-SSOT, error-channel conversion, and @effect/vitest testing</focus>
  <defer_to skill="typescript-ecosystem">
    Base TypeScript compiler configuration, generics, utility types, module resolution, and general tsconfig/tooling. This skill covers only the Effect-specific surface built on top of TypeScript.
  </defer_to>
  <unique_coverage>
    Effect.Service and layer wiring (Layer.provide/provideMerge/mergeAll), Effect.scoped resource lifecycles, escaping the Effect runtime late, converting imperative try/catch/async to the typed error channel, deriving types from Schema, and it.effect/it.scoped/TestClock test patterns.
    Also: layer instance identity across provision graphs, long-running fiber and error-channel discipline, Ref atomicity and multi-Ref serialization, Effect.forEach concurrency widening, and hot-path allocation inside Effect.
  </unique_coverage>
</scope>

<version_info>
  <current_version>Effect 3.x (verified against effect 3.19.x)</current_version>
  <companions>
    <item>@effect/vitest 0.25.x — Effect-native test bindings (it.effect / it.scoped / it.live)</item>
    <item>vitest 3.2.x — underlying runner (expect still imported from vitest)</item>
  </companions>
  <api_stability>
    <item>Effect.Service, Layer.provide/provideMerge/merge/mergeAll, Effect.scoped, TestClock, and Schema.Struct are stable across the Effect 3.x line. Patterns below were confirmed against the official Effect documentation.</item>
    <item>Import everything from the single "effect" package (`import { Effect, Layer, Schema, Ref } from "effect"`); test bindings come from "@effect/vitest".</item>
  </api_stability>
  <caution>
    <item>Effect 2.x and pre-3.x tutorials predate Effect.Service and the current Schema module location (Schema moved into the core "effect" package). Do not mix guidance across major versions.</item>
  </caution>
</version_info>

<tools>
  <tool>Read - Inspect service, layer, schema, and test files</tool>
  <tool>Edit - Apply targeted conversions and refactors</tool>
  <tool>Bash - Run type checks and the vitest suite</tool>
  <tool>mcp__plugin_claude-code-home-manager_context7__query-docs - Verify current Effect / @effect/vitest APIs before asserting version-specific behavior</tool>
</tools>

<concepts>
  <concept name="service_as_capability">A service is an interface of capabilities addressed by a tag; a Layer builds the implementation and declares what it needs to be built.</concept>
  <concept name="requirement_channel">Every Effect carries a third type parameter R (`Effect&lt;A, E, R&gt;`) listing unmet requirements. A program is only runnable when R is `never`; Layers are what discharge R.</concept>
  <concept name="escape_late">The Effect runtime should be entered once at the program edge. `runPromise`/`runSync` scattered through hot paths discard the error channel, interruption, and scheduling that Effect exists to provide.</concept>
  <concept name="schema_ssot">A Schema is the single source of truth: the static type is derived from it, so validation and typing never drift apart.</concept>
</concepts>

<service_definition>
  <description>
    Prefer one uniform way to declare services. Effect.Service is syntactic sugar over a
    tag plus a default Layer: it generates the tag from the identifier string, builds the
    implementation from a constructor (effect / scoped / sync / succeed), and exposes a
    `.Default` Layer. Reserve the manual Context.GenericTag + Layer.effect form for cases
    where a default implementation must not be assumed.
  </description>

  <pattern name="effect_service_unified">
    <when_to_use>Application-level services that have a sensible runtime implementation</when_to_use>
    <example>
      import { Effect } from "effect"

      // The class value doubles as the tag; the string is the stable identifier.
      class Database extends Effect.Service&lt;Database&gt;()("app/Database", {
        // `effect` runs when the layer is built and returns the implementation.
        effect: Effect.gen(function* () {
          const config = yield* Config
          return {
            query: (sql: string) =&gt; Effect.succeed(`result of ${sql} @ ${config.url}`),
          } as const
        }),
        // Dependencies needed to BUILD this service.
        dependencies: [Config.Default],
      }) {}

      // Generated layers:
      //   Database.Default                 -> Layer&lt;Database&gt;            (deps baked in)
      //   Database.DefaultWithoutDependencies -> Layer&lt;Database, never, Config&gt; (deps external)
    </example>
    <notes>
      <item>Consumers write `const db = yield* Database` regardless of how the service was defined — migrating from Context.GenericTag to Effect.Service does not change call sites.</item>
      <item>Use a stable, namespaced identifier string (e.g. "app/Database"). It participates in tag identity.</item>
    </notes>
  </pattern>

  <pattern name="scoped_service">
    <when_to_use>Services that acquire resources (connections, listeners, timers) needing release</when_to_use>
    <example>
      class EventBus extends Effect.Service&lt;EventBus&gt;()("app/EventBus", {
        // `scoped` ties acquisition to a Scope; release runs on scope close.
        scoped: Effect.gen(function* () {
          const handler = yield* makeHandler
          yield* Effect.acquireRelease(
            Effect.sync(() =&gt; source.subscribe(handler)),
            () =&gt; Effect.sync(() =&gt; source.unsubscribe(handler)),
          )
          return { publish: (e: Event) =&gt; Effect.sync(() =&gt; source.emit(e)) } as const
        }),
      }) {}
    </example>
    <notes>
      <item>`Effect.acquireRelease` inside a `scoped` constructor guarantees cleanup symmetric with construction — the general replacement for manual add/removeEventListener or setInterval/clearInterval pairs.</item>
    </notes>
  </pattern>

  <pattern name="context_tag_when_no_default">
    <when_to_use>Library-facing or inherently contextual services where the caller must supply the implementation</when_to_use>
    <example>
      import { Effect, Context, Layer } from "effect"

      class Clock extends Context.Tag("app/Clock")&lt;
        Clock,
        { readonly now: Effect.Effect&lt;number&gt; }
      &gt;() {}

      const ClockLive = Layer.effect(
        Clock,
        Effect.sync(() =&gt; ({ now: Effect.sync(() =&gt; performance.now()) })),
      )
    </example>
    <notes>
      <item>Context.Tag makes the default implementation optional; Effect.Service requires one. Choose Context.Tag only when "no assumed implementation" is the point.</item>
    </notes>
  </pattern>

  <constructor_input_shapes>
    <description>
      The four constructor keys are not interchangeable spellings of the same thing — they
      differ in WHAT they accept and WHEN it runs. `succeed` takes the finished implementation
      as a plain object; `sync` takes a thunk; `effect` takes an Effect; `scoped` takes an
      Effect that requires a Scope. The last three run when the layer is built, so anything
      that must be constructed per service instance has to live in one of them.
    </description>

    <trap name="succeed_with_a_factory">
      <problem>
        `succeed` does not call what you hand it. Passing a factory function stores the
        function itself rather than its result, so per-instance state is never created — and
        when the state was hoisted into a module-level closure to make the types line up, every
        consumer of the service silently shares one copy of it.
      </problem>
      <rule>For synchronous stateful construction use `sync: () =&gt; impl` or `effect: Effect.sync(() =&gt; impl)`. Reserve `succeed` for a literal that needs no construction step at all.</rule>
      <example>
        // WRONG: the factory is stored, not invoked; nothing is constructed per instance.
        // succeed: () =&gt; ({ counter: makeCounter() })

        // RIGHT: the thunk runs when the layer is built, once per built instance.
        class Counter extends Effect.Service&lt;Counter&gt;()("app/Counter", {
          sync: () =&gt; ({ next: makeCounter() }),
        }) {}
      </example>
    </trap>
  </constructor_input_shapes>

  <decision_tree name="service_style">
    <question>Does this service have one sensible runtime implementation owned by the app?</question>
    <if_yes>Use Effect.Service with effect/scoped constructor and `dependencies`.</if_yes>
    <if_no>Use Context.Tag + a separately provided Layer so callers choose the implementation.</if_no>
  </decision_tree>
</service_definition>

<layer_composition>
  <description>
    Layers form a dependency graph. The discipline that avoids type errors and leaked
    requirements is: sequence dependent layers with provide / provideMerge, and reserve flat
    merge for genuinely independent layers.
  </description>

  <operator name="Layer.provide">
    <semantics>`inner.pipe(Layer.provide(outer))` builds `inner` using `outer`, and CONSUMES `outer` — it does not appear in the resulting output type. Use when the dependency is an implementation detail the rest of the app should not see.</semantics>
    <example>
      // Layer&lt;Database, never, never&gt;  (Config is satisfied and hidden)
      const DatabaseWired = Database.DefaultWithoutDependencies.pipe(
        Layer.provide(Config.Default),
      )
    </example>
  </operator>

  <operator name="Layer.provideMerge">
    <semantics>Like provide, but ALSO keeps the provided service in the output. Use when a lower-level service must remain visible to the final program in addition to satisfying a higher one.</semantics>
    <example>
      // Layer&lt;Database | Config&gt;  (Config both feeds Database and stays available)
      const Wired = Database.DefaultWithoutDependencies.pipe(
        Layer.provideMerge(Config.Default),
      )
    </example>
  </operator>

  <operator name="Layer.merge / Layer.mergeAll">
    <semantics>Combine layers side by side. The output is the union of outputs AND the union of requirements — no wiring happens. `mergeAll(a, b, c)` is the n-ary form.</semantics>
    <example>
      // Layer&lt;Metrics | Tracer, never, ...both requirements unioned...&gt;
      const Observability = Layer.merge(Metrics.Default, Tracer.Default)
    </example>
  </operator>

  <principle name="why_flat_merge_leaks">
    <explanation>
      When layer B depends on layer A, `Layer.mergeAll(A, B)` does NOT feed A into B. It
      merely places both in one layer whose requirement set is the union, so A's service
      leaks upward as an unsatisfied requirement of the combined layer. The leak surfaces
      later — typically as an assignability error at the top-level `Effect.provide`, far from
      the merge that caused it. Sequencing with provide/provideMerge discharges the
      dependency at the point of composition, where the type is still local and legible.
    </explanation>
    <rule>If B needs A, wire them (`B.pipe(Layer.provide(A))`). Use merge/mergeAll only for layers with no dependency relationship.</rule>
  </principle>

  <principle name="scoped_placement">
    <explanation>
      Keep `Effect.scoped` OUTSIDE the layer-provision pipeline and apply `Effect.provide`
      before scoping. Wrapping a partially-provided program in `Effect.scoped` too early can
      narrow the inferred requirement environment and produce spurious `Effect&lt;..., R&gt;`
      assignability errors even though every requirement is eventually satisfied.
    </explanation>
    <example>
      // Prefer: provide first, scope the fully-provided program last.
      const runnable = program.pipe(Effect.provide(MainLayer), Effect.scoped)
    </example>
  </principle>

  <principle name="memoization_is_per_provision_graph">
    <explanation>
      Layer memoization is scoped to a single provision graph, not to the layer value. If the
      same layer is provided at two points — once inside a sub-composition and again at the
      top level — it is BUILT TWICE, and the two builds produce two distinct service
      instances. Nothing in the types says so, because both instances satisfy the same tag.
      The duplication is invisible for a pure, stateless service and severe for one that owns
      a singleton: two sockets open, two DOM subtrees mounted, two caches diverging, two
      subscriptions delivering every event twice.
    </explanation>
    <rule>Provide a shared layer at exactly ONE point in the composition. If an inner composition needs it, leave it as an unmet requirement there and discharge it once at the outer provide, rather than provisioning it defensively at both levels.</rule>
  </principle>

  <principle name="provide_is_not_free_per_iteration">
    <explanation>
      `Effect.provide` is not a type-level annotation — it builds the layer graph. Applying it
      inside a per-iteration handler (a frame callback, a per-message handler, a per-request
      path) reconstructs every layer in the graph on every iteration. This is the same mistake
      as a hot-path `runPromise`, wearing different clothes: the escape is cheap-looking and
      the cost scales with graph size times iteration rate.
    </explanation>
    <rule>Resolve services ONCE at the program edge and forward the resolved values into the handler as an explicit record, so the handler's type is `Effect&lt;void, E, never&gt;` and needs no provide of its own.</rule>
    <example>
      // Resolve once; the handler receives concrete services, not a requirement.
      const main = Effect.gen(function* () {
        const services = { renderer: yield* Renderer, clock: yield* Clock } as const
        const onFrame = makeFrameHandler(services) // Effect&lt;void, never, never&gt;
        yield* driveLoop(onFrame)
      }).pipe(Effect.provide(MainLayer))
    </example>
  </principle>

  <decision_tree name="composition_operator">
    <question>Does one layer depend on another?</question>
    <branch condition="Dependency, and the provider should stay hidden">Layer.provide</branch>
    <branch condition="Dependency, and the provider must remain in the final environment">Layer.provideMerge</branch>
    <branch condition="No dependency relationship (independent services)">Layer.merge / Layer.mergeAll</branch>
  </decision_tree>
</layer_composition>

<escape_effect_late>
  <description>
    "Escape the Effect runtime as late as possible." Each `Effect.runPromise` / `Effect.runSync`
    crosses the boundary out of Effect: it forfeits typed errors, interruption, structured
    concurrency, and scheduling. Entering that boundary once per hot iteration (render frame,
    stream item, request) multiplies the cost and fragments error handling into many
    independent `.catch()` sites.
  </description>

  <anti_pattern name="runtime_escape_in_hot_loop">
    <problem>A per-iteration callback fires many independent `Effect.runPromise` calls, each with its own ad-hoc `.catch`.</problem>
    <example>
      // BAD: N runtime entries per tick, N separate error paths, no interruption.
      const tick = () =&gt; {
        Effect.runPromise(stepA()).catch(console.error)
        Effect.runPromise(stepB()).catch(console.error)
        Effect.runPromise(stepC()).catch(console.error)
        scheduleNext(tick)
      }
    </example>
  </anti_pattern>

  <pattern name="queue_plus_fiber_bridge">
    <description>Bridge the external callback into Effect ONCE by enqueuing, then process on a single forked fiber that stays inside Effect.</description>
    <example>
      import { Effect, Queue } from "effect"

      const makeLoop = Effect.gen(function* () {
        const commands = yield* Queue.unbounded&lt;number&gt;()

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
        const onTick = (ts: number) =&gt; Effect.runFork(Queue.offer(commands, ts))
        return { onTick } as const
      })
    </example>
    <notes>
      <item>Prefer `Effect.runFork` over `Effect.runPromise` at the boundary when you are firing-and-forgetting; it returns a Fiber you can interrupt and does not create an unhandled-rejection surface.</item>
      <item>Attach `Effect.catchAllCause(cause =&gt; Effect.logError(...))` to the forked program so failures land in Effect's logger instead of a bare `.catch(console.error)`.</item>
      <item>One processing fiber gives a single pause/resume/interrupt control point that the multi-escape version cannot.</item>
    </notes>
  </pattern>

  <pattern name="enumerate_resolve_replay">
    <description>
      The inbound mirror of the queue bridge: a pure algorithm takes a SYNCHRONOUS predicate,
      but the data it must consult has become asynchronous. The two instinctive answers — run
      the effect synchronously inside the callback, or fork the algorithm into an async copy —
      trade correctness or duplication for convenience. A third shape avoids both.
    </description>
    <steps>
      <step order="1">Run the existing algorithm with a predicate that always answers "no", using it purely as an ENUMERATOR of the candidates it would have consulted, in order.</step>
      <step order="2">Resolve those candidates through the async source in that order, caching each one exactly once and short-circuiting at the first hit.</step>
      <step order="3">Re-run the same algorithm against the now-populated synchronous cache to recover the canonical result and its metadata.</step>
    </steps>
    <notes>
      <item>The algorithm is never copied, so its semantics — ordering, first-hit short-circuit, single-read-per-candidate — are preserved by construction rather than by review.</item>
      <item>The cost is one extra traversal over an in-memory cache, which is almost always cheaper than the I/O the pattern exists to batch.</item>
      <item>Applies to any pure search/scan whose data source has moved behind an Effect: route lookup, permission resolution, dependency walks.</item>
    </notes>
  </pattern>
</escape_effect_late>

<description_vs_execution>
  <description>
    An Effect is a description. Everything evaluated OUTSIDE the description — in the builder
    function, before `Effect.gen`, in a default argument — runs once, at construction time,
    and its value is frozen into every subsequent execution. Everything inside the description
    re-runs per execution. Mixing the two up is the Effect analogue of the classic
    closure-capture bug, and it is easy to write accidentally the moment a handler is
    refactored into a "build once, invoke later" shape.
  </description>

  <anti_pattern name="reading_mutable_state_at_construction">
    <problem>A factory reads a Ref (or any mutable cell) while BUILDING the Effect, so the returned Effect carries the value that cell held at startup no matter when it is later run.</problem>
    <example>
      // BAD: `dirty` is read once, when flush is constructed.
      const makeFlush = Effect.gen(function* () {
        const dirty = yield* Ref.get(dirtyRef)          // ← evaluated at build time
        return Effect.if(dirty, { onTrue: () =&gt; save, onFalse: () =&gt; Effect.void })
      })

      // GOOD: the read is part of the description, so it re-runs on every execution.
      const flush = Effect.gen(function* () {
        const dirty = yield* Ref.get(dirtyRef)
        if (dirty) yield* save
      })
    </example>
    <notes>
      <item>Symptom shape: the effect behaves correctly the first time and then appears "stuck" on a stale decision, with no error anywhere.</item>
      <item>Test honesty rule for the same code: when an effect is fired at a lifecycle edge that the test cannot outlive (page unload, shutdown signal), assert that it was FORKED, not that its work completed. Asserting completion there either passes vacuously or forces the production code to block a teardown path it must not block.</item>
    </notes>
  </anti_pattern>
</description_vs_execution>

<long_running_fibers>
  <description>
    A supervisory loop — a repeating daemon, a frame processor, a consumer fiber — is only as
    durable as its error channel and only as responsive as its wake-up condition. The failure
    modes below are all silent: the loop stops, stalls, or burns a core, and nothing is logged
    because the mechanism that would have logged it is the thing that died.
  </description>

  <principle name="repeated_effects_must_be_total">
    <explanation>
      `Effect.repeat` and `Effect.forever` terminate on the FIRST failure — the schedule
      governs recurrence, not recovery. A daemon that repeats an effect which can fail
      therefore stops permanently the first time a transient error occurs, typically hours
      after start-up and with no trace.
    </explanation>
    <rule>Type the repeated body as `Effect&lt;A, never&gt;`: handle and log its failures inside the body so the daemon has nothing left to propagate. Reserve the error channel for conditions that genuinely should stop the loop.</rule>
  </principle>

  <principle name="catch_all_does_not_catch_defects">
    <explanation>
      `Effect.catchAll` handles typed failures only. A defect — an unexpected exception thrown
      inside a `sync`/`try` body, a bug rather than a modelled error — passes straight through
      it and kills the fiber. A loop guarded only by `catchAll` therefore dies silently on
      exactly the class of problem the guard was meant to survive.
    </explanation>
    <rule>Guard long-running loops with `Effect.catchAllCause`, and log the Cause. Use `catchAll` where you are recovering a specific modelled failure, not where you are keeping a fiber alive.</rule>
  </principle>

  <principle name="backpressure_choice_is_a_queue_choice">
    <explanation>
      A bounded queue applies backpressure by SUSPENDING the producer's offer. Behind a
      real-time producer that cannot be slowed — a frame callback, an event listener, an
      inbound socket — each suspended offer is a fiber that accumulates, so the memory the
      bound was meant to cap is consumed by blocked fibers instead of queued items. Under
      sustained overload the process degrades rather than sheds.
    </explanation>
    <rule>Choose the queue by what should happen when the consumer falls behind: `Queue.bounded` when the producer can legitimately be slowed, `Queue.dropping`/`Queue.sliding` when it cannot and stale items are worth less than liveness. `Queue.unbounded` only when the producer is provably finite.</rule>
  </principle>

  <anti_pattern name="start_that_joins_its_own_fiber">
    <problem>A `start()` that forks a daemon and then calls `Fiber.join` on it never returns — the daemon runs forever by design, so the caller blocks forever. In tests this surfaces as a mass timeout rather than a failure at the offending line.</problem>
    <instead>Fork and return immediately, handing back the Fiber (or a `Scope`-bound handle) so callers can interrupt it. Joining is for fibers that are expected to complete.</instead>
  </anti_pattern>

  <anti_pattern name="repeat_over_a_level_triggered_condition">
    <problem>Repeating an effect that waits on a LEVEL-triggered condition busy-spins: once the condition becomes true it stays true until something external resets it, so every iteration returns immediately and the loop consumes a core doing nothing.</problem>
    <instead>Wait on an edge rather than a level — take from a Queue, await a Deferred, or clear/consume the condition as part of the same iteration that observes it — so the loop blocks until there is genuinely new work.</instead>
  </anti_pattern>
</long_running_fibers>

<concurrency>
  <description>
    Widening `Effect.forEach` / `Effect.all` from sequential to concurrent looks like a
    one-word change (`{ concurrency: n }`) and is in fact a semantics change. Four conditions
    have to hold before it is behavior-preserving; the fourth is the one that decides whether
    the change buys anything at all.
  </description>

  <decision_tree name="safe_to_widen_concurrency">
    <question>Can this forEach/all be widened without changing behavior?</question>
    <branch condition="Items are independent and their effects are read-only or touch disjoint state">Safe on this axis</branch>
    <branch condition="Any item writes back into a target shared with other items">NOT safe — keep sequential, or collect concurrently and fold sequentially</branch>
    <branch condition="A downstream consumer depends on result order">Only safe if ordering is restored explicitly; concurrent completion order is not input order</branch>
    <branch condition="The real bottleneck downstream (worker pool, connection pool, rate limit) is narrower than the chosen concurrency">Widening buys nothing — raise the ceiling that actually binds, or match the concurrency to it</branch>
  </decision_tree>

  <pattern name="collect_concurrently_fold_sequentially">
    <description>The standard repair when an accumulator is involved: run the concurrent work as a pure map producing per-item results, then fold those results into the shared accumulator on a single fiber after the batch completes. Mutating a shared tracker from inside concurrent fibers races and drops updates.</description>
    <example>
      const results = yield* Effect.forEach(items, computeOne, { concurrency: 4 })
      // Fold happens on one fiber, after the batch — no shared mutable target during the batch.
      yield* Ref.update(totals, current =&gt; results.reduce(applyOne, current))
    </example>
  </pattern>

  <notes>
    <item>Pick the concurrency number from the real downstream capacity (pool size, permitted request rate), not from a round number. An arbitrary bump adds scheduling overhead and hides the ceiling that actually limits throughput.</item>
    <item>`concurrency: "unbounded"` on an input whose size comes from user data is a resource-exhaustion hazard; bound it.</item>
  </notes>
</concurrency>

<stateful_services>
  <description>
    A service that owns mutable state owns its invariants too. `Ref` gives atomicity for a
    single cell and a single operation — everything beyond that (a decision spanning read and
    write, an invariant spanning two cells, a restore that must not land half-applied) has to
    be arranged deliberately, and the Ref API does not hint at it.
  </description>

  <principle name="fold_the_decision_into_the_update">
    <explanation>
      `Ref.get`, then a decision, then `Ref.set` is three steps, and another fiber can
      interleave between any two of them — the classic time-of-check-to-time-of-use race.
      Folding the decision into `Ref.update` / `Ref.modify` makes the whole transition one
      atomic step, because the callback runs inside the update.
    </explanation>
    <example>
      // RACY: eviction decided on a value that may already be stale by the time we set.
      // const cache = yield* Ref.get(cacheRef)
      // yield* Ref.set(cacheRef, insertWithEviction(cache, key, value))

      // ATOMIC: decision and write are one transition.
      yield* Ref.update(cacheRef, cache =&gt; insertWithEviction(cache, key, value))

      // Ref.modify when the transition must also RETURN something (e.g. the evicted entry).
      const evicted = yield* Ref.modify(cacheRef, cache =&gt; {
        const [next, dropped] = insertWithEviction(cache, key, value)
        return [dropped, next] as const   // [returned value, new state]
      })
    </example>
    <rule>Keep I/O OUT of the modify callback. Compute the next state and RETURN the work to be performed, then perform it after the update completes — a side effect inside the critical section is what makes people abandon atomicity in the first place.</rule>
  </principle>

  <principle name="invariants_across_two_refs_need_explicit_serialization">
    <explanation>
      `Ref.modify` is atomic per Ref. The moment a single logical transition has to touch two
      Refs — a public queue and its private sidecar, a balance and its ledger — the pair is no
      longer atomic, and interleaving fibers can observe or produce a state that satisfies
      neither Ref's invariant on its own.
    </explanation>
    <rule>Serialize the paired update under one `Effect.Semaphore` (a one-permit mutex) and make the paired section `Effect.uninterruptible`, so no fiber can observe the intermediate state and interruption cannot leave the pair half-updated. If the pairing is permanent, prefer collapsing the two Refs into one Ref holding a single record.</rule>
  </principle>

  <principle name="restore_is_all_or_nothing">
    <explanation>
      Rehydrating a service from a persisted snapshot is a validation step and a state
      transition, and running them interleaved is how a corrupt snapshot leaves the service in
      a partially-restored state that is worse than either the old state or a clean failure.
    </explanation>
    <rule>Validate the exact persisted shape and reconstruct every owned value FIRST, then commit with a single Ref update. An invalid snapshot must leave the existing state completely unchanged.</rule>
  </principle>

  <principle name="copy_on_accept">
    <explanation>
      A value handed in from outside the service — a candidate from a caller, a payload from a
      host — remains reachable by that caller. Storing it by reference means later mutation
      outside the service silently rewrites state the service believes it owns.
    </explanation>
    <rule>Copy accepted values on the way in; reject invalid candidates without disturbing current state. Effect makes the copy cheap to justify because the alternative failure is untraceable.</rule>
  </principle>

  <principle name="widen_the_error_channel_rather_than_defect">
    <explanation>
      Converting a storage read failure or a decode failure into a defect (a thrown exception,
      `Effect.die`, a `decodeSync` at a boundary) removes it from the type and from every
      caller's ability to recover. These are expected conditions at an I/O boundary, not bugs.
    </explanation>
    <rule>Widen the service's `E` (and `R`, when the recovery needs a capability) to name the failure, so callers see it and choose. Reserve defects for genuine invariant violations that no caller could sensibly handle.</rule>
  </principle>

  <principle name="attempt_the_fallible_operation_before_flipping_state">
    <explanation>
      When a state transition is paired with a resource transfer, ORDER decides what a partial
      failure costs. Perform the fallible transfer first and commit the state change only on
      success: the failure mode becomes "the transition did not happen, retry is available"
      instead of "the state changed and the resource is gone." Applied symmetrically — acquire
      before entering, return before leaving — every failure becomes a no-op, and the
      snapshot-and-rollback machinery people reach for first turns out to be unnecessary.
    </explanation>
    <example>
      // Return the resource FIRST; flip the state only if the return succeeded.
      yield* returnResource(item)                 // fallible: may reject (full, closed, …)
      yield* Ref.set(engagedRef, false)           // reached only on success
    </example>
    <rule>Rollback is the fallback for transitions whose steps genuinely cannot be ordered. Try ordering first.</rule>
  </principle>

  <notes>
    <item>These are the Effect-shaped expressions of general state-ownership rules. For the framework-neutral versions — who owns a piece of state, request/response correlation, idempotency keys, durability ordering, and three-state reads — see the `state-transactions` skill rather than restating them here.</item>
  </notes>
</stateful_services>

<hot_path_allocation>
  <description>
    Every combinator that takes a callback allocates a closure per invocation. That is
    irrelevant almost everywhere and measurable in a 60 Hz loop or a per-request path under
    load. The rewrites below are mechanical and behavior-preserving, which is what makes them
    safe to apply — and the discipline that keeps this from becoming premature optimization is
    that they are applied only where a profile or an explicit frame/latency budget justifies
    it, never as a house style.
  </description>

  <substitutions>
    <row from="Effect.map(() =&gt; CONSTANT)" to="Effect.as(CONSTANT)" why="Drops the inline callback allocation for a result that does not depend on the input" />
    <row from="Effect.gen wrapping a single downstream step" to="Direct composition: Effect.flatMap / map / as / tap" why="A generator wrapper allocates an iterator and a frame to express one step" />
    <row from="Effect.option(e) consumed immediately as present/absent" to="Direct null bailout or catchAll returning null" why="Allocates an Option that is destructured on the next line" />
    <row from="Chained Ref.update callbacks for a trivial set" to="One Ref.get, compute locally, one Ref.set" why="Each update callback allocates; a single-writer path does not need the atomicity" />
    <row from="Array.from(...) / map / filter chains over a known-length input" to="A pre-sized array with an indexed loop" why="Iterator helpers allocate intermediates per stage" />
  </substitutions>

  <caveats>
    <item>The `Ref.update` → get/compute/set substitution trades atomicity for allocation and is valid ONLY where a single writer is guaranteed. Where concurrent writers exist, `fold_the_decision_into_the_update` wins outright — correctness before allocation.</item>
    <item>Measure before and after on the actual budget being defended. An allocation rewrite that does not move the number is a readability regression with no payoff.</item>
    <item>Do not apply these to cold paths — start-up, configuration, error handling. `Effect.gen` is more readable and that matters more where it runs once.</item>
  </caveats>
</hot_path_allocation>

<imperative_to_effect>
  <description>Correspondence table for replacing imperative constructs with Effect equivalents that preserve the typed error channel and interruption.</description>
  <mapping>
    <row imperative="try/catch around sync code" effect="Effect.try({ try, catch }) — models the failure as a typed error E instead of swallowing it" />
    <row imperative="try/catch around a Promise / async fn" effect="Effect.tryPromise({ try, catch })" />
    <row imperative="throw new DomainError()" effect="Effect.fail(new DomainError()) with a Data.TaggedError subclass" />
    <row imperative="setInterval / clearInterval" effect="Effect.repeat(effect, Schedule.spaced(...)) on a forked fiber; interrupt to stop" />
    <row imperative="setTimeout(fn, ms)" effect="Effect.delay(effect, Duration) or Effect.sleep" />
    <row imperative="addEventListener / removeEventListener pair" effect="Effect.acquireRelease inside a scoped layer" />
    <row imperative="let mutable = ...; mutable = next" effect="Ref.make + Ref.set/Ref.update for shared state that outlives a single expression" />
    <row imperative="console.log / warn / error" effect="Effect.log / Effect.logWarning / Effect.logError (structured, testable)" />
    <row imperative="new Promise + resolve/reject callback API" effect="Effect.async(register) to lift a callback source into an interruptible Effect" />
    <row imperative="throw to abort a batch" effect="Effect.fail short-circuits the Effect.gen; recover with Effect.catchTag / catchAll" />
  </mapping>
  <caveats>
    <item>An `async` function passed to `Effect.tryPromise` is already correctly wrapped — rewriting its internal await chain into Promise combinators is a style change, not a correctness fix; leave it unless there is a reason.</item>
    <item>Not every callback must be lifted. Event-emitter integrations with third-party libraries whose API is fundamentally callback-based are legitimate boundaries; wrap the surface in an Effect interface rather than abstracting the whole library.</item>
    <item>Local loop counters and accumulators inside a single function are fine as plain `let`. Reserve `Ref` for state that is shared across effects or must survive between fiber steps.</item>
  </caveats>
</imperative_to_effect>

<schema_as_ssot>
  <description>
    Define data with Schema and DERIVE the static type from it, so a value type and its
    validator can never diverge. This is the default for domain/application value types.
  </description>

  <pattern name="derive_type_from_schema">
    <example>
      import { Schema } from "effect"

      export const PositionSchema = Schema.Struct({
        x: Schema.Number,
        y: Schema.Number,
        z: Schema.Number,
      })
      export type Position = Schema.Schema.Type&lt;typeof PositionSchema&gt;

      // Branded identifiers via Schema (nominal typing over primitives):
      export const EntityIdSchema = Schema.String.pipe(Schema.brand("EntityId"))
      export type EntityId = Schema.Schema.Type&lt;typeof EntityIdSchema&gt;

      // Tagged unions:
      export const CommandSchema = Schema.TaggedStruct("Tick", { at: Schema.Number })
    </example>
    <notes>
      <item>Universal pattern: `export const XSchema = Schema.Struct({...})` then `export type X = Schema.Schema.Type&lt;typeof XSchema&gt;`. Never hand-write a parallel interface.</item>
      <item>Decode at boundaries with `Schema.decodeUnknown` (Effect-returning) or `decodeUnknownSync`; keep structural validation in the schema and push value-range clamping into an explicit follow-up step when bounds are business rules rather than shape.</item>
      <item>For JS `Date` instances use `Schema.DateFromSelf`; for ISO-string dates use `Schema.Date`. Choosing the wrong one silently changes the decoded representation.</item>
    </notes>
  </pattern>

  <api_traps>
    <description>
      Two Schema misuses that do not announce themselves: one type-checks into nonsense, the
      other moves a validation failure out of the error channel and into a defect.
    </description>

    <trap name="schema_filter_is_curried">
      <problem>
        `Schema.filter` in Effect 3.x is curried — it takes the predicate and returns a
        transformation to be piped. Calling it in the uncurried, "obvious" shape
        (`Schema.filter(schema, predicate, options)`) passes the SCHEMA where the predicate is
        expected. It can typecheck and produce a refinement that means something entirely
        different from what was written.
      </problem>
      <example>
        // WRONG: schema is being treated as the predicate.
        // const Positive = Schema.filter(Schema.Number, (n) =&gt; n &gt; 0)

        // RIGHT: pipe the schema through the curried filter.
        const Positive = Schema.Number.pipe(
          Schema.filter((n) =&gt; n &gt; 0, { message: () =&gt; "must be positive" }),
        )
      </example>
    </trap>

    <trap name="decode_sync_throws_before_sanitizing">
      <problem>
        `Schema.decodeUnknownSync` (and `decodeSync`) THROW a `ParseError` on invalid input.
        Inside an `Effect.gen` body that throw is a defect, not a typed failure: it bypasses
        `catchAll`, is absent from the effect's `E`, and can kill a supervising fiber. This
        bites hardest when a later step was meant to sanitize the value — clamping,
        normalization, and range repair all run AFTER decode, so a non-finite or out-of-range
        input never reaches them.
      </problem>
      <rule>Use the Effect-returning `Schema.decodeUnknown` at boundaries so the parse failure lands in the typed error channel. Keep `decodeUnknownSync` for construction-time literals and test fixtures, where a throw is the intended outcome.</rule>
    </trap>
  </api_traps>

  <exclusions>
    <description>Types that must NOT be forced into Schema — converting them costs more than it returns or is impossible.</description>
    <exclude reason="Hot-path binary buffers">TypedArrays (Uint8Array, Float32Array, …) and structs that live on a per-iteration hot path — Schema decode overhead is unacceptable there.</exclude>
    <exclude reason="Opaque external class instances">Instances of third-party classes (physics bodies, GPU handles, DB driver objects, browser objects like IndexedDB) — they are not plain value types.</exclude>
    <exclude reason="Mutable internal machinery">Structures whose identity is a mutable Map/Set/LRU cache with a dirty flag — Schema models values, not mutable containers.</exclude>
    <exclude reason="Service contracts">Service capability interfaces (the object of methods a Layer produces) — these are behavior, not data.</exclude>
    <exclude reason="Pure helpers">Pure functions do not need Effect wrapping or Schema; keep them plain.</exclude>
  </exclusions>
</schema_as_ssot>

<testing>
  <description>
    Test Effect code with @effect/vitest, composing dependencies as Layers and injecting
    mocks as Layers. Prefer the Effect-native `it.effect` family so the test body is itself
    an Effect and the test environment (including a controllable clock) is provided
    automatically.
  </description>

  <pattern name="effect_native_tests">
    <example>
      import { describe, it } from "@effect/vitest"
      import { expect } from "vitest"
      import { Effect } from "effect"

      describe("Database", () =&gt; {
        // it.effect: body returns an Effect; TestContext (incl. TestClock) auto-provided.
        it.effect("returns a result", () =&gt;
          Effect.gen(function* () {
            const db = yield* Database
            const out = yield* db.query("SELECT 1")
            expect(out).toContain("SELECT 1")
          }).pipe(Effect.provide(TestLayer)),
        )
      })
    </example>
    <notes>
      <item>`describe`/`it` come from "@effect/vitest"; `expect` still comes from "vitest".</item>
      <item>`it.effect` provides the Test services (TestClock, TestRandom, …) automatically. Use `it.scoped` when the body opens a Scope, and `it.live` when you deliberately want the real clock/services.</item>
    </notes>
  </pattern>

  <pattern name="layer_composed_dependencies">
    <description>Assemble the unit-under-test with its real dependency layers once, reuse across cases.</description>
    <example>
      const TestLayer = Database.DefaultWithoutDependencies.pipe(
        Layer.provide(Config.Default),
      )
    </example>
  </pattern>

  <pattern name="mock_via_layer_succeed">
    <description>Replace a dependency with a fixed implementation using Layer.succeed keyed by its tag.</description>
    <example>
      const NoiseMock = Layer.succeed(NoiseSource, {
        sample: (_x: number) =&gt; 0.5,
        reseed: (_seed: number) =&gt; Effect.void,
      })

      const program = subjectUnderTest.pipe(Effect.provide(
        Subject.DefaultWithoutDependencies.pipe(Layer.provide(NoiseMock)),
      ))
    </example>
    <notes>
      <item>For an Effect.Service you can alternatively construct a mock instance (`new Subject({ ...methods })`) and inject it with `Effect.provideService`.</item>
      <item>Keep mocks synchronous where possible (`Effect.sync`, `Effect.void`, `Effect.succeed`) so tests stay deterministic.</item>
    </notes>
  </pattern>

  <pattern name="deterministic_time">
    <description>Drive throttles, schedules, and delays with TestClock instead of real waiting.</description>
    <example>
      import { Effect, TestClock } from "effect"

      it.effect("fires after the interval", () =&gt;
        Effect.gen(function* () {
          const before = yield* pollOutput
          yield* TestClock.adjust("60 minutes") // advance virtual time
          const after = yield* pollOutput
          expect(before).not.toEqual(after)
        }),
      )
    </example>
    <notes>
      <item>`TestClock.adjust` advances the virtual clock and runs any effects scheduled within that window — recurring/delayed effects become synchronous and deterministic.</item>
      <item>To flush forked handlers inside a test, prefer an Effect-based pause such as `Effect.yieldNow()` over a raw `Promise` sleep; wall-clock sleeps are both flaky and, in strict Effect codebases, flagged as domain-layer violations.</item>
    </notes>
  </pattern>
</testing>

<best_practices>
  <practice priority="critical">Sequence dependent layers with provide/provideMerge; never rely on mergeAll to wire a dependency.</practice>
  <practice priority="critical">Enter the Effect runtime once at the program edge; do not scatter runPromise/runSync through hot paths.</practice>
  <practice priority="high">Standardize on Effect.Service for app services with a real implementation; reserve Context.Tag for "no assumed implementation".</practice>
  <practice priority="high">Derive every value type from its Schema; do not hand-maintain a parallel interface.</practice>
  <practice priority="high">Model failures as typed errors (Effect.fail / Data.TaggedError), not thrown exceptions or silent catch blocks.</practice>
  <practice priority="high">Apply Effect.provide before Effect.scoped; keep scoping at the outermost layer of the pipeline.</practice>
  <practice priority="medium">Use Effect.acquireRelease inside scoped layers for every acquire/release pair (listeners, intervals, connections).</practice>
  <practice priority="medium">Test with it.effect and Layer-based mocks; use TestClock for anything time-dependent.</practice>
  <practice priority="medium">Route failures to Effect.logError via catchAllCause rather than bare .catch(console.error).</practice>
  <practice priority="critical">Guard long-running loops with catchAllCause and keep the repeated body total; catchAll alone lets a defect kill the fiber silently.</practice>
  <practice priority="high">Provide a shared layer at exactly one point in the composition — a second provide builds a second instance of the same service.</practice>
  <practice priority="high">Resolve services once at the edge and pass them into per-iteration handlers; Effect.provide rebuilds the graph every time it runs.</practice>
  <practice priority="high">Fold read-then-decide-then-write into Ref.update/Ref.modify, and keep I/O out of the modify callback.</practice>
  <practice priority="high">Attempt the fallible resource operation before committing the state change, so a failure is a no-op rather than a loss.</practice>
  <practice priority="medium">Check all four widening conditions before adding a concurrency option; match the number to the real downstream ceiling.</practice>
  <practice priority="medium">Read mutable state inside the Effect description, never while constructing it.</practice>
</best_practices>

<anti_patterns>
  <avoid name="mergeall_for_dependencies">
    <description>Using Layer.mergeAll to combine layers that depend on one another.</description>
    <instead>Wire with Layer.provide/provideMerge so the dependency is discharged at the composition site, not leaked to the top-level provide.</instead>
  </avoid>
  <avoid name="runtime_escape_per_iteration">
    <description>Calling Effect.runPromise/runSync inside a render frame, stream item, or request handler.</description>
    <instead>Bridge the external event into a Queue once and process on a single forked fiber.</instead>
  </avoid>
  <avoid name="silent_try_catch">
    <description>Raw try/catch whose catch block returns a default and discards the cause.</description>
    <instead>Effect.try with an explicit error constructor, so the failure is typed and observable.</instead>
  </avoid>
  <avoid name="parallel_interface_and_schema">
    <description>Declaring both a Schema and a separate interface for the same data.</description>
    <instead>Keep the Schema as SSOT and derive the type with Schema.Schema.Type.</instead>
  </avoid>
  <avoid name="schema_on_hot_or_opaque_types">
    <description>Forcing TypedArrays, external class instances, or mutable caches into Schema.</description>
    <instead>Leave them as native types; Schema is for plain value data only.</instead>
  </avoid>
  <avoid name="scoped_inside_provision_pipeline">
    <description>Wrapping a partially-provided program in Effect.scoped before provide.</description>
    <instead>Provide first, then scope the fully-provided program.</instead>
  </avoid>
  <avoid name="duplicate_provision_of_a_singleton_layer">
    <description>Providing the same layer both inside a sub-composition and at the top level.</description>
    <instead>Provide it once at the outer boundary; memoization does not span provision graphs, so the second provide builds a second instance.</instead>
  </avoid>
  <avoid name="provide_inside_the_iteration">
    <description>Calling Effect.provide inside a frame, message, or request handler.</description>
    <instead>Resolve the services once at the edge and forward them as a plain record.</instead>
  </avoid>
  <avoid name="get_decide_set_on_a_ref">
    <description>Ref.get, then a decision, then Ref.set — three interleavable steps.</description>
    <instead>Ref.update/Ref.modify with the decision folded into the callback; perform any I/O after the update returns.</instead>
  </avoid>
  <avoid name="succeed_with_a_factory_function">
    <description>Passing a factory to the Effect.Service `succeed` constructor.</description>
    <instead>Use `sync` (thunk) or `effect: Effect.sync(...)`; `succeed` stores what it is given rather than calling it.</instead>
  </avoid>
  <avoid name="decode_sync_at_a_boundary">
    <description>Schema.decodeUnknownSync on external input inside an Effect.gen body.</description>
    <instead>Schema.decodeUnknown, so a ParseError becomes a typed failure instead of a defect that escapes catchAll.</instead>
  </avoid>
  <avoid name="bounded_queue_behind_an_unslowable_producer">
    <description>Queue.bounded fed by a frame callback, listener, or socket that cannot be back-pressured.</description>
    <instead>Queue.dropping/sliding, so overload sheds items instead of accumulating suspended offer fibers.</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Wire dependent layers with provide/provideMerge; use merge/mergeAll only for independent layers.</rule>
  <rule>Cross the Effect runtime boundary once, at the edge; keep hot paths inside Effect.</rule>
  <rule>Import from the single "effect" package (and "@effect/vitest" for tests); do not mix pre-3.x module locations.</rule>
  <rule>Verify any version-specific API claim against current Effect docs before asserting it.</rule>
</rules>

<rules priority="standard">
  <rule>Use Effect.Service for app services with a default implementation; Context.Tag otherwise.</rule>
  <rule>Derive types from Schema; exclude hot-path/opaque/mutable types from Schema.</rule>
  <rule>Replace imperative try/catch/setInterval/addEventListener with their Effect equivalents.</rule>
  <rule>Test with it.effect + Layer mocks and drive time with TestClock.</rule>
  <rule>Keep a repeated/forever body total and guard it with catchAllCause; fork daemons and return, never join them.</rule>
  <rule>Make every state transition atomic at the Ref that owns it; serialize multi-Ref transitions under a semaphore and make them uninterruptible.</rule>
  <rule>Widen E (and R) to name I/O and decode failures instead of converting them into defects.</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Understand the service graph and where the Effect boundary sits</objective>
    <step order="1">
      <action>Identify services, their tags, and dependencies</action>
      <tool>Read / Grep for Effect.Service and Context.Tag declarations</tool>
      <output>Service dependency map</output>
    </step>
    <step order="2">
      <action>Locate every runPromise/runSync/runFork call and classify edge vs hot-path</action>
      <tool>Grep</tool>
      <output>Runtime-boundary inventory</output>
    </step>
  </phase>
  <phase name="implement">
    <objective>Apply the targeted conversion</objective>
    <step order="1">
      <action>Unify service definitions and wire layers with provide/provideMerge</action>
      <tool>Edit</tool>
      <output>Composed layer graph without leaked requirements</output>
    </step>
    <step order="2">
      <action>Collapse hot-path escapes into a queue + single fiber; convert imperative constructs</action>
      <tool>Edit</tool>
      <output>Effect-native control flow</output>
    </step>
  </phase>
  <phase name="validate">
    <objective>Confirm types and behavior</objective>
    <step order="1">
      <action>Run the type checker; a leaked requirement shows as an assignability error at the top-level provide</action>
      <tool>Bash (tsc --noEmit)</tool>
      <output>Zero requirement leaks</output>
    </step>
    <step order="2">
      <action>Run the vitest suite; add it.effect + TestClock coverage for converted time/schedule logic</action>
      <tool>Bash (vitest)</tool>
      <output>Green suite with deterministic time tests</output>
    </step>
  </phase>
</workflow>

<error_escalation>
  <examples>
    <example severity="low">Style-only difference (async closure inside a correctly-wrapped Effect.tryPromise)</example>
    <example severity="medium">Requirement leak from mergeAll surfacing as a top-level provide type error</example>
    <example severity="high">Hot-path runtime escapes dropping the error channel and interruption</example>
    <example severity="critical">Silent catch blocks hiding failures with no logging or typed error</example>
  </examples>
</error_escalation>

<constraints>
  <must>Wire dependent layers explicitly; keep Effect.scoped outermost</must>
  <must>Model errors in the typed channel, not via throw/silent catch</must>
  <must>Derive value types from Schema and honor the Schema exclusion rules</must>
  <avoid>runPromise/runSync in hot paths</avoid>
  <avoid>Layer.mergeAll for dependent layers</avoid>
  <avoid>Parallel Schema + interface declarations</avoid>
  <avoid>Effect.provide inside a per-iteration handler</avoid>
  <avoid>Read-then-write sequences on a Ref that another fiber can interleave</avoid>
  <avoid>Restating framework-neutral state, durability, or trust rules that other skills own</avoid>
</constraints>

<related_skills>
  <skill name="typescript-ecosystem">Underlying TypeScript config, generics, and utility types Effect builds on</skill>
  <skill name="testing-patterns">General test strategy that @effect/vitest patterns plug into</skill>
  <skill name="context7-usage">Verify current Effect / @effect/vitest APIs and version behavior</skill>
  <skill name="investigation-patterns">Evidence-based tracing of requirement leaks and runtime-boundary issues</skill>
  <skill name="state-transactions">Framework-neutral rules for state ownership, atomicity, request/response correlation, idempotency, durability ordering, schema evolution, and three-state reads — this skill covers only the Effect-shaped expression of them</skill>
  <skill name="trust-boundaries">Discipline for untrusted input crossing into a service; Schema decoding here is the mechanism, not the policy</skill>
  <skill name="test-integrity">Whether a test genuinely exercises the code it claims to; pairs with the fork-not-completion honesty rule</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate service, layer, and schema declarations across the codebase</agent>
  <agent name="quality-assurance">Review Effect composition and error-channel discipline</agent>
  <agent name="test">Design it.effect + TestClock coverage for converted logic</agent>
</related_agents>
