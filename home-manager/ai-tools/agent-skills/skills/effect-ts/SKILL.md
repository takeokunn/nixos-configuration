---
name: effect-ts
description: This skill should be used when the user works with Effect (Effect-TS) — writing or reviewing Effect.Service definitions, Layer composition (Layer.provide/provideMerge/mergeAll), Effect.scoped resource handling, converting imperative try/catch/async code to the Effect error channel, deriving types from Schema (Schema-as-SSOT), or testing Effect code with @effect/vitest, TestClock, and Layer-based mocks. Provides generalized design principles and minimal reference patterns for Effect 3.x.
version: 2.0.0
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
</escape_effect_late>

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

<error_escalation inherits="core-patterns#error_escalation">
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
</constraints>

<related_skills>
  <skill name="typescript-ecosystem">Underlying TypeScript config, generics, and utility types Effect builds on</skill>
  <skill name="testing-patterns">General test strategy that @effect/vitest patterns plug into</skill>
  <skill name="context7-usage">Verify current Effect / @effect/vitest APIs and version behavior</skill>
  <skill name="investigation-patterns">Evidence-based tracing of requirement leaks and runtime-boundary issues</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate service, layer, and schema declarations across the codebase</agent>
  <agent name="quality-assurance">Review Effect composition and error-channel discipline</agent>
  <agent name="test">Design it.effect + TestClock coverage for converted logic</agent>
</related_agents>
