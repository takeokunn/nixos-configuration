---
name: state-transactions
description: This skill should be used when a mutation must cross a boundary to reach the component that owns the state, and the question is what makes the crossing safe — outbox and drain contracts, who may consume an emitted collection, atomicity and rollback above a store that offers neither, teardown where a cleanup failure must not replace the original error, idempotency keys and duplicate delivery, correlating an async response with its request, the commit point and acknowledged-versus-durable, coalescing write queues, migration defaults for absent versus invalid fields, present/absent/unknown reads, and clock-paced throttles, budgets, and accumulators. Keywords — outbox, drain, exactly-once, idempotency key, correlation ID, rollback, snapshot, ownership flag, commit point, durable, fsync, write queue, debounce, migration default, three-state read, fail closed, catch-up burst, clock regression, decrementing budget, accumulator.
version: 2.1.0
---

<purpose>
  Provide language- and framework-neutral principles for the moment a mutation leaves the
  code that requests it and arrives at the component that owns the state. That crossing is
  where ownership blurs, where "read then write" stops being atomic, where a retry becomes a
  duplicate, and where an acknowledgement gets mistaken for durability. The focus is the
  "why" behind each rule — how a snapshot silently discards a concurrent writer, why a
  cleanup failure erases the real error, how an absent value turns a cache permanently valid —
  followed by the smallest statement of the fix.
</purpose>

<scope>
  <focus>Safe mutation across an ownership boundary: outbox and handoff contracts, atomicity and rollback above a store that provides neither, request/response correlation, idempotency, commit ordering and durability, schema evolution of persisted payloads, partial-read semantics, and clock-driven cadence and accumulation.</focus>
  <defer_to skill="trust-boundaries">
    Validating untrusted input, decode budgets, TOCTOU windows, deriving authority from evidence rather than accepting a requested effect, validator domains, and non-finite guards. This skill assumes the mutation is already authorized and asks only what makes its application correct.
  </defer_to>
  <defer_to skill="sql-ecosystem">
    Engine-level ACID, isolation levels, and DDL migration mechanics. This skill covers achieving atomicity and durability ABOVE a store that does not offer them.
  </defer_to>
  <defer_to skill="effect-ts">
    Effect-specific mechanics for the same problems — Ref, Semaphore, Queue, Layer lifecycles, and @effect/vitest. The principles here are the framework-neutral statement; that skill is the idiomatic encoding.
  </defer_to>
  <defer_to skill="testing-patterns">
    General test design. Only the assertions that are specific to these invariants (identity across rollback, exactly-once commit, non-modification of real user state) are named here.
  </defer_to>
  <unique_coverage>
    Producer/consumer drain contracts, the audit-record vs work-queue distinction, atomicity of read-then-clear, rollback that preserves reference identity and the primary error, ownership-flagged teardown, allocation-identity correlation, payload-fingerprinted idempotency, content-first/pointer-last commit, single-writer coalescing writes, absent-vs-invalid migration defaults, three-state reads, catch-up-free time cadence, and the general rule that a bounded wait uses a decrementing budget rather than an absolute deadline.
  </unique_coverage>
</scope>

<concepts>
  <concept name="owner">The single component whose internal representation the state actually is. Only the owner can offer an operation that is atomic with respect to that state; everything else is a caller making a request.</concept>
  <concept name="boundary_crossing">Any point where a mutation is described in one component and applied in another — a queue, a message, a remote call, a callback, a persisted record. Every crossing introduces the possibility of duplication, loss, reordering, and partial application.</concept>
  <concept name="atomic_operation">An operation that either fully happens or does not happen, as observed by every other reader of the same state. Composing two atomic operations does NOT produce an atomic operation.</concept>
  <concept name="commit_point">The single irreversible step that makes a change visible. Everything before it must be undoable; nothing after it may be reported as a failure of the change itself.</concept>
  <concept name="acknowledged_vs_durable">"The owner accepted it" and "it survives a crash" are different guarantees. Conflating them is how accepted work disappears.</concept>
</concepts>

<ownership>
  <description>
    Most cross-boundary bugs are ownership bugs wearing a different costume. Before asking
    whether an operation is atomic, ask who owns the state it touches, and whether the
    operation being used was designed by that owner for this purpose.
  </description>

  <principle name="producer_never_drains_its_own_outbox">
    <why>
      When a component appends events for a consumer and then also consumes them itself, three
      things break at once. The published contract changes silently, because existing consumers
      that used to observe those events now see an empty collection. Observability is lost, since
      nothing external can tell what was produced. And delivery becomes unsafe: if the producer
      drains and then fails while materializing the effect, the record is already gone and no
      retry is possible.
    </why>
    <implication>
      A producer appends. Exactly one consumer drains, atomically, and defines its own retry and
      failure behavior. Repeated drains are empty by construction. If the producer needs to know
      the outcome, that is a separate response channel, not a self-drain.
    </implication>
  </principle>

  <principle name="an_emitted_collection_must_state_its_consumer_obligation">
    <why>
      "Things that happened" and "things you must do" have identical shapes — a list of records
      appended during processing — and opposite consumer obligations. Nothing in the type
      distinguishes an audit record already applied by the owner from deferred work the consumer
      must still perform. A consumer that guesses wrong applies the effect a second time.
    </why>
    <implication>
      Every emitted collection declares, in its contract and preferably in its name, whether
      draining it is required, optional, or forbidden. When the owner has already applied the
      effect, say so explicitly: consumers may read it for display or telemetry and must not
      re-apply it.
    </implication>
  </principle>

  <principle name="read_then_clear_is_not_a_transaction">
    <why>
      A snapshot call followed by a reset call is two atomic operations, not one atomic pair.
      Between them, another mutation can interleave, or the second call can fail. The observable
      results are duplication (the snapshot is used and the state is never cleared) or loss (the
      state is cleared and the snapshot is dropped). This is invisible in single-threaded testing
      and unfixable at the caller.
    </why>
    <implication>
      When the caller needs "take everything and leave it empty", the owner must expose a single
      drain operation that does both. Until it does, the higher layer must not synthesize the
      behavior; it must report the missing operation as a blocking gap.
    </implication>
  </principle>

  <principle name="add_the_narrow_operation_at_the_owner">
    <why>
      When the operation a caller needs is missing, the nearest available operation is almost
      always broader — a full restore, a full reset, a whole-record overwrite. Using it appears
      to work because the field of interest ends up correct, while neighbouring state the caller
      never considered is silently destroyed. A reset that also clears persisted, unrelated
      contents is the classic shape.
    </why>
    <implication>
      Approximating a missing narrow operation with a broad overwrite at the call site is not an
      acceptable substitute. Add the narrow operation where the state lives. Review any use of a
      broad restore/reset by enumerating everything it touches, not just the field you wanted.
    </implication>
  </principle>

  <principle name="metadata_belongs_to_the_value_not_to_a_position">
    <why>
      Keying per-item attributes by the item's current position — slot index, row number, tab
      order, line number — is a side table that is correct only while nothing moves. Ownership is
      violated the moment values move, swap, split, merge, are transferred into another
      container, or are restored from persistence. The failure mode is the worst kind: stale
      attributes silently reattach to an unrelated replacement value that happens to land in the
      same position.
    </why>
    <implication>
      Use one stored representation where the attributes travel with the value. An empty position
      carries no attributes. Splitting copies them; merging is permitted only when the value and
      all attributes match. When the model cannot be fixed yet, every transition must explicitly
      move or clear the side entry, destroying a container is a transfer rather than a deletion,
      and persistence must reject attributes whose backing value no longer exists.
    </implication>
  </principle>
</ownership>

<atomicity_and_rollback>
  <description>
    Rollback is the part of a transaction that is written last, tested least, and runs only in
    the situations nobody rehearsed. Each principle below is a way rollback fails while
    appearing to succeed.
  </description>

  <principle name="never_snapshot_and_restore_state_you_do_not_own">
    <why>
      Capture-once snapshotting assumes you are the only writer between capture and restore. If
      another component writes to the same shared slot during that window, restoring the snapshot
      discards its write — destructive loss of state you never knew about, attributed to nobody.
      The wider the shared namespace, the more likely a second writer exists.
    </why>
    <implication>
      Represent your state in a namespace you exclusively own rather than mutating and later
      restoring a shared one. Where a shared slot is unavoidable, record an ownership flag per
      resource: repeated setup keeps ownership, resources that were already present stay
      non-owned, and teardown removes only what you actually installed. Reference-counted
      installation is the same rule for a slot shared by many independent activations.
    </implication>
  </principle>

  <principle name="restore_recovers_values_not_identity">
    <why>
      Rolling back by replacing state with copies restores the values but not the object
      identities. Any caller holding a reference obtained before the transaction still points at
      the mutated original, so mutations made during the failed transaction remain observable
      through those aliases after the rollback "succeeded". Mutable containers, tables, and
      collections are exactly the things callers hold references to.
    </why>
    <implication>
      Either roll back by mutating the original objects in place, or guarantee no external
      references exist. Tests must retain an external alias across the rollback and assert
      identity, not merely equality — an equality-only assertion passes in precisely the broken
      case.
    </implication>
  </principle>

  <principle name="a_rollback_failure_must_not_replace_the_original_condition">
    <why>
      A naive unwind block lets a failure raised during cleanup propagate in place of the failure
      that triggered the cleanup. The caller then sees a confusing secondary error, the real
      cause is gone, and any cleanup steps after the failing one are skipped. Diagnosis of the
      original problem becomes impossible from the outside.
    </why>
    <implication>
      Capture the primary condition first. Run every cleanup action independently, protected from
      interruption. Record both the primary condition and any rollback failure in a structured
      diagnostic, then re-signal the primary condition exactly, preserving its identity and
      payload rather than a reconstructed copy. A failed rollback should leave state retryable,
      not discarded.
    </implication>
    <example>
      // Pseudocode: aggregate cleanup that preserves the first condition.
      primary = null
      for step in cleanup_steps:
          try: step()
          catch c: if primary == null then primary = c else record_secondary(c)
      if primary != null: resignal(primary)   // same object, not a copy
    </example>
    <notes>
      <item>The set of conditions to catch must include cancellation/interruption, not only errors. In runtimes where user cancellation is a distinct condition class, a handler that catches errors alone leaves the cleanup half-run on a routine user action.</item>
    </notes>
  </principle>

  <principle name="bulk_cleanup_isolates_per_item_failure">
    <why>
      A teardown loop that aborts on the first failing item leaves every later item fully live —
      resources held, handlers registered, entries retained — while the caller's aggregate flag
      already reports "shut down". The system is now in a state no code was written to handle,
      and there is no remaining owner who will retry.
    </why>
    <implication>
      Attempt every item, collect the failures, and report after the traversal completes. The
      same shape applies to fan-out delivery: a failing recipient is pruned, healthy recipients
      still receive, and the registry is left free of dead entries.
    </implication>
  </principle>

  <principle name="the_commit_point_is_one_way">
    <why>
      Once the irreversible step has happened, the change exists. Treating a later failure — a
      notification, a cache update, a cleanup — as a failure of the whole operation makes callers
      retry something already applied, and reporting failure while the change is visible is worse
      than reporting the partial success.
    </why>
    <implication>
      Identify the single commit point explicitly. Everything before it is undoable and may fail
      the operation; everything after it is best-effort and may only be logged. Never pretend to
      roll back past the commit point. Externally visible side effects belong at or after the
      commit point, because effects already performed by a downstream handler cannot be reversed.
    </implication>
  </principle>
</atomicity_and_rollback>

<event_delivery>
  <description>
    A request that leaves and a result that comes back later must be matched. Matching by
    anything the caller could observe about the request — rather than by an identity allocated
    at enqueue time — is a bug that only appears when two requests are in flight.
  </description>

  <principle name="correlate_by_allocation_identity_not_by_content_key">
    <why>
      Matching a returning result to a pending request by a natural key — the target, the
      position, the resource name — is correct exactly while at most one request per key is in
      flight. As soon as an earlier request targets the same key, it consumes the later request's
      result. The symptom is a rare, load-dependent mismatch that is nearly impossible to
      reproduce.
    </why>
    <implication>
      Allocate an identity when the request is appended, and correlate on that identity alone.
      Capture whatever context the commit will need at enqueue time — not at completion time,
      when it may already have changed.
    </implication>
    <example>
      // Pseudocode: capture-at-enqueue, correlate-after, commit-once-on-success.
      id = queue.append(request)              // identity allocated by the append itself
      pending[id] = capture_affected_context()

      on result:
          ctx = pending.remove(result.id)
          if ctx == null: ignore              // duplicate or unmatched: never guess
          if result.success: commit(ctx)      // exactly one commit per request
    </example>
    <notes>
      <item>Retain processed identities long enough that a retry arriving in a later cycle cannot commit a second time; removing the pending entry is what makes the commit exactly-once.</item>
      <item>The provider that produces the result must not mutate the owned state itself. The requester commits, and only for a matching successful result. This keeps the commit decision in one place.</item>
    </notes>
  </principle>

  <principle name="idempotency_is_keyed_by_actor_and_command_with_a_payload_fingerprint">
    <why>
      A cache keyed only by command identifier has two holes. A client that reconnects can reuse
      a previously accepted identifier for entirely different content, and the system trusts it.
      And an identifier collision silently overwrites the legitimate cached result of the
      original command, so the original requester's retry now receives someone else's answer.
    </why>
    <implication>
      Key the result cache by actor plus command identifier, and store a fingerprint of the
      payload alongside the result. An exact repeat returns the cached result without re-applying
      the effect. An identifier reused with different content is rejected, and the rejection must
      not replace the existing entry. Bound the cache; an unbounded idempotency cache is a
      resource leak with a retention policy nobody wrote.
    </implication>
  </principle>

  <principle name="an_invariant_spanning_two_stores_needs_explicit_serialization">
    <why>
      A single-cell atomic update primitive gives atomicity for that one cell. The moment an
      invariant relates two cells — a public queue and its private side table, an index and its
      backing collection — two independently atomic updates can interleave, and an interruption
      between them leaves the invariant broken with no error raised anywhere.
    </why>
    <implication>
      Serialize the paired update under one mutual-exclusion primitive and make the pair
      uninterruptible. If that is not possible, collapse the two cells into one so that a single
      atomic update covers the whole invariant.
    </implication>
  </principle>
</event_delivery>

<durability_and_evolution>
  <description>
    Building a transaction on top of a store that does not offer one. The ordering below is not
    a preference; each step exists because of a specific crash window.
  </description>

  <principle name="content_first_pointer_last_cleanup_best_effort">
    <why>
      If the pointer to the current state is published before the content it names is written, a
      crash in between leaves a pointer to nothing — the state is unrecoverable. Writing the
      content first and publishing the pointer last means a crash at any point leaves the
      previous, complete state still reachable. The published pointer swap is the commit point.
    </why>
    <implication>
      Write every new content record, then publish the new pointer as the last step. On failure
      before publication, best-effort remove the unpublished content while preserving and
      re-raising the original error. After publication, garbage-collect the superseded content
      best-effort — a cleanup failure must never turn an already-published commit into a reported
      failure.
    </implication>
    <notes>
      <item>Retained records carry monotonic versions so a completing operation deletes exactly the snapshot it captured and never a newer one written while it was in flight.</item>
    </notes>
  </principle>

  <principle name="single_writer_coalescing_write_queue">
    <why>
      The debounced save everyone writes is usually wrong in the same four ways. Concurrent
      writers produce out-of-order final state. A write that fails is forgotten, so the caller's
      last successful checkpoint is silently older than they believe. Unbounded chaining of
      pending writes grows without limit. And a request arriving while a write settles is dropped
      because the writer has already decided it is idle.
    </why>
    <implication>
      At most one active write, plus at most one retained pending snapshot — the latest, since
      intermediate states have no value. A failure remains pending and reportable: draining
      continues to report failure until a later successful write explicitly clears it, so a
      caught write failure can never be treated as success. A drain retries the same final
      snapshot even when no new request has arrived. Shutdown, and any transition that abandons
      the context (navigating away, closing, switching targets), must drain first.
    </implication>
    <notes>
      <item>The adversarial cases that belong in the test suite: a reentrant synchronous request from within the write, retry with no new request, an absent/empty value as a legitimate state, and two concurrent drain callers. Each must still preserve exactly one active writer.</item>
    </notes>
  </principle>

  <principle name="acknowledged_is_not_durable">
    <why>
      Accepting a mutation, updating in-memory state, and replying "accepted" can all complete
      before anything is written durably. The requester now believes the change is permanent. A
      crash before the write loses work that was explicitly confirmed — the most damaging class
      of data loss, because it is the class users trust least in retrospect.
    </why>
    <implication>
      Decide deliberately which acknowledgements imply durability, and for those, acknowledge
      only after a durable write completes. Where the single-writer assumption is load-bearing,
      enforce it: a second process pointed at the same state has no reason to respect an
      assumption that lives only in the first process's code. Process-level locking is part of
      the durability design, not an operational detail.
    </implication>
  </principle>

  <principle name="migrations_default_only_a_genuinely_absent_own_property">
    <why>
      A migration that supplies a default whenever a field "looks missing" cannot distinguish
      "this payload predates the field" from "this payload is corrupt and carries the field with
      a broken value". Treating the second case as the first turns corruption into
      plausible-looking data that validation will never catch again, because the migration
      repaired it before validation ran.
    </why>
    <implication>
      Default only when the property is genuinely absent as an own property. If a legacy payload
      carries it explicitly as empty or invalid, preserve it so the current validation rejects the
      payload. Never silently replace an unreadable or future-version record with a fresh empty
      one — surface it. Keep the distinct compatibility boundaries separate and version them
      independently: the payload schema, the storage layout, and the content encoding are three
      different things that evolve on three different schedules.
    </implication>
  </principle>
</durability_and_evolution>

<read_semantics>
  <description>
    Reads across a boundary have more outcomes than reads within one. Collapsing them into a
    boolean is a reliable source of both data loss and permissive behavior.
  </description>

  <principle name="reads_are_three_state_present_absent_unknown">
    <why>
      A cross-boundary read can succeed, succeed with nothing, or fail to determine anything —
      the region is not loaded, the range is out of bounds, the source is unavailable. Code that
      models this as a two-valued answer maps "I could not read it" onto "it is not there", which
      is the permissive branch in almost every design. The consequences are symmetric and both
      bad: state gets discarded because it "wasn't there", or a check gets passed because nothing
      contradicted it.
    </why>
    <implication>
      Represent the three outcomes distinctly in the return type and force callers to handle them
      separately. An unknown result takes the conservative branch — preserve existing state,
      block the effect, retry later — never the permissive one. Confirming that something changed
      requires positive evidence of the new value, not the absence of the old one.
    </implication>
  </principle>

  <principle name="absence_is_not_a_comparable_value">
    <why>
      Cache validity expressed as "the stamp is unchanged" degenerates when the stamp source is
      missing and the stamp becomes a null sentinel. The stored sentinel then compares equal to
      every future sentinel, so the cache is permanently valid and never revalidates. The same
      shape appears wherever a domain value doubles as the absence marker: a legitimately empty
      stored value is indistinguishable from a miss, so a presence check answers the wrong
      question and a lookup reports failure for a key that exists.
    </why>
    <implication>
      Treat absence as a miss, not as a value that can match. Presence must be answered by an
      explicit presence check that is independent of the stored value's own domain, never by
      testing the value for emptiness. A wrapper that folds success-with-empty and failure into
      one return cannot express the difference and must not be used where the difference matters.
    </implication>
  </principle>
</read_semantics>

<time_and_accumulation>
  <description>
    Time-derived state is state whose owner is a clock you do not control, so a mutation paced
    by elapsed time is a boundary crossing like any other and fails in the same ways: a reading
    is lost, one interval is applied twice, or a backlog is reconstructed and applied all at
    once. Anything paced by a clock — throttles, cooldowns, cadences, rate limits, bounded
    waits, progress accumulation — shares a small set of edges that are almost never tested.
  </description>

  <principle name="no_catch_up_bursts">
    <why>
      A limiter implemented as "how many intervals have elapsed since the last event" emits one
      event per elapsed interval after any stall — a suspend, a long pause, a slow batch. The
      backlog arrives as a burst of backdated events against current state, which is both
      incorrect and, where the events cause an effect, disproportionate.
    </why>
    <implication>
      Store only the timestamp of the last emission. Emit immediately when the state is empty,
      and thereafter at most once when elapsed time has advanced by at least the interval. A long
      gap produces one event, not a queue of them.
    </implication>
  </principle>

  <principle name="cadence_follows_a_clock_not_a_tick_count">
    <why>
      Pacing by iteration count — frames, loop turns, poll cycles — makes observable behavior a
      function of machine speed and load, so the same code runs at different rates on different
      hardware and a slow environment changes the semantics rather than merely running slower.
    </why>
    <implication>Express cadence in elapsed time from an injected clock. Iteration counts are for work, not for time.</implication>
  </principle>

  <principle name="bound_a_wait_with_a_decrementing_budget_not_an_absolute_deadline">
    <why>
      This is the canonical statement of the rule; other skills point here rather than restate
      it. A wall clock is not monotonic — synchronization corrections, suspend and resume, and
      manual changes move it in both directions during normal operation — so two readings are
      not two points on one timeline. An absolute deadline ("stop once the clock reads T")
      inherits every jump: a forward jump ends the wait before the awaited work could possibly
      have finished, and a backward jump extends it arbitrarily, up to never ending. The same
      subtraction wedges a limiter, because "now minus last" is negative after a regression,
      compares below any interval, and stays wedged until the clock returns to where it was.
    </why>
    <implication>
      Carry a remaining budget and subtract the time each slice actually consumed, looping while
      the budget stays positive. Termination then depends only on slices consumed and never on
      an absolute reading, so a jump in either direction can neither shorten nor extend the
      wait, and a slow machine degrades into fewer iterations rather than a hang. Where an
      elapsed measurement is genuinely needed, a negative or otherwise invalid value resets the
      cycle at the current reading rather than blocking.
    </implication>
    <notes>
      <item>A budget is finite only if its input is: positive infinity minus a slice is still positive infinity, so an unbounded budget never decrements to zero and is a concrete non-termination case rather than a theoretical one. Validate the whole input domain when the budget is caller-supplied — NaN, both infinities, zero, negative, non-numeric — and reject rather than silently clamp. Bound the iteration count too, since a slice that returns immediately consumes no budget and would otherwise spin.</item>
      <item>A budget bounds one wait; it does not pace repeated ones. Reconstructing a backlog from total elapsed time reintroduces the catch-up burst above, so cadence stays governed by at most one emission per advance even when every individual wait is budgeted.</item>
    </notes>
  </principle>

  <principle name="accumulate_at_the_rate_in_force_at_each_step">
    <why>
      Recomputing total progress from total elapsed time using the current rate retroactively
      applies a rate change to history the old rate produced. Where the rate is under a
      participant's influence, this is directly exploitable: accumulate cheaply, switch to the
      favorable rate, and collect as though the favorable rate always applied.
    </why>
    <implication>
      Accumulate incrementally: each step adds its own elapsed time multiplied by the rate in
      force during that step. Never recompute the whole history with the newest rate.
    </implication>
  </principle>

  <principle name="accumulated_progress_is_keyed_to_the_identity_it_measures">
    <why>
      An accumulator measures progress toward a specific target. If the target is replaced while
      the accumulator survives — same position, different value — the accumulated progress now
      credits work that was never done against the replacement.
    </why>
    <implication>
      Make the accumulator's key the full identity of what it measures, not its location. Any
      identity change resets both in-progress and completed accumulation. Detect completion by
      whether the applied step reaches the remaining work, not by testing equality against a
      target total, which is unreliable for fractional accumulation.
    </implication>
  </principle>
</time_and_accumulation>

<anti_patterns>
  <avoid name="self_draining_producer">
    <description>A component appends events for consumers and also consumes them internally.</description>
    <instead>Producer appends; exactly one consumer drains atomically and owns retry. Use a separate response channel if the producer needs the outcome.</instead>
  </avoid>
  <avoid name="snapshot_then_clear_as_a_transaction">
    <description>Calling a read operation and then a clear operation and treating the pair as atomic.</description>
    <instead>Require a single drain operation from the owner; report its absence as a blocking gap rather than emulating it.</instead>
  </avoid>
  <avoid name="broad_overwrite_for_a_narrow_change">
    <description>Using a whole-record restore or reset to change one field because no narrow operation exists.</description>
    <instead>Add the narrow operation at the owner. Enumerate everything a broad operation touches before using it.</instead>
  </avoid>
  <avoid name="position_keyed_metadata">
    <description>A side table of per-item attributes keyed by the item's current position.</description>
    <instead>Store attributes with the value so they travel with it; empty positions hold none.</instead>
  </avoid>
  <avoid name="restoring_shared_state_you_did_not_install">
    <description>Snapshotting a shared slot and restoring it later, discarding writes from other owners in the window.</description>
    <instead>Own your namespace, or track an ownership flag and remove only what you installed.</instead>
  </avoid>
  <avoid name="cleanup_error_replaces_the_real_error">
    <description>An unwind block whose own failure propagates instead of the failure that triggered it.</description>
    <instead>Capture the primary condition, run all cleanup independently, report both, re-signal the primary unchanged.</instead>
  </avoid>
  <avoid name="teardown_loop_aborting_on_first_failure">
    <description>A bulk cleanup that stops at the first failing item while the aggregate flag reports completion.</description>
    <instead>Attempt every item, collect failures, report after the traversal.</instead>
  </avoid>
  <avoid name="correlating_by_natural_key">
    <description>Matching an async result to its request by target, position, or name.</description>
    <instead>Correlate by an identity allocated at enqueue time; commit once, on a matching success.</instead>
  </avoid>
  <avoid name="id_only_idempotency_cache">
    <description>A result cache keyed by command identifier alone, and unbounded.</description>
    <instead>Key by actor plus identifier, store a payload fingerprint, reject mismatched reuse without replacing the entry, and bound the cache.</instead>
  </avoid>
  <avoid name="pointer_published_before_content">
    <description>Updating the current-state pointer before the content it names is fully written.</description>
    <instead>Write content first, publish the pointer last, treat post-publication cleanup as best-effort.</instead>
  </avoid>
  <avoid name="fire_and_forget_write_queue">
    <description>A debounced save whose failures vanish, whose writes can overlap, or whose pending chain grows unbounded.</description>
    <instead>One active write plus one retained latest snapshot; failures stay pending until a later success clears them; drain before shutdown or context change.</instead>
  </avoid>
  <avoid name="permissive_migration_defaults">
    <description>Supplying a default whenever a field is missing OR present-but-invalid.</description>
    <instead>Default only a genuinely absent own property; let invalid values reach validation and be rejected.</instead>
  </avoid>
  <avoid name="unknown_treated_as_absent">
    <description>Collapsing an unreadable or unavailable read into "not present" and taking the permissive branch.</description>
    <instead>Model present/absent/unknown distinctly; unknown takes the conservative branch.</instead>
  </avoid>
  <avoid name="catch_up_burst_limiter">
    <description>Emitting one event per elapsed interval after a stall, pacing by iteration count, or bounding a wait with an absolute deadline.</description>
    <instead>Store the last emission time, emit at most once per advance, derive cadence from an injected clock, and bound waits with a validated decrementing budget.</instead>
  </avoid>
  <avoid name="retroactive_rate_application">
    <description>Recomputing accumulated progress over all elapsed time using the current rate.</description>
    <instead>Accumulate per step at the rate in force during that step; reset on identity change.</instead>
  </avoid>
</anti_patterns>

<best_practices>
  <practice priority="critical">Name the owner of every piece of state before designing the mutation path; a caller may request, only the owner may guarantee atomicity.</practice>
  <practice priority="critical">Identify the single commit point of every multi-step operation; make everything before it undoable and everything after it best-effort.</practice>
  <practice priority="critical">Correlate async results by an identity allocated at enqueue time, and commit exactly once on a matching success.</practice>
  <practice priority="critical">Take the conservative branch on any read whose outcome is unknown rather than merely absent.</practice>
  <practice priority="high">Preserve the primary failure across rollback: capture first, clean up completely and uninterruptibly, re-signal the original condition itself.</practice>
  <practice priority="high">Write content before publishing the pointer that names it, and never fail a committed operation because of a cleanup error.</practice>
  <practice priority="high">Acknowledge durability only after a durable write; enforce the single-writer assumption rather than documenting it.</practice>
  <practice priority="high">Track ownership flags for anything installed into shared state so teardown removes exactly what you added.</practice>
  <practice priority="medium">Attach attributes to values, not to positions; define split, merge, transfer, and destruction behavior explicitly.</practice>
  <practice priority="medium">Keep write coalescing to one active writer plus one retained latest snapshot, with failures that stay pending.</practice>
  <practice priority="medium">Derive cadence from an injected clock, bound waits with a validated decrementing budget rather than a deadline, and accumulate at the rate in force at each step.</practice>
  <practice priority="medium">Assert reference identity across rollback, exactly-once commit per request, and non-modification of state outside the transaction — equality-only assertions pass in the broken cases.</practice>
</best_practices>

<rules priority="critical">
  <rule>A producer never drains its own outbox; exactly one consumer drains and owns retry.</rule>
  <rule>Never compose two atomic operations and call the result atomic; require the combined operation from the owner.</rule>
  <rule>Never restore a snapshot of state owned by someone else; use ownership flags instead.</rule>
  <rule>A cleanup or rollback failure must never replace or suppress the condition that triggered it.</rule>
  <rule>An unreadable or indeterminate read takes the conservative branch, never the permissive one.</rule>
</rules>

<rules priority="standard">
  <rule>Correlate by allocation identity; key idempotency by actor plus command with a payload fingerprint.</rule>
  <rule>Commit content before pointer; treat post-commit cleanup as best-effort and non-failing.</rule>
  <rule>Default only genuinely absent own properties during migration; never repair corruption silently.</rule>
  <rule>Version the payload schema, the storage layout, and the content encoding independently.</rule>
  <rule>Pace by elapsed clock time without catch-up bursts; bound waits by a validated decrementing budget rather than an absolute deadline; accumulate incrementally at the current rate.</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Locate the boundary and its ownership</objective>
    <step order="1">
      <action>Identify which component owns each piece of state the operation touches</action>
      <tool>Read / Grep for the declaration and the mutating operations</tool>
      <output>Owner map per state element</output>
    </step>
    <step order="2">
      <action>Trace the mutation path and mark every crossing: queue, message, callback, persisted record</action>
      <tool>Grep for enqueue/drain/publish/commit sites</tool>
      <output>Crossing map with duplication and loss windows</output>
    </step>
  </phase>
  <phase name="design">
    <objective>Establish the transaction shape</objective>
    <step order="1">
      <action>Name the commit point; classify each step as undoable, committing, or best-effort</action>
      <tool>Design review against the ordering principles</tool>
      <output>Ordered step classification</output>
    </step>
    <step order="2">
      <action>Define correlation identity, idempotency key, and consumer obligation for every emitted collection</action>
      <tool>Edit contract and type declarations</tool>
      <output>Explicit cross-boundary contract</output>
    </step>
  </phase>
  <phase name="validate">
    <objective>Prove the invariants under failure</objective>
    <step order="1">
      <action>Exercise interruption at each step, duplicate delivery, concurrent drain, and reentrant request</action>
      <tool>Targeted failure-injection tests</tool>
      <output>No duplication, no loss, no masked errors</output>
    </step>
    <step order="2">
      <action>Assert identity across rollback and confirm state outside the transaction is unchanged</action>
      <tool>Identity and non-modification assertions</tool>
      <output>Rollback correctness evidence</output>
    </step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Cleanup ordering that is safe but harder to read than necessary</example>
    <example severity="medium">Correlation by natural key with only one request in flight today</example>
    <example severity="high">Read-then-clear treated as a transaction, or a pointer published before its content</example>
    <example severity="critical">Snapshot-restore over state owned elsewhere, a rollback that masks the primary failure, or a migration that repairs corruption silently</example>
  </examples>
</error_escalation>

<constraints>
  <must>Establish the owner and the commit point before proposing any cross-boundary mutation</must>
  <must>Preserve the primary failure condition through every rollback path</must>
  <must>Model unknown reads distinctly from absent reads and fail closed on unknown</must>
  <avoid>Emulating a missing owner operation from a higher layer</avoid>
  <avoid>Correlating async results by content keys or matching on natural identifiers</avoid>
  <avoid>Treating an acknowledgement as a durability guarantee</avoid>
</constraints>

<related_skills>
  <skill name="trust-boundaries">Validating untrusted input and deriving authority before a mutation is accepted at all</skill>
  <skill name="effect-ts">Idiomatic encoding of these invariants with Ref, Semaphore, Queue, and scoped lifecycles</skill>
  <skill name="sql-ecosystem">Engine-provided ACID for the cases where the store can do this work for you</skill>
  <skill name="testing-patterns">Fixture isolation, failure injection, and non-vacuous assertions for these invariants</skill>
  <skill name="test-integrity">The test-time consequence of the cleanup rules here: a teardown failure that masks a real one, and auditing whether a green run proves the invariant at all. This skill owns the runtime rule; that one owns the audit.</skill>
  <skill name="performance-benchmarking">Choosing a deterministic metric over wall-clock time, and asserting the wait budget and iteration count rather than elapsed seconds</skill>
  <skill name="investigation-patterns">Evidence-based tracing of duplication, loss, and masked-failure reports</skill>
</related_skills>

<related_agents>
  <agent name="design">Review ownership boundaries and commit-point placement across components</agent>
  <agent name="quality-assurance">Audit rollback paths, drain contracts, and emitted-collection obligations</agent>
  <agent name="test">Design interruption, duplicate-delivery, and concurrent-drain coverage</agent>
  <agent name="database">Decide when to push a transaction down into a store that provides one</agent>
</related_agents>
