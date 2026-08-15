---
name: state-transactions
description: Use when a mutation crosses a boundary to reach the state owner, such as outbox and drain contracts, atomicity without transactional stores, idempotency keys, correlation IDs, commit points, write-queue coalescing, and clock-paced throttles.
version: 3.0.0
---

The moment a mutation leaves the code that requests it and arrives at the component that owns the state. That
crossing is where ownership blurs, where "read then write" stops being atomic, where a retry becomes a
duplicate, and where an acknowledgement gets mistaken for durability.

Whether the mutation is *authorized* and its inputs *believable* belongs to
[trust-boundaries](../trust-boundaries/SKILL.md); this file assumes it is, and asks what makes its application
correct. Engine-level ACID belongs to [sql-ecosystem](../sql-ecosystem/SKILL.md) — here we build these
guarantees **above** a store that does not offer them.

## Vocabulary

**The owner** is the single component whose internal representation the state actually is. Only the owner can
offer an operation atomic with respect to that state; everything else is a caller making a request.

**An atomic operation** either fully happens or does not, as observed by every other reader. **Composing two
atomic operations does not produce an atomic operation.**

**The commit point** is the single irreversible step that makes a change visible. Everything before it must be
undoable; nothing after it may be reported as a failure of the change itself.

**Acknowledged and durable are different guarantees.** "The owner accepted it" and "it survives a crash" —
conflating them is how accepted work disappears.

## Ownership

Most cross-boundary bugs are ownership bugs wearing a different costume. Before asking whether an operation is
atomic, ask who owns the state and whether the operation being used was designed by that owner for this
purpose.

**A producer never drains its own outbox.** When a component appends events for a consumer and also consumes
them itself, three things break at once: the published contract changes silently, because existing consumers
now see an empty collection; observability is lost, since nothing external can tell what was produced; and
delivery becomes unsafe — if the producer drains and then fails while materializing the effect, **the record is
already gone and no retry is possible.** A producer appends; exactly one consumer drains, atomically, and
defines its own retry. Repeated drains are empty by construction. If the producer needs the outcome, that is a
separate response channel.

**An emitted collection states its consumer obligation.** "Things that happened" and "things you must do" have
identical shapes — a list of records appended during processing — and **opposite consumer obligations**.
Nothing in the type distinguishes an audit record already applied by the owner from deferred work. A consumer
that guesses wrong applies the effect a second time. Declare in the contract, and preferably in the name,
whether draining is required, optional, or forbidden.

**Read-then-clear is not a transaction.** A snapshot call followed by a reset call is two atomic operations,
not one atomic pair. Between them another mutation can interleave, or the second call can fail — producing
duplication (the snapshot is used and the state is never cleared) or loss (the state is cleared and the
snapshot dropped). **This is invisible in single-threaded testing and unfixable at the caller.** When the caller
needs "take everything and leave it empty", the owner must expose a single drain operation. Until it does, the
higher layer must not synthesize the behavior — it reports the missing operation as a blocking gap.

**Add the narrow operation at the owner.** When the operation a caller needs is missing, the nearest available
one is almost always broader: a full restore, a full reset, a whole-record overwrite. Using it *appears* to
work because the field of interest ends up correct, **while neighbouring state the caller never considered is
silently destroyed.** A reset that also clears unrelated persisted contents is the classic shape. Add the narrow
operation where the state lives, and review any use of a broad restore by enumerating everything it touches.

**Metadata belongs to the value, not to a position.** Keying per-item attributes by current position — slot
index, row number, tab order, line number — is a side table correct only while nothing moves. Ownership is
violated the moment values move, swap, split, merge, transfer, or are restored from persistence, and the
failure is the worst kind: **stale attributes silently reattach to an unrelated replacement value that happens
to land in the same position.** Use one stored representation where attributes travel with the value; an empty
position carries none; splitting copies them; merging is permitted only when value and attributes match. Where
the model cannot be fixed yet, every transition must explicitly move or clear the side entry, destroying a
container is a *transfer* rather than a deletion, and persistence must reject attributes whose backing value no
longer exists.

## Atomicity and rollback

**Rollback is written last, tested least, and runs only in situations nobody rehearsed.** Each rule below is a
way rollback fails while appearing to succeed.

**Never snapshot and restore state you do not own.** Capture-once snapshotting assumes you are the only writer
between capture and restore. If another component writes to the same shared slot in that window, restoring
discards its write — destructive loss of state you never knew about, attributed to nobody. **The wider the
shared namespace, the more likely a second writer exists.** Represent your state in a namespace you exclusively
own. Where a shared slot is unavoidable, record an **ownership flag per resource**: repeated setup keeps
ownership, resources already present stay non-owned, and teardown removes only what you actually installed.
Reference-counted installation is the same rule for a slot shared by many activations.

**Restore recovers values, not identity.** Rolling back by replacing state with copies restores values but not
object identities. Any caller holding a reference obtained before the transaction still points at the mutated
original, so **mutations made during the failed transaction remain observable through those aliases after the
rollback "succeeded"** — and mutable containers, tables, and collections are exactly what callers hold
references to. Either roll back by mutating the originals in place, or guarantee no external references exist.
Tests must retain an external alias across the rollback and assert **identity, not equality**: an equality-only
assertion passes in precisely the broken case.

**A rollback failure must not replace the original condition.** A naive unwind block lets a failure raised
during cleanup propagate in place of the one that triggered it. The caller then sees a confusing secondary
error, the real cause is gone, and cleanup steps after the failing one are skipped.

```
primary = null
for step in cleanup_steps:
    try: step()
    catch c: if primary == null then primary = c else record_secondary(c)
if primary != null: resignal(primary)   // the same object, not a copy
```

Capture the primary condition first, run every cleanup action independently and protected from interruption,
record both, then re-signal the primary **preserving its identity and payload** rather than a reconstructed
copy. A failed rollback leaves state retryable, not discarded. **The set of conditions to catch includes
cancellation** — in runtimes where user cancellation is a distinct class, a handler catching errors alone
leaves cleanup half-run on a routine user action.

**Bulk cleanup isolates per-item failure.** A teardown loop that aborts on the first failing item leaves every
later item fully live — resources held, handlers registered, entries retained — **while the caller's aggregate
flag already reports "shut down".** The system is now in a state no code was written to handle, with no
remaining owner to retry. Attempt every item, collect failures, report after the traversal. The same shape
applies to fan-out delivery: prune the failing recipient, deliver to the healthy ones, leave no dead entries.

**The commit point is one way.** Once the irreversible step has happened, the change exists. Treating a later
failure — a notification, a cache update, a cleanup — as a failure of the whole operation makes callers retry
something already applied, and **reporting failure while the change is visible is worse than reporting partial
success.** Identify the commit point explicitly: everything before it is undoable and may fail the operation;
everything after it is best-effort and may only be logged. Externally visible side effects belong at or after
the commit point, because effects already performed downstream cannot be reversed.

## Correlating a result with its request

**Correlate by allocation identity, never by a content key.** Matching a returning result to a pending request
by a natural key — the target, the position, the resource name — is correct exactly while at most one request
per key is in flight. As soon as an earlier request targets the same key, **it consumes the later request's
result**, and the symptom is a rare load-dependent mismatch that is nearly impossible to reproduce.

```
id = queue.append(request)              // identity allocated by the append itself
pending[id] = capture_affected_context()

on result:
    ctx = pending.remove(result.id)
    if ctx == null: ignore              // duplicate or unmatched: never guess
    if result.success: commit(ctx)      // exactly one commit per request
```

Capture whatever context the commit will need **at enqueue time**, not at completion time when it may already
have changed. Retain processed identities long enough that a retry arriving in a later cycle cannot commit
twice — removing the pending entry is what makes the commit exactly-once. **The provider producing the result
must not mutate the owned state itself**; the requester commits, and only on a matching success, keeping the
commit decision in one place.

**Idempotency is keyed by actor and command, with a payload fingerprint.** A cache keyed only by command
identifier has two holes: a client that reconnects can reuse a previously accepted identifier for entirely
different content, and an identifier collision silently overwrites the legitimate cached result — so **the
original requester's retry now receives someone else's answer.** Key by actor plus identifier and store a
fingerprint of the payload. An exact repeat returns the cached result without re-applying; an identifier reused
with different content is rejected, **and the rejection must not replace the existing entry.** Bound the cache:
an unbounded idempotency cache is a resource leak with a retention policy nobody wrote.

**An invariant spanning two stores needs explicit serialization.** A single-cell atomic primitive gives
atomicity for that one cell. The moment an invariant relates two cells — a public queue and its private side
table, an index and its backing collection — two independently atomic updates can interleave, and an
interruption between them **leaves the invariant broken with no error raised anywhere.** Serialize the paired
update under one mutual-exclusion primitive and make the pair uninterruptible, or collapse the two cells into
one.

## Durability

The ordering below is not a preference; each step exists because of a specific crash window.

**Content first, pointer last, cleanup best-effort.** If the pointer to current state is published before the
content it names is written, a crash in between leaves **a pointer to nothing** — the state is unrecoverable.
Writing content first and publishing the pointer last means a crash at any point leaves the previous, complete
state reachable. The pointer swap *is* the commit point. On failure before publication, best-effort remove the
unpublished content while preserving and re-raising the original error. After publication, garbage-collect the
superseded content best-effort — **a cleanup failure must never turn an already-published commit into a
reported failure.** Retained records carry monotonic versions, so a completing operation deletes exactly the
snapshot it captured and never a newer one written while it was in flight.

**Single-writer coalescing write queue.** The debounced save everyone writes is usually wrong in the same four
ways: concurrent writers produce out-of-order final state; a write that fails is forgotten, so the caller's last
successful checkpoint is silently older than they believe; unbounded chaining of pending writes grows without
limit; and a request arriving while a write settles is dropped because the writer already decided it was idle.

At most one active write, plus at most one retained pending snapshot — **the latest, since intermediate states
have no value.** A failure remains pending and reportable: draining continues to report failure until a later
successful write explicitly clears it, so a caught write failure can never be treated as success. A drain
retries the same final snapshot even when no new request arrived. Shutdown, and any transition abandoning the
context — navigating away, closing, switching targets — must drain first.

The adversarial cases that belong in the suite: a reentrant synchronous request from within the write; retry
with no new request; an absent or empty value as a legitimate state; two concurrent drain callers. Each must
still preserve exactly one active writer.

**Acknowledged is not durable.** Accepting a mutation, updating in-memory state, and replying "accepted" can
all complete before anything is written durably. A crash before the write loses work that was **explicitly
confirmed** — the most damaging class of data loss, because it is the class users trust least in retrospect.
Decide deliberately which acknowledgements imply durability and acknowledge those only after the durable write.
Where the single-writer assumption is load-bearing, **enforce it**: a second process pointed at the same state
has no reason to respect an assumption living only in the first process's code. Process-level locking is part
of the durability design, not an operational detail.

**Migrations default only a genuinely absent own property.** A migration supplying a default whenever a field
"looks missing" cannot distinguish "this payload predates the field" from "this payload is corrupt and carries
the field with a broken value". Treating the second as the first **turns corruption into plausible-looking data
that validation will never catch again**, because the migration repaired it before validation ran. Default only
on genuine absence as an own property; if a legacy payload carries it explicitly as empty or invalid, preserve
it so current validation rejects the payload. Never silently replace an unreadable or future-version record
with a fresh empty one — surface it. And version the payload schema, the storage layout, and the content
encoding **independently**: three different things evolving on three different schedules.

## Reads have three outcomes

A cross-boundary read can succeed, succeed with nothing, or **fail to determine anything** — the region is not
loaded, the range is out of bounds, the source is unavailable. Code modelling this as two values maps "I could
not read it" onto "it is not there", which is the permissive branch in almost every design. The consequences
are symmetric and both bad: state gets discarded because it "wasn't there", or a check passes because nothing
contradicted it.

Represent all three distinctly in the return type and force callers to handle them separately. **An unknown
result takes the conservative branch** — preserve existing state, block the effect, retry later. Confirming
that something changed requires positive evidence of the new value, not the absence of the old one.

**Absence is not a comparable value.** Cache validity expressed as "the stamp is unchanged" degenerates when
the stamp source is missing and the stamp becomes a null sentinel: the stored sentinel compares equal to every
future sentinel, so **the cache is permanently valid and never revalidates.** The same shape appears wherever a
domain value doubles as the absence marker — a legitimately empty stored value is indistinguishable from a
miss, so a presence check answers the wrong question. Answer presence with an explicit check independent of the
stored value's own domain, never by testing the value for emptiness. A wrapper folding success-with-empty and
failure into one return cannot express the difference and must not be used where the difference matters.

## Time

Time-derived state is state whose owner is a clock you do not control, so a mutation paced by elapsed time is a
boundary crossing like any other, failing the same ways: a reading is lost, one interval is applied twice, or a
backlog is reconstructed and applied all at once.

**No catch-up bursts.** A limiter implemented as "how many intervals have elapsed since the last event" emits
one event per elapsed interval after any stall — a suspend, a long pause, a slow batch — and the backlog
arrives as a burst of backdated events against current state. Store only the timestamp of the last emission,
emit immediately when the state is empty, and thereafter at most once when elapsed time has advanced by at
least the interval. **A long gap produces one event, not a queue of them.**

**Cadence follows a clock, not a tick count.** Pacing by iteration count — frames, loop turns, poll cycles —
makes observable behavior a function of machine speed and load, so **a slow environment changes the semantics
rather than merely running slower.** Express cadence in elapsed time from an injected clock; iteration counts
are for work, not for time.

**Bound a wait with a decrementing budget, not an absolute deadline.** This is the canonical statement; other
skills point here rather than restate it. A wall clock is not monotonic — synchronization corrections, suspend
and resume, and manual changes move it in both directions during normal operation — so two readings are not two
points on one timeline. An absolute deadline inherits every jump: **a forward jump ends the wait before the
awaited work could possibly have finished, and a backward jump extends it arbitrarily, up to never ending.**
The same subtraction wedges a limiter, because "now minus last" is negative after a regression, compares below
any interval, and stays wedged until the clock returns.

Carry a remaining budget and subtract the time each slice actually consumed, looping while it stays positive.
Termination then depends only on slices consumed, so a jump in either direction can neither shorten nor extend
the wait, and a slow machine degrades into fewer iterations rather than a hang.

**A budget is finite only if its input is:** positive infinity minus a slice is still positive infinity, so an
unbounded budget never decrements to zero — a concrete non-termination case, not a theoretical one. Validate
the whole input domain when the budget is caller-supplied — NaN, both infinities, zero, negative, non-numeric —
and reject rather than silently clamp. Bound the iteration count too, since a slice returning immediately
consumes no budget and would otherwise spin. And note that **a budget bounds one wait; it does not pace
repeated ones** — reconstructing a backlog from total elapsed time reintroduces the catch-up burst.

**Accumulate at the rate in force at each step.** Recomputing total progress from total elapsed time using the
*current* rate retroactively applies a rate change to history the old rate produced. Where the rate is under a
participant's influence this is directly exploitable: accumulate cheaply, switch to the favorable rate, collect
as though it always applied. Each step adds its own elapsed time multiplied by the rate in force during that
step.

**Accumulated progress is keyed to the identity it measures.** If the target is replaced while the accumulator
survives — same position, different value — the accumulated progress credits work that was never done against
the replacement. Key the accumulator by the full identity, reset both in-progress and completed accumulation on
any identity change, and detect completion by whether the applied step reaches the remaining work rather than
by equality against a target total, **which is unreliable for fractional accumulation.**

## Working through it

1. **Locate ownership.** Identify which component owns each piece of state the operation touches, then trace
   the mutation path and mark every crossing: queue, message, callback, persisted record. Output an owner map
   and a crossing map naming the duplication and loss windows.
2. **Establish the transaction shape.** Name the commit point and classify each step undoable, committing, or
   best-effort. Define the correlation identity, the idempotency key, and the consumer obligation for every
   emitted collection.
3. **Prove the invariants under failure.** Exercise interruption at each step, duplicate delivery, concurrent
   drain, and reentrant request. Assert **identity** across rollback, exactly-once commit per request, and that
   state outside the transaction is unchanged — equality-only assertions pass in the broken cases.

## Related

- [trust-boundaries](../trust-boundaries/SKILL.md) — authorizing the mutation before it is applied at all
- [effect-ts](../effect-ts/SKILL.md) — idiomatic encoding with Ref, Semaphore, Queue, and scoped lifecycles
- [sql-ecosystem](../sql-ecosystem/SKILL.md) — engine-provided ACID where the store can do this work for you
- [testing-patterns](../testing-patterns/SKILL.md) — fixture isolation and failure injection for these invariants
- [test-integrity](../test-integrity/SKILL.md) — the audit side of the cleanup rules: a teardown failure masking a real one
- [performance-benchmarking](../performance-benchmarking/SKILL.md) — deterministic metrics over wall-clock time
- [investigation-patterns](../investigation-patterns/SKILL.md) — tracing duplication, loss, and masked-failure reports
