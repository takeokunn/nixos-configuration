---
name: trust-boundaries
description: Use when untrusted input crosses a privilege boundary, such as parsing client or network requests, decoding payloads or deserialization, expanding archives, restoring persisted files, evaluating input via eval, validating paths, URLs, or schemes, or guarding against TOCTOU. Covers fail-closed validation, decode budgets, format-template injection, and log injection.
version: 3.0.0
---

The central question is never "is this value well-formed?" but **"what authority does this value carry, and
what must be recomputed rather than believed?"**

Almost every hole in this area comes from a check that was written in the right place but **ran in the wrong
order, measured the wrong quantity, or trusted a value already normalized past the point where the check could
see the problem.** Each rule below leads with the mechanism, because the rule alone does not tell you where
else it applies.

## Vocabulary

**A trust boundary** is the line at which data stops being produced by code you control. Every value crossing
it is a *claim*, not a fact — and a boundary is not only a network socket: a persisted file the user can edit,
a byte stream from whatever program the user chose to run, an archive member, a plugin's return value, and an
inter-language call all cross one.

**Fail closed.** When a check cannot be completed — ambiguous input, unknown key, exhausted budget,
unverifiable identity, missing checksum — the outcome is refusal. A missing verification result is a failure,
never a pass. **The symptom of a fail-open design is that removing an input makes the code take the happy
path.**

**Revalidate per boundary.** Validation on one side of a language, process, or module boundary does not protect
the other. Tests, stubs, alternative entry points, and future refactors all reach the inner code without
passing the outer parser.

**Refusal is a valid boundary.** Sometimes the correct validation is to not offer the capability. When no
wrapper can prove the property you need, declining the whole feature is legitimate and often the only sound
answer.

## Authority: accept evidence, never effects

An untrusted party may report **what happened to it**; it may never state **what should change.**

A command carrying a caller-chosen magnitude — "apply amount N", "grant tier T", "set remaining quota to Q" —
delegates the entire policy to the caller. Every mitigation, floor, ceiling, cooldown, and eligibility rule the
authoritative side implements is bypassed, because the caller supplied the answer those rules were supposed to
produce. **Authenticating the caller does not help: authentication answers "who", not "what may they assert".**

Replace the magnitude command with cause-specific evidence claims. The caller says *which cause occurred*, with
the parameters only it can observe; the authoritative side looks up the base magnitude, applies its own
modifiers and mitigations, clamps to its own bounds, and records the result. **The wire protocol should have no
representation for a raw effect at all** — an unrepresentable request cannot be smuggled through.

- Scoring: the client reports "objective X completed at time T"; the server owns the point value and the
  anti-replay window.
- Pricing: the client reports line identifiers and a coupon code; the server computes the price. It never
  receives a total.
- Rate limiting: the client never reports its remaining allowance; the limiter derives it from its own record.
- Permissions: the client never sends its own role; the authorizer resolves it from the identity.

**Identity and scope on every mutation.** Even a correctly derived effect can be applied to the wrong object.
If the mutation carries a target identifier but not the partition, tenant, session, or world the caller is
actually attached to, an otherwise valid claim reaches state the caller has no relationship with. Verify the
target belongs to the caller's scope **against the server's record of that scope**, never against a scope value
the caller supplied alongside the target.

**Reject unknown shapes.** A restore or update path that ignores unrecognized keys silently accepts whatever a
future — or hostile — writer adds, and gives an attacker a stable channel to probe which keys the
implementation actually consumes. Unknown keys, extra positional elements, and unexpected variants are errors,
not noise to skip. This is the structural half of fail-closed.

### Write the refusals first

The acceptance path is what everyone tests by accident; **the refusal path is what nobody notices has
regressed.**

1. Enumerate the hostile shapes: absent field, wrong type, out-of-domain value, ambiguous duplicate, oversized
   payload, wrong scope, replayed identifier, adversarially chosen boundary value.
2. Write a rejection test for each, asserting on a **typed** rejection value — a named error variant or tagged
   result — never on a message string, so a reworded diagnostic does not silently turn a rejection test into a
   tautology.
3. Snapshot the typed rejection set, so a change that removes a refusal is visible in review as a deleted
   entry rather than as an absence.
4. Only then implement the acceptance path.

The integrity of those tests — vacuity, assertion strength, not computing an expected value from the subject —
belongs to [test-integrity](../test-integrity/SKILL.md).

## Validation ordering

Most holes here are not missing checks. **The check exists, and is unreachable, or is measuring something
other than what crosses the wire.**

**Validate raw before any normalizing coercion.** When a guard exists to reject input class A, and a coercion
maps A into class B, running the coercion first makes the guard unreachable — by the time it runs, no input is
still in class A. The refactor that causes this ("normalize first, then validate, so the validator handles one
shape") sounds strictly safer and is exactly backwards, and **the guard keeps passing its own tests because the
fixtures were already in normal form.** Run class-discriminating checks against the original input, then
normalize, then run the shape-and-range checks that require normal form. If a guard's stated purpose is to
reject a class, its tests must include an input of that class *in its original form.*

*Symptom:* the rejection test only passes when the fixture is constructed pre-coercion; the same value through
the public entry point is accepted.

**Validate before and after decoding.** Decoders normalize *and* decoders preserve, and both defeat a check
placed on one side only. A percent-decoder may fold encoded control characters into innocuous ones, so a check
on the decoded string never sees the injection present in the encoded form; the same decoder may pass a
malformed escape triplet through unchanged, so a check on the raw form that assumed well-formedness never sees
it either. Reject encoded control characters and malformed escapes in the raw form, decode, then validate the
decoded value for its own domain. **Neither check substitutes for the other.**

**Validate in the encoding actually written.** A protocol framed by byte-level delimiters is attacked at the
byte level, but validation is usually written over decoded text. When the transport uses an encoding that
cannot represent some characters, the encoder substitutes replacement bytes — and a character harmless as a
codepoint can become the protocol's own terminator on the wire. The codepoint check is then correct and
irrelevant. Encode first, then check the encoded bytes for delimiters, control bytes, and length.

**One normalization boundary, plus a fail-closed helper.** Ambiguity in structured input — duplicate keys
differing only by case, a key present with no value, a continuation-folded field — is resolved differently by
each helper that parses it. When the authorization gate resolves it one way and the consuming helper another,
**the value that was authorized is not the value that is used.** That is the classic parameter-pollution bypass
and it needs no exotic input. Name exactly one boundary that owns normalization and let it *reject* ambiguity
rather than picking a winner; then independently make each downstream helper apply the same rejection rules.
The helper's fail-closed behavior must not be justified by "the gate already checked" — a later caller will
reach the helper directly.

**Match the validator's domain to the producer's range.** Validators get written against the example in front
of the author: an integer validator over a genuinely fractional quantity, a positive-only validator over a
value that legitimately reaches zero, a fixed-length validator over an identifier whose length changed. Over
live requests this rejects valid traffic; **over persisted data it is worse than no validator at all**, because
it makes legitimate saved state permanently unloadable and the failure appears long after the deploy that
caused it. Derive the accepted set from the producer's real range, and treat tightening a validator that runs
over persisted data as a breaking change requiring a migration path.

*The mirror-image trap:* a standard-library character-class predicate usually accepts a wider class than the
grammar you are implementing — decimal-digit predicates commonly accept non-ASCII digit forms. When the grammar
defines an ASCII-only class, implement the predicate explicitly and cover fullwidth and non-Latin digit forms
in regression tests.

**Guard non-finite and out-of-domain numbers at the boundary.** A non-finite value reaching persistent state is
unrecoverable and silent: every subsequent comparison against it is false, so the downstream guards neither
fire nor report the value that defeated them. The boundary is the last place the number is still identifiable
as input rather than as state. Model three outcomes without collapsing the first two: invalid input is
**rejected**; in-domain but inert input (zero, a direction that does not apply) is a **no-op that succeeds**;
in-domain active input is **applied**. Collapsing rejection into the inert case converts a validation failure
into a silent success.

**Consume single-use credentials last.** Burning a one-time code before every check has passed converts any
malformed or mis-scoped attempt into a denial of service against the legitimate holder, drivable by an attacker
who knows only the code's identifier. Validate identity, scope, expiry, and payload fully; consume only on the
success path.

## Resource budgets

**A budget checked after the work has been done bounds nothing, and a budget measured on the input does not
bound the output.**

**Enforce limits before allocation.** The general shape of a bypass is that the limit is enforced on the
emitted result while the memory blowup happens in an intermediate already built. Declared dimensions in a
header, a scale or exponent field, a repeat count, a denominator, or a requested buffer size all determine an
allocation *before any output exists to measure.* Validate header-declared dimensions before asking the decoder
for its buffer size and before allocating it; enforce numeric limits before raising to a power, factorizing, or
expanding to a decimal representation; truncate *while rendering* into a bounded destination rather than
building an unbounded string and trimming it.

*Symptom:* the limit constant appears in the code only near the return statement or the emit call.

**Compressed size does not bound decompression.** A cap on bytes received bounds only the transfer. A container
that is well-formed, correctly signed, and contains exactly the one member you expected can still expand to
many gigabytes, and post-extraction validation runs only after the disk is full. Cap the **uncompressed** byte
count and enforce it *during* extraction, aborting mid-stream. Any declared uncompressed size in the metadata
is itself untrusted: use it for early rejection only, and still count actual bytes written.

**Each decoder is its own boundary.** When a payload is decoded on two sides — a fast native path and a
fallback in another language, a preview renderer and the real consumer — the cap installed on one path does not
exist on the other. **The second path is usually the one nobody reviewed, precisely because it was described as
a fallback.** Every decode path carries its own budget, in its own language, with its own test.

**Bound retained payloads, not only reports.** An error object retaining the offending input for diagnostics
keeps the whole payload alive for as long as the error propagates, even when the rendered message is truncated.
Truncation at the presentation layer bounds what you *see*, not what you *hold* — sanitize and bound at
construction time.

### Extracting one file from an untrusted archive

1. List the members without extracting anything.
2. Require an exact single-member list matching what you expect. More than one member, or a different name, is
   a rejection — **not a reason to search the list.**
3. Extract only that member into a freshly created private temporary directory, never a shared or predictable
   location.
4. Reject symbolic links, hard links, device entries, and any member whose path is absolute or contains upward
   traversal, **before writing anything.**
5. Enforce a hard cap on uncompressed bytes while writing, aborting when exceeded.
6. Require a regular file, and verify its checksum or signature while it is still inside the private directory.
   **A missing checksum is a failure, not a skipped optional step** — this is where fail-open most often hides:
   code that verifies "if one is provided" grants an attacker the ability to remove verification by removing
   data.
7. Only after verification succeeds, publish to the destination.

**The order of 6 and 7 is load-bearing.** Verifying after publishing leaves an unverified artifact readable at
its final path for the width of the verification, and a consumer watching for the file's appearance will read
it there; a failed verification then has to be repaired by deletion rather than simply never having happened.

## Time-of-check to time-of-use

**A path is a name, not an object.** Every check performed against a path is a statement about whatever the
name referred to at that instant.

**Metadata checks are early rejection only.** Checking size, type, and ownership by path and then opening the
same path re-resolves the name, and between the two resolutions the entry can be replaced. The metadata check
has exactly one honest purpose: cheaply rejecting input that is already wrong. **Never phrase a security
property as "we checked the file first."**

**Pin the identity, then read the pin.** For a check to be meaningful, the checked object and the read object
must be provably the same object, which a second path resolution cannot establish. Hold an open descriptor
where the platform allows it, or create a link to the checked entry inside a private directory you control and
read only through that link, requiring the linked entry's identity to equal the pre-link identity. Where the
platform genuinely cannot close the window, compare the **full** identity tuple — device, inode, size,
modification time, status-change time — before and after, and *document the residual window explicitly* rather
than describing the result as safe. Device, inode, and size alone miss a same-size in-place rewrite.

Hardlink pinning is frequently unavailable — different devices, or a filesystem that refuses hard links. **The
fallback is not to check the path and open it afterwards; that is the original hazard restored.** Invert the
order: open first with symlink-following disabled, then run every type, size, and ownership check against the
*open descriptor*. The checks then describe the object you are already holding. The same inversion is right
wherever a platform offers descriptor-relative operations: resolve once, keep operating on the handle.

**Bound the read itself.** A pre-read size check does not bound the read, because the entry can grow between
the stat and the read; a post-read check bounds nothing, because the bytes are already in memory. Read at most
limit-plus-one bytes and reject when the extra byte materializes — one read that both enforces the cap and
detects overflow.

**Reject by type, because some types block.** A named pipe is the sharpest case: readable, reports size zero,
is not a symbolic link, satisfies every metadata precheck — **and then blocks forever on open**, before any
post-read size limit or identity comparison gets a chance to run. The denial of service happens strictly inside
the code you believed was guarded. Require a regular file explicitly, and note that "is a regular file"
predicates commonly follow symlinks, so a separate symlink check is still needed.

Writing safely into the same space — atomic publish, temporary-file lifecycle, pointer-last ordering — belongs
to [state-transactions](../state-transactions/SKILL.md).

## Never let external data select code

External data selects a **name**; you select the code.

**Never evaluate an external payload.** An allowlist checked before evaluation does not make evaluation safe,
because the evaluator is reached by the whole expression, not only the part the allowlist inspected. Any nested
form, any argument expression, and any object the reader constructs on the way are all live before the
allowlist's decision has any effect. Parse with the reader's evaluation hooks disabled, then match the operator
against exact supported names and call the corresponding function **directly**, so the set of reachable code is
fixed at compile time rather than derived from input.

**Exact names only, and nothing left over.** Prefix and substring matching turns an allowlist into a wildcard —
a check accepting a command because it *begins with* an allowed name accepts every longer name sharing that
prefix. Ignoring bytes after the parsed form lets an attacker append a second payload a differently-configured
consumer will read. Compare for exact equality, reject trailing input, and enforce arity before dispatching so
a mismatch is a rejection rather than a runtime error inside a handler that has already started work.

**Reject reader-constructed cycles.** Readers supporting internal references can be made to produce cyclic
structures, and any later traversal — validation, logging, comparison, serialization — then hangs or overflows
the stack, **inside code that looks total.** Reject cycles at the boundary with a cycle-safe traversal and
bound traversal depth: an unbounded recursive walk is a scale-dependent failure the test corpus will not reach.

**Documents must not configure their reader.** Many document and container formats include a mechanism by which
the document instructs the tool that opens it — embedded settings, in-band directives, in some ecosystems
arbitrary expressions attached to the file. Opening a file on behalf of external input therefore inherits code
execution from that file's author, **with no explicit call site to review.** When opening a path derived from
external input, disable in-band configuration and evaluation for the duration of the open, as part of the open
operation rather than as a global setting someone else might change.

**Persistence files are input, not policy.** A layout, bookmark, session, or preference file is user-editable,
often synchronized between machines, and restored automatically at startup with no user gesture. A file storing
an executable command string is therefore **a persistent, auto-triggering execution channel that never passed
through your protocol validation.** Persist a symbolic identifier resolved against a fixed table, reject unknown
identifiers on restore, and revalidate every restored value exactly as if it had arrived over the network.

**Locally configured extension points are still boundaries.** "The user configured it themselves, so it is
trusted" fails as soon as configuration is shared, generated, templated, inherited from a repository, or
written by another program. **The data coming back from a locally configured helper is not authored by the
person who configured it.**

## Output and logging

Untrusted data is still dangerous on the way out. The output channel has its own interpreter — a log pipeline,
a terminal, a formatter, a renderer — and untrusted bytes reach it directly through diagnostics written for
convenience.

**Never use untrusted text as a format template.** Formatting functions *interpret* their template argument.
Passing an already-composed message, or any attacker-influenced text, in the template position hands the
formatter's directive language to the attacker — which across ecosystems means crashes, resource exhaustion
through repeat directives, argument-stack reads, or writes into a stream. Always pass composed or untrusted
text as a formatting **argument** with a plain pass-through directive. This applies to error constructors and
logging helpers, **which are exactly where the mistake looks harmless.**

**Never interpolate raw untrusted data into a message.** An error message travels to places with different
interpreters: a log file, an aggregator that splits on newlines, a terminal that acts on escape sequences, a
notification area. Newline enables log forging; a NUL byte truncates the record in some consumers; escape
sequences can rewrite a terminal's state or spoof subsequent output; and **the message often crosses a
privilege level upward, being read by an operator.** Report a bounded, sanitized representation — escape or
strip control characters, cap the length, prefer a stable identifier or a hash over the content.

**Untrusted strings can carry presentation metadata.** In ecosystems where a string carries attached metadata —
display substitutions, styling, an attached key map — that metadata survives concatenation and formatting, is
preserved through ordinary substitution directives, and is **not** removed by applying an outer style. A value
from a lookup table, a cache, or a network response can therefore alter what the user sees, or what a keystroke
does, at a place in the code that appears to be doing nothing but string building. Generic deep-copy helpers
commonly treat strings as leaves and do not copy this metadata away. Take an explicitly metadata-free copy at
the presentation boundary. Where such a mechanism exists, this is a **rule**, with the same weight as escaping
in markup languages.

## External references

**Allowlist schemes; never denylist.** The set of schemes a platform can dereference is open-ended and grows
with installed software, and several read local files, launch programs, or perform privileged actions. **A
denylist is a list of the ones you thought of.** For links intended to open in a browser, the accepted set is
the two web schemes and nothing more.

**Require the full authority form.** Shortened, scheme-relative, and authority-less forms are re-resolved
differently by different consumers, so the reference you validated is not necessarily the one dereferenced.
Embedded credentials in the authority component are additionally a display-spoofing vector, because the
visually prominent part is then chosen by the attacker. Reject anything relative or abbreviated, and reject any
userinfo component **outright rather than stripping it.**

**Validate the host by class.** A single pattern intended to match "a hostname" inevitably admits forms it was
not designed for, because the host component has several genuinely different grammars — and the failures are
silent, producing a validator that is confidently wrong. Classify first, then validate within the class: a
dotted name label by label against the label grammar and length limits; a dotted-quad as **exactly four**
decimal components, each in range and **each without a leading zero**; a bracketed literal as an address of
that family. Reject anything fitting no class, and decide *separately* whether loopback, link-local, and
private ranges are permitted, since the class check alone accepts them.

Both extra conditions on the dotted-quad class exist because **the resolver is more permissive than the grammar
most validators assume.** A component with a leading zero is reinterpreted as octal by several resolvers, so a
validator that only range-checks approves one address while the connection is made to another. Forms with fewer
than four parts — a bare integer, or two or three parts where the last absorbs the remaining bytes — are
likewise still accepted, so "it did not look like an address" is not a safe conclusion from a failed four-part
match. **When a validator and a resolver disagree about a grammar, the resolver wins**, and the disagreement is
exactly where a private-range check gets bypassed.

**Reject before creating the affordance.** Validating at the moment of dereference, while still rendering the
clickable element, means the interface has already told the user this reference is legitimate. Their decision
to act was made against your endorsement, and the eventual refusal reads as a bug rather than a warning.
Validate before the overlay, link decoration, menu entry, or button exists; a rejected reference renders as
inert text.

**A host-supplied path cannot be vouched for.** When a protocol lets a peer name a local file for the software
to read and return, no wrapper type, canonicalization, or prefix check can establish that the requester is
entitled to that file's contents — **the entitlement question is not answerable from the path.** Decline the
capability rather than attempting to validate it, and document it as a deliberate refusal so it is not
reintroduced as a missing feature.

## Related

- [state-transactions](../state-transactions/SKILL.md) — how an authorized mutation is applied and persisted
- [test-integrity](../test-integrity/SKILL.md) — integrity of the rejection tests themselves
- [testing-patterns](../testing-patterns/SKILL.md) — the strategy the rejection-tests-first method plugs into
- [investigation-patterns](../investigation-patterns/SKILL.md) — tracing a suspected boundary to the failing input
- [core-patterns](../core-patterns/SKILL.md) — shared decision-criteria and escalation structures
- [typescript-ecosystem](../typescript-ecosystem/SKILL.md) — branded types and schema decoding
- [rust-ecosystem](../rust-ecosystem/SKILL.md) — newtypes carrying a validated value
- [common-lisp-ecosystem](../common-lisp-ecosystem/SKILL.md) — conditions, and standard-predicate character classes
- [emacs-ecosystem](../emacs-ecosystem/SKILL.md) — document-directed evaluation and string presentation metadata
