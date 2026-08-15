---
name: trust-boundaries
description: Use when untrusted input crosses a privilege boundary, such as parsing client or network requests, decoding payloads or deserialization, expanding archives, restoring persisted files, evaluating input via eval, validating paths, URLs, or schemes, or guarding against TOCTOU. Covers fail-closed validation, decode budgets, format-template injection, and log injection.
version: 2.1.0
---

<purpose>
  Provide the discipline for code that consumes input it does not control. The central
  question is never "is this value well-formed?" but "what authority does this value carry,
  and what must be recomputed rather than believed?". This skill states the reason a rule
  exists before the rule itself, because almost every hole in this area comes from a check
  that was written in the right place but ran in the wrong order, measured the wrong
  quantity, or trusted a value that had already been normalized past the point where the
  check could see the problem.
</purpose>

<scope>
  <focus>
    Authority derivation across a client/server or caller/callee trust boundary; validation
    ordering and validator domains; resource budgets on decoders and extractors; time-of-check
    to time-of-use hazards on filesystem reads; refusing to evaluate external payloads;
    interpolation of untrusted text into messages and rendered surfaces; and validation of
    external references such as URLs and hostnames.
  </focus>
  <defer_to skill="state-transactions">
    Ownership of mutable state, atomicity, rollback, durability, and three-state reads
    (present / absent / unknown). This skill decides whether a mutation is *authorized* and
    whether its inputs are *believable*; that skill decides how the mutation is *applied*
    and *persisted*.
  </defer_to>
  <defer_to skill="typescript-ecosystem">
    Language-level expression of these rules in TypeScript: branded types, schema decoding,
    and the concrete validation libraries.
  </defer_to>
  <defer_to skill="rust-ecosystem">
    Newtype and typed-boundary construction, and the borrow/ownership mechanics of holding a
    validated value.
  </defer_to>
  <defer_to skill="common-lisp-ecosystem">
    Condition-system mechanics and the specific standard-library predicates whose accepted
    character classes are wider than an ASCII-defined grammar.
  </defer_to>
  <defer_to skill="emacs-ecosystem">
    Editor-specific expressions of these rules: document-directed evaluation on file open,
    presentation metadata attached to strings, and the API-level file-type predicates.
  </defer_to>
  <unique_coverage>
    Evidence-versus-effect authority derivation; validation-ordering rules (validate raw
    before normalizing, revalidate after decoding, one owning normalization boundary plus a
    fail-closed helper); validator-domain derivation and non-finite guards; budgets enforced
    before allocation and the fact that a compressed-size cap does not bound decompression;
    identity-pinned reads; explicit-dispatch instead of evaluation; and reference allowlists.
  </unique_coverage>
</scope>

<concepts>
  <concept name="trust_boundary">The line at which data stops being produced by code you control. Every value crossing it is a *claim*, not a fact. A boundary is not only a network socket: a persisted file the user can edit, a byte stream produced by whatever program the user chose to run, an archive member, a plugin's return value, and an inter-language call all cross one.</concept>
  <concept name="evidence_vs_effect">An untrusted party may report *what happened to it*; it may never state *what should change*. The authoritative side accepts cause-specific evidence and derives the effect from its own rules. Accepting the effect directly is an authority hole even when the sender is fully authenticated — authentication answers "who", not "what may they assert".</concept>
  <concept name="fail_closed">When a check cannot be completed — ambiguous input, unknown key, exhausted budget, unverifiable identity, missing checksum — the outcome is refusal. A missing verification result is a failure, never a pass. The symptom of a fail-open design is that removing an input makes the code take the happy path.</concept>
  <concept name="revalidate_per_boundary">Validation performed on one side of a language, process, or module boundary does not protect the other side. Tests, stubs, alternative entry points, and future refactors all reach the inner code without passing the outer parser. Each boundary re-checks its own preconditions.</concept>
  <concept name="refusal_as_the_boundary">Sometimes the correct validation is to not offer the capability. When no wrapper can prove the property you need — for example, that a host-supplied path is one the requester is entitled to read — declining the whole feature is a legitimate and often the only sound answer.</concept>
</concepts>

<authority_derivation>
  <description>
    The rules governing which side of a boundary owns a decision. This section generalizes
    beyond any one protocol: it applies identically to scoring, pricing and discounts, quota
    and rate limits, progress and achievement state, and permission grants.
  </description>

  <principle name="accept_evidence_not_effect">
    <why>
      A command carrying a caller-chosen magnitude ("apply amount N", "grant tier T",
      "set remaining quota to Q") delegates the entire policy to the caller. Every mitigation,
      floor, ceiling, cooldown, and eligibility rule the authoritative side implements is
      bypassed, because the caller supplied the answer those rules were supposed to produce.
      Authenticating the caller does not help: it establishes identity, not entitlement to
      that magnitude.
    </why>
    <implication>
      Replace the generic magnitude command with a set of cause-specific evidence claims. The
      caller says *which cause occurred*, with the parameters only it can observe; the
      authoritative side looks up the base magnitude for that cause, applies its own
      modifiers and mitigations, clamps to its own floors and ceilings, and records the
      result. The wire protocol should have no representation for a raw effect at all — an
      unrepresentable request cannot be smuggled through.
    </implication>
    <examples>
      <item>Scoring: the client reports "objective X completed at time T"; the server owns the point value and the anti-replay window.</item>
      <item>Pricing: the client reports cart line identifiers and a coupon code; the server computes the price and the discount. It never receives a total.</item>
      <item>Rate limiting: the client never reports its remaining allowance; the limiter derives it from its own record of the caller's requests.</item>
      <item>Permissions: the client never sends its own role or capability set; the authorizer resolves them from the identity.</item>
    </examples>
  </principle>

  <principle name="identity_and_scope_on_every_mutation">
    <why>
      Even a correctly derived effect can be applied to the wrong object. If the mutation
      carries a target identifier but not the partition, tenant, session, or world the caller
      is actually attached to, an otherwise valid claim reaches state the caller has no
      relationship with.
    </why>
    <implication>
      Every mutation carries both the acting identity and the scope identity, and the
      authoritative side verifies that the target belongs to that scope before applying
      anything. Verify against the server's record of the caller's current scope, never
      against a scope value the caller supplied alongside the target.
    </implication>
  </principle>

  <principle name="reject_unknown_shapes">
    <why>
      A restore or update path that ignores unrecognized keys silently accepts whatever a
      future — or hostile — writer adds, and gives an attacker a stable channel to probe
      which keys the implementation actually consumes.
    </why>
    <implication>
      Unknown keys, extra positional elements, and unexpected variants are errors, not
      noise to be skipped. This is the structural half of fail-closed.
    </implication>
  </principle>

  <methodology name="rejection_tests_first">
    <description>
      Harden a boundary by writing the refusals before the acceptances. The acceptance path
      is what everyone tests by accident; the refusal path is what nobody notices has
      regressed.
    </description>
    <step order="1">Enumerate the hostile shapes for the boundary: absent field, wrong type, out-of-domain value, ambiguous duplicate, oversized payload, wrong scope, replayed identifier, adversarially chosen boundary value.</step>
    <step order="2">Write a rejection test for each, asserting on a *typed* rejection value — a named error variant or tagged result — rather than on a message string, so a reworded diagnostic does not silently turn a rejection test into a tautology.</step>
    <step order="3">Snapshot the typed rejection set. A change that removes a rejection is then visible in review as a deleted snapshot entry, not as an absence.</step>
    <step order="4">Only then implement or adjust the acceptance path.</step>
    <notes>
      <item>The integrity of these tests — what an assertion must pin down, and how a rejection test goes vacuous — is covered by test-integrity; apply it to every test written here.</item>
    </notes>
    <defer_to skill="test-integrity">
      Design of the rejection tests themselves: vacuity, assertion strength, and the rule against
      calling the subject under test to compute its own expected value. This methodology owns only
      the security-specific requirement — that a boundary gets its refusals written before its
      acceptances, with the typed rejection set snapshotted so a removed refusal is visible.
    </defer_to>
  </methodology>
</authority_derivation>

<input_validation>
  <description>
    Ordering and domain rules. Most validation holes in this category are not missing checks
    — the check exists, and is unreachable, or is measuring something other than what crosses
    the wire.
  </description>

  <principle name="validate_raw_before_normalizing_coercion">
    <why>
      When a guard exists to reject input class A, and a normalizing coercion maps A into
      class B, running the coercion first makes the guard unreachable: by the time it runs,
      no input is still in class A. The refactor that causes this — "normalize first, then
      validate, so the validator only handles one shape" — sounds strictly safer and is
      exactly backwards. The guard keeps passing its own tests because the fixtures were
      already in normal form.
    </why>
    <implication>
      Run class-discriminating checks against the original input, before any coercion that
      can erase the distinction the check depends on. Then normalize, then run the
      shape-and-range checks that require normal form. If a guard's stated purpose is to
      reject a class, its test suite must include an input of that class in its *original*
      form.
    </implication>
    <symptom>The guard's rejection test only passes when the fixture is constructed in the pre-coercion form; feeding it the same value through the public entry point accepts it.</symptom>
  </principle>

  <principle name="validate_before_and_after_decoding">
    <why>
      Decoders normalize and decoders preserve, and both behaviors defeat a check placed only
      on one side. A percent-decoder may fold encoded control characters into innocuous
      characters, so a check on the decoded string never sees the injection that was present
      in the encoded form; the same decoder may pass a malformed escape triplet through
      unchanged rather than rejecting it, so a check on the raw form that assumed
      well-formedness never sees it either.
    </why>
    <implication>
      Reject encoded control characters and malformed escape sequences in the *raw* form,
      decode, then validate the decoded value again for its own domain. Neither check
      substitutes for the other.
    </implication>
  </principle>

  <principle name="validate_in_the_encoding_actually_written">
    <why>
      A protocol framed by byte-level delimiters is attacked at the byte level, but validation
      is usually written over decoded text. When the transport is configured with an encoding
      that cannot represent some characters, the encoder substitutes replacement bytes — and
      a character that looked harmless as a codepoint can become the protocol's own
      terminator or separator byte on the wire. The codepoint-level check is then correct and
      irrelevant.
    </why>
    <implication>
      Perform wire-format validation against the bytes produced by the configured encoding,
      not against the codepoint sequence. Encode first, then check the encoded bytes for
      delimiters, control bytes, and length; reject there.
    </implication>
  </principle>

  <principle name="one_normalization_boundary_and_a_fail_closed_helper">
    <why>
      Ambiguity in a structured input — duplicate keys that differ only by case, a key present
      with no value, an empty value, a continuation-folded field — is resolved differently by
      each helper that parses it. When the authorization gate resolves the ambiguity one way
      and the consuming helper resolves it another, the value that was authorized is not the
      value that is used. This is the classic parameter-pollution bypass and it needs no
      exotic input.
    </why>
    <implication>
      Name exactly one boundary that owns normalization and let it reject ambiguity outright
      rather than picking a winner. Then, independently, make each downstream helper apply the
      same rejection rules to the fragments it parses. The helper's fail-closed behavior must
      not be justified by "the gate already checked" — a later caller will reach the helper
      directly.
    </implication>
  </principle>

  <principle name="validator_domain_matches_the_producer_range">
    <why>
      Validators get written against the example value in front of the author rather than
      against the producer's actual range: an integer validator over a quantity that is
      genuinely fractional, a positive-only validator over a value that legitimately reaches
      zero, a fixed-length validator over an identifier whose length changed. Over live
      requests this rejects valid traffic; over *persisted* data it is worse than having no
      validator at all, because it makes legitimate saved state permanently unloadable and
      the failure appears long after the deploy that caused it.
    </why>
    <implication>
      Derive the accepted set from the producer's real range, not from a sample. Treat
      tightening a validator that runs over persisted data as a breaking change requiring a
      migration path. Where a value is continuous, accept finite fractional values and
      constrain by range rather than by type.
    </implication>
    <notes>
      <item>The mirror-image trap: a standard-library character-class predicate usually accepts a wider class than the grammar you are implementing (decimal-digit predicates commonly accept non-ASCII digit forms). When the grammar defines an ASCII-only class, implement the predicate explicitly and cover fullwidth and non-Latin digit forms in regression tests.</item>
    </notes>
  </principle>

  <principle name="non_finite_and_out_of_domain_numeric_guards">
    <why>
      A non-finite value that reaches persistent state is unrecoverable and silent: every
      subsequent comparison against it is false, so the guards downstream neither fire nor
      report the value that defeated them. The boundary is the last place the number is still
      identifiable as input rather than as state.
    </why>
    <implication>
      Reject non-finite and out-of-domain numeric input at the boundary, and model three
      outcomes explicitly without collapsing the first two: invalid input (non-finite, wrong
      type, outside the producer's range) is *rejected*; in-domain but inert input (zero, or a
      direction that does not apply) is a *no-op that succeeds*; in-domain active input is
      *applied*. Collapsing rejection into the inert case converts a validation failure into a
      silent success.
    </implication>
    <defer_to skill="state-transactions">
      How an accepted quantity is then accumulated and compared against a budget, deadline, or
      completion threshold — including equality-comparison hazards and clock behavior. This
      principle covers only whether the number was believable when it arrived.
    </defer_to>
  </principle>

  <principle name="consume_single_use_credentials_last">
    <why>
      Burning a one-time code before every check has passed converts any malformed or
      mis-scoped attempt into a denial of service against the legitimate holder, and can be
      driven by an attacker who knows only the code's identifier.
    </why>
    <implication>
      Validate identity, scope, expiry, and payload fully; consume or invalidate the credential
      only on the success path. A failed attempt — wrong identifier, expired entry, malformed
      record — fails closed without burning the credential.
    </implication>
  </principle>
</input_validation>

<resource_budgets>
  <description>
    Budgets exist to bound work. A budget checked after the work has been done bounds nothing,
    and a budget measured on the input does not bound the output.
  </description>

  <principle name="enforce_limits_before_allocation">
    <why>
      The general shape of a resource-limit bypass is that the limit is enforced on the
      emitted result while the memory blowup happens in an intermediate that was already
      built. Declared dimensions in a payload header, a scale or exponent field, a repeat
      count, a denominator, or a requested buffer size all determine an allocation *before*
      any output exists to measure.
    </why>
    <implication>
      Validate header-declared dimensions and counts before asking the decoder for its
      required buffer size and before allocating it. Enforce numeric limits before raising to
      a power, factorizing, or expanding to a decimal representation. Truncate while rendering
      into a bounded destination rather than building an unbounded string and trimming it.
    </implication>
    <symptom>The limit constant appears in the code only near the return statement or near the emit call.</symptom>
  </principle>

  <principle name="compressed_size_does_not_bound_decompression">
    <why>
      A cap on the bytes received bounds only the transfer. A container that is well-formed,
      correctly signed, and contains exactly the one member you expected can still expand to
      many gigabytes, and post-extraction validation of the resulting file runs only after the
      disk is full.
    </why>
    <implication>
      Cap the *uncompressed* byte count and enforce it during extraction, aborting mid-stream
      when it is exceeded. Any declared uncompressed size in the container metadata is itself
      untrusted input: use it for early rejection only, and still count actual bytes written.
    </implication>
  </principle>

  <principle name="each_decoder_is_its_own_boundary">
    <why>
      When a payload is decoded on two sides — a fast native path and a fallback path in a
      different language, or a preview renderer and the real consumer — the cap installed on
      one path does not exist on the other. The second path is usually the one nobody
      reviewed, precisely because it was described as a fallback.
    </why>
    <implication>
      Every decode path carries its own budget, expressed in its own language, with its own
      test. Do not treat a shared upstream check as covering a second decoder.
    </implication>
  </principle>

  <pattern name="safe_single_member_extraction">
    <description>The full recipe for extracting one expected file from an untrusted archive.</description>
    <step order="1">List the members without extracting anything.</step>
    <step order="2">Require the listing to be an exact single-member list matching what you expect. More than one member, or a different name, is a rejection — not a reason to search the list.</step>
    <step order="3">Extract only that member, into a freshly created private temporary directory, never into a shared or predictable location.</step>
    <step order="4">Reject symbolic links, hard links, device entries, and any member whose path is absolute or contains upward traversal, before writing anything.</step>
    <step order="5">Enforce a hard cap on uncompressed bytes while writing, aborting the extraction when it is exceeded.</step>
    <step order="6">Require the entry to be a regular file, and verify its checksum or signature while it is still inside the private temporary directory. A *missing* checksum is a failure, not a skipped optional step.</step>
    <step order="7">Only after verification has succeeded, publish the artifact to its destination. Nothing that failed or skipped verification ever appears at the final location.</step>
    <notes>
      <item>Step 6 is where fail-open most often hides: code that verifies a checksum "if one is provided" grants an attacker the ability to remove verification by removing data.</item>
      <item>The order of steps 6 and 7 is load-bearing. Verifying after publishing leaves an unverified artifact readable at its final path for the width of the verification, and a consumer that watches for the file's appearance will read it there; a failed verification then has to be repaired by deletion rather than simply never having happened.</item>
    </notes>
  </pattern>

  <principle name="bound_retained_payloads_not_only_reports">
    <why>
      An error object that retains the offending input for diagnostics keeps the whole payload
      alive for as long as the error propagates, even when the rendered message is truncated.
      Truncation at the presentation layer bounds what you see, not what you hold.
    </why>
    <implication>
      Sanitize and bound the payload at construction time, when the condition or error value is
      initialized — not at the point where it is formatted.
    </implication>
  </principle>
</resource_budgets>

<toctou>
  <description>
    Time-of-check to time-of-use on filesystem reads. The governing insight is that a path is
    a name, not an object: every check performed against a path is a statement about whatever
    the name referred to at that instant.
  </description>

  <principle name="metadata_checks_are_early_rejection_only">
    <why>
      Checking size, type, and ownership by path and then opening the same path re-resolves the
      name. Between the two resolutions the entry can be replaced. The metadata check therefore
      has exactly one honest purpose: cheaply rejecting input that is already wrong, so you do
      not pay for the expensive path.
    </why>
    <implication>
      Never phrase a security property as "we checked the file first". Metadata checks reject
      early; they do not authorize the read.
    </implication>
  </principle>

  <principle name="pin_the_identity_then_read_the_pin">
    <why>
      To make a check meaningful, the checked object and the read object must be provably the
      same object, which a second path resolution cannot establish.
    </why>
    <implication>
      Pin the identity: hold an open descriptor for the duration where the platform allows it,
      or otherwise create a link to the checked entry inside a private directory you control
      and read only through that link, requiring the linked entry's identity to equal the
      pre-link identity. Where the platform genuinely cannot close the window, compare the full
      identity tuple — device, inode, size, modification time, and status-change time — before
      and after the read, and *document the residual window explicitly* rather than describing
      the result as safe. Device, inode, and size alone miss a same-size in-place rewrite.
    </implication>
    <notes>
      <item>Hardlink pinning is frequently unavailable: the private directory and the target sit on different devices, or the filesystem refuses hard links to that entry. The fallback is *not* to check the path and open it afterwards — that is the original hazard restored. Invert the order instead: open first with symlink-following disabled, then run every type, size, and ownership check against the open descriptor rather than against the name. The checks then describe the object you are already holding, so nothing can be substituted between deciding and reading.</item>
      <item>The same inversion is the right shape whenever a platform offers descriptor-relative operations: resolve once, then keep operating on the resolved handle instead of re-resolving the name at each step.</item>
    </notes>
  </principle>

  <principle name="bound_the_read_itself">
    <why>
      A pre-read size check does not bound the read, because the entry can grow between the
      stat and the read. A post-read size check bounds nothing, because the bytes are already
      in memory.
    </why>
    <implication>
      Read at most limit-plus-one bytes and reject when the extra byte materializes. This
      simultaneously enforces the cap and detects overflow with one read.
    </implication>
  </principle>

  <principle name="reject_by_type_because_some_types_block">
    <why>
      A named pipe is the sharpest case: it is readable, reports a size of zero, is not a
      symbolic link, and satisfies every metadata precheck — and then blocks forever on open,
      before any post-read size limit or identity comparison you carefully wrote gets a chance
      to run. The denial of service happens strictly inside the code you believed was guarded.
    </why>
    <implication>
      Require the entry to be a regular file explicitly, and be aware that "is a regular file"
      predicates commonly follow symbolic links, so a separate symlink check is still needed.
      Where the platform offers it, open with symlink-following and blocking disabled rather
      than testing beforehand.
    </implication>
  </principle>

  <defer_to skill="state-transactions">
    Writing safely into the same space — atomic publish, temporary-file lifecycle, and pointer-last
    ordering — belongs there. This section covers only the read side.
  </defer_to>
</toctou>

<code_execution>
  <description>
    Rules for anything that turns external data into behavior. The default is that external
    data never selects code; it selects a *name*, and you select the code.
  </description>

  <principle name="never_evaluate_an_external_payload">
    <why>
      An allowlist checked before evaluation does not make evaluation safe, because the
      evaluator is reached by the whole expression, not only by the part the allowlist
      inspected. Any nested form, any argument expression, and any object the reader
      constructs on the way are all live before your allowlist's decision has any effect.
    </why>
    <implication>
      Parse, then dispatch — never evaluate. Concretely: read the payload with the reader's
      evaluation hooks disabled so parsing cannot execute anything; then match the operator
      against exact supported names and call the corresponding function *directly*, so the
      set of reachable code is fixed at compile time rather than derived from the input.
    </implication>
  </principle>

  <principle name="exact_names_only_and_nothing_left_over">
    <why>
      Prefix and substring matching turns an allowlist into a wildcard: a check that accepts a
      command because it begins with an allowed name accepts every longer name sharing that
      prefix. Ignoring bytes after the parsed form lets an attacker append a second payload
      that a differently-configured consumer will read.
    </why>
    <implication>
      Compare names for exact equality. Reject any trailing input after the first complete
      form rather than discarding it. Enforce the expected argument count before dispatching,
      so an arity mismatch is a rejection rather than a runtime error inside a handler that
      has already started work.
    </implication>
  </principle>

  <principle name="reader_constructed_cycles">
    <why>
      Readers that support internal references can be made to produce cyclic structures. Any
      later traversal — validation, logging, comparison, serialization — then hangs or
      overflows the stack, and it does so inside code that looks total.
    </why>
    <implication>
      Reject reader-created circular objects at the boundary using a cycle-safe traversal, and
      bound traversal depth. Recursion over untrusted structure must be iterative or
      depth-limited: an unbounded recursive walk is a scale-dependent failure that the test
      corpus will not reach.
    </implication>
  </principle>

  <principle name="documents_must_not_configure_their_reader">
    <why>
      Many document and container formats include a mechanism by which the document instructs
      the tool that opens it — embedded settings, in-band directives, and in some ecosystems
      arbitrary expressions attached to the file. Opening a file on behalf of external input
      therefore inherits code execution from that file's author, with no explicit call site to
      review.
    </why>
    <implication>
      When opening a path derived from external input — a request, a link, an index entry, a
      search result — disable in-band configuration and any in-band evaluation for the
      duration of the open. Treat this as part of the open operation, not as a global setting
      someone else might change.
    </implication>
  </principle>

  <principle name="persistence_files_are_input_not_policy">
    <why>
      A layout, bookmark, session, or preference file is user-editable, is often synchronized
      between machines, and is restored automatically at startup with no user gesture. A file
      that stores an executable command string is therefore a persistent, auto-triggering
      execution channel that never passed through your protocol validation.
    </why>
    <implication>
      Never persist or restore a raw executable command string. Persist a symbolic identifier
      that the restoring code resolves against a fixed table, reject unknown keys and unknown
      identifiers on restore, and revalidate every restored value exactly as if it had arrived
      over the network.
    </implication>
  </principle>

  <principle name="locally_configured_extension_points_are_still_boundaries">
    <why>
      "The user configured it themselves, so it is trusted" fails as soon as configuration is
      shared, generated, templated, inherited from a repository, or written by another program.
      The data that comes back from a locally configured helper is not authored by the person
      who configured it.
    </why>
    <implication>
      Normalize and validate output from user-configured extension points, helpers, and
      subprocesses with the same rules applied to network input.
    </implication>
  </principle>
</code_execution>

<output_and_logging>
  <description>
    Untrusted data is still dangerous on the way out. The output channel has its own
    interpreter — a log pipeline, a terminal, a formatter, a renderer — and untrusted bytes
    reach it directly through diagnostics that were written for convenience.
  </description>

  <principle name="never_use_untrusted_text_as_a_format_template">
    <why>
      Formatting functions interpret their template argument. Passing an already-composed
      message, or any attacker-influenced text, in the template position hands the formatter's
      directive language to the attacker — which in various ecosystems means crashes, resource
      exhaustion through repeat directives, argument-stack reads, or writes into a stream.
    </why>
    <implication>
      Always pass composed or untrusted text as a formatting *argument* with a plain
      pass-through directive, never as the template. This applies to error constructors and
      logging helpers, which are exactly where the mistake looks harmless.
    </implication>
  </principle>

  <principle name="never_interpolate_raw_untrusted_data_into_messages">
    <why>
      An error message travels to places with different interpreters: a log file, a log
      aggregator that splits on newlines, a terminal that acts on escape sequences, a
      notification area. Newline enables log forging; a NUL byte truncates the record in some
      consumers; control and escape sequences can rewrite a terminal's state or spoof
      subsequent output; and the message often crosses a privilege level upward, being read by
      an operator.
    </why>
    <implication>
      Do not interpolate the offending key, value, or payload into a diagnostic. Report a
      bounded, sanitized representation: escape or strip control characters, cap the length,
      and prefer a stable identifier or a hash over the content itself.
    </implication>
  </principle>

  <principle name="untrusted_strings_carry_presentation_metadata">
    <why>
      In ecosystems where a string can carry attached metadata — display substitutions, styling,
      or an attached key map — that metadata survives concatenation and formatting, is
      preserved through ordinary substitution directives, and is *not* removed by applying an
      outer style. A value that arrived from a lookup table, a cache, or a network response can therefore
      alter what the user sees, or what a keystroke does, at a place in the code that appears
      to be doing nothing but string building. The related trap is that generic deep-copy
      helpers commonly treat strings as leaves and do not copy this metadata away.
    </why>
    <implication>
      Take an explicitly metadata-free copy of untrusted text at the presentation boundary,
      keeping any semantic metadata on the original object rather than on the rendered copy.
      Where such a mechanism exists in the target language, this is a *rule*, not a
      nicety — treat it as an injection channel with the same weight as escaping in markup
      languages.
    </implication>
  </principle>
</output_and_logging>

<external_references>
  <description>
    Validation of references — links, callback targets, resource locators — that the software
    will later dereference or offer to the user.
  </description>

  <principle name="allowlist_schemes_never_denylist">
    <why>
      The set of schemes a platform can dereference is open-ended and grows with installed
      software, and several of them read local files, launch programs, or perform privileged
      actions. A denylist is a list of the ones you thought of.
    </why>
    <implication>
      Accept an explicit, small set of schemes and reject everything else. For links intended
      to open in a browser, that set is the two web schemes and nothing more.
    </implication>
  </principle>

  <principle name="require_the_full_authority_form">
    <why>
      Shortened, scheme-relative, and authority-less forms are re-resolved differently by
      different consumers, so the reference you validated is not necessarily the reference that
      gets dereferenced. Embedded credentials in the authority component are additionally a
      display-spoofing vector, because the visually prominent part of the reference is then
      chosen by the attacker.
    </why>
    <implication>
      Require the complete authority form and reject anything relative or abbreviated. Reject
      any reference containing a userinfo component outright rather than stripping it.
    </implication>
  </principle>

  <principle name="validate_the_host_by_class">
    <why>
      A single pattern intended to match "a hostname" inevitably admits forms it was not
      designed for, because the host component has several genuinely different grammars.
      Pattern-matching failures here are silent and produce a validator that is confidently
      wrong.
    </why>
    <implication>
      Classify first, then validate within the class: a dotted name is validated label by
      label against the label grammar and length limits; a dotted-quad literal is validated as
      *exactly four* decimal components, each in range and each written without a leading zero;
      a bracketed literal is validated as an address of that family. Reject anything that fits
      no class. Decide explicitly and separately whether loopback, link-local, and private
      ranges are permitted, since the class check alone accepts them.
    </implication>
    <notes>
      <item>Both extra conditions on the dotted-quad class exist because the resolver is more permissive than the grammar most validators assume. A component with a leading zero is reinterpreted as octal by several resolvers, so a validator that only range-checks each component approves one address while the connection is made to another. Forms with fewer than four parts — a bare integer, or two or three parts where the last absorbs the remaining bytes — are likewise still accepted by some resolvers, so "it did not look like an address" is not a safe conclusion from a failed four-part match.</item>
      <item>The general rule this instance illustrates: when a validator and a resolver disagree about a grammar, the resolver wins, and the disagreement is exactly where a range or private-range check gets bypassed.</item>
    </notes>
  </principle>

  <principle name="reject_before_creating_the_affordance">
    <why>
      Validating at the moment of dereference, while still rendering the clickable or
      actionable element, means the interface has already told the user this reference is
      legitimate and actionable. The user's decision to act on it was made against your
      endorsement, and the eventual refusal reads as a bug rather than as a warning.
    </why>
    <implication>
      Perform validation before creating any affordance — before the overlay, the link
      decoration, the menu entry, or the button exists. A rejected reference renders as inert
      text.
    </implication>
  </principle>

  <principle name="a_host_supplied_path_cannot_be_vouched_for">
    <why>
      When a protocol lets a peer name a local file for the software to read and return, no
      wrapper type, canonicalization, or prefix check can establish that the requester is
      entitled to that file's contents — the entitlement question is not answerable from the
      path.
    </why>
    <implication>
      Decline the capability rather than attempting to validate it. Removing a feature is a
      legitimate security fix and is usually the only complete one; document it as a
      deliberate refusal so it is not reintroduced as a missing feature.
    </implication>
  </principle>
</external_references>

<anti_patterns>
  <avoid name="accepting_a_caller_supplied_magnitude">
    <description>A command whose payload contains the resulting amount, score, price, tier, or quota.</description>
    <instead>Accept a cause-specific evidence claim and derive the magnitude from server-owned rules. Remove the raw-effect shape from the protocol entirely.</instead>
  </avoid>
  <avoid name="normalize_then_validate">
    <description>Coercing input to a canonical form before running the guard that was supposed to reject a non-canonical class.</description>
    <instead>Run class-discriminating checks on the original input; normalize afterwards.</instead>
  </avoid>
  <avoid name="validating_only_the_decoded_form">
    <description>Checking a value after percent-, entity-, or escape-decoding and assuming the raw form was equivalent.</description>
    <instead>Reject malformed and control-bearing raw forms first, then decode, then validate again.</instead>
  </avoid>
  <avoid name="cap_on_the_wrong_quantity">
    <description>Bounding the transferred or compressed size, the declared size, or the emitted output, when the risk is the intermediate allocation.</description>
    <instead>Bound the actual bytes produced, enforced during production, and validate declared sizes before allocating from them.</instead>
  </avoid>
  <avoid name="optional_integrity_verification">
    <description>Verifying a checksum or signature only when one is present.</description>
    <instead>Treat a missing verification input as a failure. Absence must not be a bypass.</instead>
  </avoid>
  <avoid name="check_the_path_then_open_the_path">
    <description>Validating file metadata by name and then re-opening the same name to read.</description>
    <instead>Pin the identity and read the pin, verifying identity across the read and bounding the read at limit-plus-one bytes.</instead>
  </avoid>
  <avoid name="allowlist_in_front_of_eval">
    <description>Checking a command name against an allowlist and then evaluating the payload.</description>
    <instead>Parse with evaluation disabled and dispatch to a fixed function by exact name.</instead>
  </avoid>
  <avoid name="prefix_matching_an_allowlist">
    <description>Accepting a name because it starts with an allowed name.</description>
    <instead>Exact equality only; also reject trailing input and enforce arity before dispatch.</instead>
  </avoid>
  <avoid name="untrusted_text_in_the_template_position">
    <description>Passing a composed or external message as a formatter's template, or interpolating raw payload bytes into an error.</description>
    <instead>Pass untrusted text as a formatting argument, bounded and control-character-sanitized.</instead>
  </avoid>
  <avoid name="rendering_then_validating_a_reference">
    <description>Creating a clickable or actionable element and validating the target only when the user acts on it.</description>
    <instead>Validate before the affordance exists; render rejected references as inert text.</instead>
  </avoid>
  <avoid name="trusting_the_inner_layer_to_be_unreachable">
    <description>Omitting checks in a helper because an outer gate already validated, on the assumption the helper has one caller.</description>
    <instead>Fail closed at each boundary independently; tests, stubs, and future callers all bypass the outer gate.</instead>
  </avoid>
</anti_patterns>

<best_practices>
  <practice priority="critical">Accept evidence, never effects: an untrusted party reports a cause; the authoritative side derives the magnitude, the mitigation, and the bounds.</practice>
  <practice priority="critical">Enforce every budget before the allocation it is meant to bound, and count actual produced bytes rather than declared or compressed sizes.</practice>
  <practice priority="critical">Never evaluate an external payload. Parse with evaluation disabled, then dispatch by exact name to a fixed function.</practice>
  <practice priority="critical">Fail closed: unknown key, ambiguous input, missing checksum, unverifiable identity, and exhausted budget are all refusals.</practice>
  <practice priority="high">Order validation so class-discriminating checks precede any normalizing coercion, and revalidate after decoding.</practice>
  <practice priority="high">Treat every language, process, and module boundary as its own trust boundary with its own checks and its own budget.</practice>
  <practice priority="high">Pin file identity across a read; use metadata checks only for early rejection; require a regular file explicitly and bound the read at limit-plus-one.</practice>
  <practice priority="high">Derive validator domains from the producer's real range, and treat tightening a validator over persisted data as a breaking change.</practice>
  <practice priority="high">Write rejection tests first and assert on typed rejection values, snapshotted so a removed refusal is visible in review.</practice>
  <practice priority="medium">Sanitize and bound untrusted data at error-construction time, not at formatting time; never interpolate it raw into a diagnostic.</practice>
  <practice priority="medium">Allowlist reference schemes, require the full authority form, reject userinfo, and validate the host by class.</practice>
  <practice priority="medium">Prefer declining a capability over validating an unprovable property; record the refusal as a deliberate decision.</practice>
</best_practices>

<related_skills>
  <skill name="state-transactions">Ownership, atomicity, rollback, durability, and three-state reads for the mutation this skill authorized</skill>
  <skill name="test-integrity">Design and integrity of the rejection tests themselves — vacuity, assertion strength, and not computing an expected value from the subject under test</skill>
  <skill name="testing-patterns">General test strategy that the rejection-tests-first methodology plugs into</skill>
  <skill name="investigation-patterns">Evidence-based tracing when a boundary is suspected but the failing input is unknown</skill>
  <skill name="core-patterns">Shared error-escalation and decision-criteria templates</skill>
  <skill name="typescript-ecosystem">Branded types and schema decoding as the language-level expression of a validated boundary</skill>
  <skill name="rust-ecosystem">Newtypes and typed boundary contracts for carrying a validated value</skill>
  <skill name="common-lisp-ecosystem">Condition construction and standard-predicate character-class traps</skill>
  <skill name="emacs-ecosystem">Editor-specific expression of document-directed evaluation, string presentation metadata, and file-type predicates</skill>
</related_skills>

<related_agents>
  <agent name="security">Vulnerability detection and remediation across the boundaries this skill defines</agent>
  <agent name="quality-assurance">Review of validation ordering, fail-closed behavior, and budget placement</agent>
  <agent name="test">Design of the rejection-test suite and typed rejection snapshots</agent>
  <agent name="explore">Locate every entry point that consumes external input before hardening any one of them</agent>
</related_agents>

<constraints>
  <must>Derive effects from evidence on the authoritative side; never apply a caller-supplied magnitude</must>
  <must>Enforce limits before allocation and count produced bytes, not declared or compressed ones</must>
  <must>Fail closed on ambiguity, unknown shapes, and missing verification inputs</must>
  <must>Validate before normalizing coercions, and revalidate after decoding</must>
  <must>Re-check preconditions at each language, process, and module boundary</must>
  <avoid>Evaluating external payloads, even behind an allowlist</avoid>
  <avoid>Re-opening a path after checking it, or trusting a pre-read size check</avoid>
  <avoid>Interpolating untrusted data into format templates, diagnostics, or rendered surfaces</avoid>
  <avoid>Denylisting schemes, prefix-matching allowlists, or validating a reference after rendering it</avoid>
</constraints>
