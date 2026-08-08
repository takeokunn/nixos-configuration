---
name: llm-prompt-architecture
description: This skill should be used when structuring the source code of an LLM-driven feature - splitting prompt builders, static prompt data, and response parsers into separate modules; replacing inline template-literal prompt blobs with typed data definitions plus a formatter; sharing one output-contract constant between the prompt that requests a JSON shape and the parser that validates it; stripping code fences and rejecting malformed model responses; or deciding what to assert at each prompt layer. The subject is source structure and testability, never prompt wording, so do not load this for requests to improve, reword, shorten, or tune the text of a prompt, to pick a model, or to fix model behavior. Triggers include prompt module, prompt fragment, prompt builder, prompt data, output contract, response parser, fence stripping, LLM JSON output, table-driven parser tests, and "this prompt can only be tested by calling the model".
version: 2.2.0
---

<purpose>
  Provide a module architecture for the code that builds prompts and consumes model
  responses, so that prompt text is diffable, prompt selection is testable without a model
  call, and the requested output shape cannot drift away from the validator that enforces
  it. The focus is on the "why" of each boundary (how a prompt becomes untestable, how an
  output contract silently diverges, why a lenient parser turns a client bug into missing
  data) followed by the smallest code that demonstrates the fix.
</purpose>

<scope>
  <focus>
    Source structure of prompt-producing and response-consuming code: the four-layer
    separation of prompt data, fragment renderers, composition-only builders, and response
    parsers; the output contract as a shared constant; and the per-layer testing
    consequences of that separation.
  </focus>
  <explicitly_not>
    This skill is NOT a prompt-writing guide. It says nothing about how to phrase
    instructions, which persona to adopt, few-shot example selection, chain-of-thought
    prompting, context-window budgeting, or persuasion technique. The subject is how to
    organize the code that assembles prompts and parses responses, so prompts are testable,
    diffable, and refactorable like any other source. Wording quality is orthogonal and out
    of scope; every rule here holds regardless of what the prompt says.
  </explicitly_not>
  <defer_to skill="typescript-ecosystem">
    Language mechanics: compiler configuration, generics, utility types, module resolution,
    schema-as-single-source-of-truth in general, and discriminated-union typing. This skill
    uses those mechanics but does not re-teach them.
  </defer_to>
  <defer_to skill="testing-patterns">
    General test design: coverage policy, fixture and factory discipline, test doubles,
    and assertion granularity. This skill covers only which layer each assertion belongs to
    once the prompt code has been split.
  </defer_to>
  <unique_coverage>
    The four prompt layers and their forbidden dependencies; typed variant dictionaries with
    a formatter instead of template-literal blobs; structured-to-string mapping at the module
    boundary; the output contract as one named constant shared by prompt and parser; parser
    isolation with fence stripping and strict optional-field semantics; per-layer test
    targets.
  </unique_coverage>
</scope>

<tools>
  <tool>Read - Inspect prompt builders, prompt data modules, parsers, and their tests</tool>
  <tool>Grep - Locate template literals inside orchestration code and duplicated schema text</tool>
  <tool>Edit - Apply the layer extraction incrementally</tool>
  <tool>Bash - Run the type checker and the test suite after each extraction</tool>
</tools>

<concepts>
  <concept name="prompt_as_code">
    A prompt is program output, not a configuration string. It is assembled by a function
    from data and runtime arguments, and therefore has all the properties of any other
    generated artifact: it can be unit tested, reviewed as a diff, and refactored - but only
    if the assembly is decomposed. A prompt that exists as one template literal inside the
    function that calls the model has none of those properties.
  </concept>
  <concept name="output_contract">
    The output contract is the description of the response shape that the prompt instructs
    the model to produce - typically a JSON schema written as prose or as an example object.
    It is one decision with two consumers: the prompt that requests it and the parser that
    validates it. Whenever those two consumers hold independent copies, the contract is a
    latent bug.
  </concept>
  <concept name="four_layers">
    Prompt code separates into four layers with a strict dependency direction: static prompt
    data, pure fragment renderers, composition-only builders, and the response parser.
    Orchestration - the code that actually calls the model - sits above all four and contains
    no prose and no parsing.
  </concept>
  <concept name="untrusted_response">
    Model output is untrusted input from a nondeterministic source. Unlike an internal
    function's return value it carries no compile-time guarantee, so the code that consumes
    it is a parser in the full sense: malformed input is an expected case on the normal path,
    not an exceptional one.
  </concept>
</concepts>

<layering>
  <description>
    Four layers, each with one responsibility and an explicit set of imports it must not
    have. The dependency direction is strictly downward: data knows nothing, fragments know
    data, builders know fragments, orchestration knows builders and the parser. The parser
    sits beside the builders and shares only the output contract with them.
  </description>

  <layer name="prompt_data" order="1">
    <responsibility>
      Hold every literal that a human would edit for wording: instruction prose, checklist
      and procedure text, variant dictionaries, labels, thresholds, and the output-contract
      text. Values only - no functions that do work.
    </responsibility>
    <forbidden_dependency>
      Imports nothing from the fragment, builder, parser, or orchestration layers. A prompt
      data module that imports a formatter has become a fragment module.
    </forbidden_dependency>
    <why>
      A wording change is the single most frequent change to prompt code and the one least
      likely to need review of executable logic. Isolating literals means a copy edit
      produces a diff that touches only inert values, which a reviewer can read as prose
      rather than as code.
    </why>
  </layer>

  <layer name="fragments" order="2">
    <responsibility>
      Pure functions that render a piece of the prompt: data plus arguments in, string out.
      One fragment per reusable section - the shared preamble, the context block, the
      enumerated-items block, the output-contract block.
    </responsibility>
    <forbidden_dependency>
      Must not import builders, the parser, the model client, or any I/O. A fragment that
      reads a file or awaits anything is doing context acquisition, which belongs above it.
    </forbidden_dependency>
    <why>
      A fragment is the smallest unit whose exact output a test can assert. Once a section is
      a named function, a test pins that one section's text without re-validating the whole
      prompt, so an unrelated wording change elsewhere cannot break it.
    </why>
  </layer>

  <layer name="builders" order="3">
    <responsibility>
      Composition only: choose which fragments apply to this request, call them, and join the
      results in order. A builder is a short function whose body is a list of fragment calls.
    </responsibility>
    <forbidden_dependency>
      Must contain no literal prose of its own, and must not parse anything. Joining
      separators and section ordering are the only strings a builder may own.
    </forbidden_dependency>
    <why>
      Selection and interpolation are two different decisions with two different failure
      modes. Keeping them in one function makes the prompt untestable, for a reason worth
      stating precisely - see below.
    </why>
  </layer>

  <layer name="parser" order="4">
    <responsibility>
      Turn a raw model response into a validated domain value or a failure: strip fences,
      parse JSON, validate the shape against the output contract.
    </responsibility>
    <forbidden_dependency>
      Must not import builders or the model client. It imports the shared output contract and
      nothing else from the prompt side, so it can be tested with hand-written strings and
      never needs a prompt to exist.
    </forbidden_dependency>
    <why>
      A parser reachable only through a model call can only be tested through a model call.
      Isolating it turns "what happens when the model returns a trailing comma" from an
      integration question into a one-line unit test.
    </why>
  </layer>

  <principle name="why_mixing_composition_and_rendering_kills_testability">
    <why>
      When one function both selects sections and interpolates their text, its only
      observable is the final multi-hundred-line string. Every assertion must therefore be a
      substring match against that blob. That has three compounding consequences. First, the
      assertions are weak: `toContain` over a long string cannot distinguish "the section is
      present in the right place" from "those words happen to appear somewhere". Second, the
      test surface is multiplicative rather than additive - with S selectable sections and W
      wording variants you must exercise combinations to reach any one branch, because you
      cannot reach the branch without also rendering everything around it. Third, the tests
      are brittle in the wrong direction: a pure copy edit breaks tests that were written to
      check selection logic, so the team learns to loosen the assertions until they check
      nothing.
    </why>
    <implication>
      Split until each layer has an observable of its own. Fragment tests assert exact text
      and are the only tests that mention wording. Builder tests assert which fragments
      appear and in what order, by calling the fragment functions and comparing - never by
      re-typing the expected prose. A wording change then updates exactly one fragment test,
      and a selection change updates exactly one builder test.
    </implication>
  </principle>

  <example>
    // BEFORE: one function owns selection, wording, the output contract, and parsing.
    async function summarize(doc: Doc, mode: Mode) {
      const prompt = `You are summarizing a document.
    ${mode === "brief" ? "Keep it under three sentences." : "Cover every section."}
    Document:
    ${doc.body}
    Reply with JSON: { "summary": string, "topics": string[] }`
      const raw = await model.complete(prompt)
      return JSON.parse(raw.replace(/```json|```/g, ""))
    }

    // AFTER: four modules, each independently assertable.
    // summary.data.ts       - MODE_INSTRUCTIONS, SUMMARY_OUTPUT_CONTRACT
    // summary.fragments.ts  - renderModeInstruction(), renderDocument(), renderContract()
    // summary.prompts.ts    - buildSummaryPrompt(doc, mode): composition only
    // summary.parsers.ts    - parseSummaryResponse(raw): fences, JSON, shape
    async function summarize(doc: Doc, mode: Mode) {
      const raw = await model.complete(buildSummaryPrompt(doc, mode))
      return parseSummaryResponse(raw)
    }
  </example>
</layering>

<static_data>
  <description>
    How to represent the literals in the data layer: typed definitions plus a formatter,
    rather than a dictionary of pre-rendered template literals.
  </description>

  <principle name="typed_definitions_plus_formatter">
    <why>
      A variant dictionary written as template-literal blobs duplicates the shared skeleton
      once per variant. The parts that actually differ between variants - a tone, a length
      limit, a list of required points - are buried inside repeated surrounding prose, so
      neither a reader nor a diff can see what distinguishes one variant from another, and a
      change to the shared skeleton must be applied N times with N chances to miss one.
      Representing each variant as a typed record makes the varying axes explicit fields, and
      a single formatter owns the skeleton.
    </why>
    <implication>
      Declare the variants as data with a named type, and render them with one small
      function. The type then also enforces that a newly added variant supplies every axis -
      an omission the template-literal form cannot detect.
    </implication>
    <example>
      // summary.data.ts - the varying axes are fields, not buried prose.
      export type ToneProfile = {
        readonly id: string
        readonly directive: string
        readonly mustCover: readonly string[]
      }

      export const TONE_PROFILES: readonly ToneProfile[] = [
        { id: "brief",    directive: "Answer in at most three sentences.",
          mustCover: ["the main claim"] },
        { id: "thorough", directive: "Address every section in order.",
          mustCover: ["the main claim", "supporting detail", "open questions"] },
      ] as const

      // summary.fragments.ts - one formatter owns the shared skeleton.
      export const renderTone = (profile: ToneProfile): string =&gt;
        [profile.directive, ...profile.mustCover.map((c) =&gt; `- cover ${c}`)].join("\n")
    </example>
  </principle>

  <principle name="map_to_strings_at_the_module_boundary">
    <why>
      Converting a prompt module to structured data is a refactor of its internals, and it
      should not be observable to callers. If the structured records leak out as the public
      export, every consumer and every consumer's test must be rewritten, which makes the
      refactor expensive enough that it does not happen. Keeping the structured-to-string
      mapping inside the module - so the exported symbol still has the shape it always had -
      makes the change local and reversible.
    </why>
    <implication>
      Export the rendered string (or the same function signature) that consumers already
      depend on; keep the typed records internal or export them additionally, never instead.
      Public exports stay stable when wording, and even the representation of wording,
      changes.
    </implication>
    <example>
      // Internal representation changed; the public export did not.
      const PROFILE_BY_ID = new Map(TONE_PROFILES.map((p) =&gt; [p.id, p]))

      export const toneInstruction = (id: string): string =&gt; {
        const profile = PROFILE_BY_ID.get(id)
        return profile === undefined ? "" : renderTone(profile)
      }
    </example>
  </principle>

  <principle name="immutable_static_prompt_data">
    <why>
      Static prompt data is shared, module-level, and read by every request. A mutable array
      or object there is a cross-request mutation hazard with no upside, since nothing
      legitimately edits prompt copy at runtime. The type-level detail matters too: an
      `as const` literal has a readonly tuple type, and a formatter whose parameter is
      `string[]` will reject it. The two ways out of that error are both bad - copy the array
      on every call, or drop `as const` and lose the literal types that made the data
      self-documenting.
    </why>
    <implication>
      Declare static prompt data with `readonly` element types and `as const`, and type every
      formatter parameter as `readonly string[]` (or `readonly T[]`). Immutable literals then
      flow through without widening and without defensive copies. `readonly string[]` is the
      strictly more permissive parameter type - a mutable array is assignable to it, so
      accepting `readonly` never costs a caller anything.
    </implication>
    <example>
      // Accepts both `readonly string[]` literals and ordinary arrays.
      export const renderChecklist = (items: readonly string[]): string =&gt;
        items.map((item, i) =&gt; `${i + 1}. ${item}`).join("\n")
    </example>
  </principle>
</static_data>

<output_contract>
  <description>
    The response shape the prompt asks for, and the shape the parser accepts, are one
    decision. Give it one name.
  </description>

  <principle name="one_named_constant_for_the_contract">
    <why>
      When the instruction text and the validator hold independent copies of the shape, they
      drift the first time either changes, and the drift is not caught by the type checker
      because one copy is prose. Both drift directions are bad and only one is loud. If the
      prompt is updated to request a new field and the validator is not, the field arrives
      and is discarded - the feature appears to do nothing, with no error anywhere. If the
      validator is tightened and the prompt is not, every response fails validation and the
      feature breaks completely, which at least announces itself. The quiet failure is the
      common one, because prompts are edited far more often than parsers.
    </why>
    <implication>
      Define the contract once in the data layer as a named export. The fragment that renders
      the instruction imports it, and the parser imports it. They cannot diverge because
      there is nothing to diverge from. Where the stack allows it, go one step further and
      make the machine-readable schema the source, rendering the instruction text from the
      schema; then adding a field to the schema updates the prompt automatically. Where it
      does not, at minimum co-locate the schema and its prose description in one module so
      that editing one puts the other in the same diff.
    </implication>
    <example>
      // summary.data.ts - one contract, two consumers.
      export const SUMMARY_SCHEMA = z.object({
        summary: z.string(),
        topics: z.array(z.string()),
        confidence: z.number().optional(),
      })
      export const SUMMARY_CONTRACT_TEXT = [
        "Reply with a single JSON object and no other text:",
        '{ "summary": string, "topics": string[], "confidence"?: number }',
      ].join("\n")

      // summary.fragments.ts
      export const renderContract = (): string =&gt; SUMMARY_CONTRACT_TEXT
      // summary.parsers.ts
      const validated = SUMMARY_SCHEMA.parse(candidate)
    </example>
    <notes>
      <item>
        A contract-drift test is cheap insurance where the text cannot be generated from the
        schema: assert that every field name in the schema appears in the contract text.
        It catches the quiet direction - a schema field the prompt never asks for.
      </item>
    </notes>
  </principle>
</output_contract>

<response_parsing>
  <description>
    The parser is its own module with its own tests, and it treats the model as what it is:
    an untrusted producer whose output is frequently not quite what was requested.
  </description>

  <principle name="parser_separate_from_orchestration">
    <why>
      Orchestration - build the prompt, call the model, hand off the response - is thin and
      hard to test because it needs a model client. Parsing is dense and trivially testable
      because it needs only a string. Fusing them imports the hard-to-test dependency into
      the easy-to-test logic, so the parsing branches end up covered, if at all, through a
      stubbed model call that must be re-stubbed for every malformed-input case.
    </why>
    <implication>
      Put parsing and structural validation in a dedicated module that takes a string and
      returns a validated value or a failure. Orchestration becomes two calls with no
      branching of its own, and every parser case is a plain function call in a table.
    </implication>
  </principle>

  <principle name="fence_stripping_is_its_own_step">
    <why>
      Models routinely wrap JSON in code fences, sometimes tag the fence with a language,
      and sometimes add a sentence before or after it. Those are three distinct extraction
      problems, and they are distinct again from JSON syntax errors and from schema
      violations. Collapsing them into one try/catch produces a single opaque "bad response"
      error, so nobody can tell whether the model chattered, emitted broken JSON, or returned
      a well-formed object of the wrong shape - which are three different fixes, in the
      prompt, in the retry policy, and in the contract respectively.
    </why>
    <implication>
      Layer the parser as extract, then parse, then validate, and make each stage's failure
      distinguishable in the returned error. Keep extraction tolerant (accept fenced,
      tagged, and bare payloads, and prose around the object) while keeping validation
      strict - the tolerance belongs at the transport-shaped stage, not the shape stage.
    </implication>
    <example>
      const stripFences = (raw: string): string =&gt; {
        const fenced = /```(?:[a-z]+)?\s*([\s\S]*?)```/i.exec(raw)
        return (fenced?.[1] ?? raw).trim()
      }

      export const parseSummaryResponse = (raw: string): Result&lt;Summary&gt; =&gt; {
        const payload = stripFences(raw)
        let candidate: unknown
        try {
          candidate = JSON.parse(payload)
        } catch {
          return { ok: false, stage: "syntax" }   // distinguishable from "shape"
        }
        const result = SUMMARY_SCHEMA.safeParse(candidate)
        return result.success
          ? { ok: true, value: result.data }
          : { ok: false, stage: "shape", issues: result.error.issues }
      }
    </example>
  </principle>

  <principle name="malformed_input_is_the_normal_case">
    <why>
      A parser over model output is not defensive programming against an impossible event.
      Truncation at a token limit, an unrequested preamble, a trailing comma, a number
      returned as a string, and an enum value that is a near-synonym of an allowed one are
      all routine. Code written on the assumption that the happy path is the overwhelming
      case gets the ergonomics backwards - it throws from deep inside and leaves the caller
      with no way to distinguish retryable from permanent failure.
    </why>
    <implication>
      Model failure as a value in the parser's return type, or throw a typed error - either
      is fine, but be consistent, and make the failure carry enough detail for the caller to
      decide between retrying, degrading, and surfacing. Then enumerate the malformed cases
      in a table-driven test rather than adding them one at a time as production incidents
      reveal them.
    </implication>
  </principle>

  <principle name="present_but_wrong_type_is_fatal">
    <why>
      "Optional" conflates two situations that deserve opposite handling: the field is
      absent, which the contract permits, and the field is present with the wrong type, which
      means the producer misunderstood the contract. The lenient reading - drop the field and
      continue - converts a producer bug into silent data loss, and it is worst exactly where
      it is most tempting: in list processing, where skipping malformed items yields a
      plausible-looking short result that no downstream check can distinguish from a
      genuinely short one.
    </why>
    <implication>
      Absence is valid; malformed presence is an error. A parser must not omit an optional
      field it could not decode, and a list parser must not skip an item it could not decode
      - it fails the whole parse, or returns the failures alongside the successes so the
      caller can see them. Choose that behavior deliberately and write it down; it is a
      design decision, not a default.
    </implication>
    <example>
      // BAD: an optional field with the wrong type vanishes without trace.
      const confidence = typeof raw.confidence === "number" ? raw.confidence : undefined

      // GOOD: absent is fine, present-and-wrong is a failure.
      if ("confidence" in raw &amp;&amp; typeof raw.confidence !== "number") {
        return { ok: false, stage: "shape", field: "confidence" }
      }
    </example>
  </principle>
</response_parsing>

<testing_consequences>
  <description>
    The layering exists to make the tests cheap. Each layer has one test target, and a test
    written at the wrong layer is either brittle or vacuous.
  </description>

  <layer_target layer="prompt_data">
    Usually no tests. A test asserting that a constant equals its own literal is a change
    detector that fails on every intentional edit and catches nothing. Test invariants over
    the data instead when they exist: every variant has a non-empty directive, two related
    dictionaries have matching key sets, every schema field name appears in the contract text.
  </layer_target>

  <layer_target layer="fragments">
    Assert the rendered string directly, and exactly. This is the only layer whose tests
    mention wording, so a copy edit produces one focused test failure that a reviewer reads
    as an intentional confirmation.
  </layer_target>

  <layer_target layer="builders">
    Assert composition, not content: that the expected fragments are present and in the
    expected order for each selection input. Obtain the expected text by calling the fragment
    function, never by re-typing the prose - a builder test containing a copied literal
    reintroduces exactly the coupling the split removed.
  </layer_target>

  <layer_target layer="parser">
    Table-driven over two axes: malformed payloads (bare, fenced, language-tagged, prose
    before and after, truncated mid-object, trailing comma, empty string) and schema-shape
    permutations (missing required field, extra unknown field, optional absent, optional
    present with the wrong type, correct type nested at the wrong depth). Assert the
    distinguishable failure stage, not just that it failed.
  </layer_target>

  <layer_target layer="orchestration">
    Wiring only, against a stub model client: the built prompt was what was sent, the raw
    response was handed to the parser, and a parser failure propagates as the intended
    outcome. Do not re-validate response shape here - that duplicates parser tests, is more
    expensive to set up, and gives a worse failure message when it breaks.
  </layer_target>

  <example>
    // Builder test: composition asserted via the fragment functions themselves.
    it("includes the contract block and the selected tone", () =&gt; {
      const prompt = buildSummaryPrompt(doc, "brief")
      expect(prompt).toContain(renderContract())
      expect(prompt).toContain(renderTone(TONE_PROFILES[0]))
      expect(prompt.indexOf(renderTone(TONE_PROFILES[0])))
        .toBeLessThan(prompt.indexOf(renderContract()))
    })

    // Parser test: one table, two axes, distinguishable stages.
    it.each([
      ["bare object",       '{"summary":"s","topics":[]}',        "ok"],
      ["fenced",            '```json\n{"summary":"s","topics":[]}\n```', "ok"],
      ["prose around",      'Sure!\n```\n{"summary":"s","topics":[]}\n```\nHope that helps', "ok"],
      ["truncated",         '{"summary":"s","topics":[',          "syntax"],
      ["missing required",  '{"topics":[]}',                      "shape"],
      ["optional mistyped", '{"summary":"s","topics":[],"confidence":"high"}', "shape"],
    ])("%s", (_name, raw, expected) =&gt; {
      const result = parseSummaryResponse(raw)
      expect(result.ok ? "ok" : result.stage).toBe(expected)
    })
  </example>
</testing_consequences>

<best_practices>
  <practice priority="critical">Define the output contract once as a named constant; import it in both the fragment that requests it and the parser that validates it.</practice>
  <practice priority="critical">Keep response parsing in a module that depends on no model client, so every malformed-input case is a plain function call.</practice>
  <practice priority="critical">Treat an optional field that is present with the wrong type as a failure; never omit it and continue.</practice>
  <practice priority="high">Keep builders composition-only - a builder body is a list of fragment calls with no literal prose.</practice>
  <practice priority="high">Represent prompt variant dictionaries as typed records plus one formatter, not as N template literals sharing a copied skeleton.</practice>
  <practice priority="high">Perform the structured-to-string mapping inside the module so public exports stay stable across a representation change.</practice>
  <practice priority="high">Declare static prompt data `readonly`/`as const` and type formatter parameters as `readonly T[]`.</practice>
  <practice priority="medium">Make fence extraction a named step whose failure is distinguishable from JSON syntax failure and from schema-shape failure.</practice>
  <practice priority="medium">Assert exact text only in fragment tests; assert order and presence in builder tests by calling the fragments.</practice>
  <practice priority="medium">Drive parser tests from a table over malformed payloads and shape permutations rather than adding cases as incidents arrive.</practice>
</best_practices>

<anti_patterns>
  <avoid name="prompt_literal_in_the_orchestrator">
    <description>The prompt is a template literal inside the async function that calls the model, alongside the parsing.</description>
    <instead>Extract data, fragments, builder, and parser; the orchestrator becomes a build call and a parse call.</instead>
  </avoid>
  <avoid name="builder_with_inline_prose">
    <description>A builder that interpolates sentences of its own between fragment calls, so wording and selection share a function.</description>
    <instead>Move every sentence into a fragment; leave the builder with only ordering and separators.</instead>
  </avoid>
  <avoid name="duplicated_output_contract">
    <description>The requested shape is written as prose in the prompt and independently as a schema in the parser.</description>
    <instead>One named constant (or generate the prose from the schema) imported by both sides.</instead>
  </avoid>
  <avoid name="lenient_optional_field">
    <description>A parser that drops an optional field it could not decode, or skips list items that failed validation.</description>
    <instead>Fail, or return the failures alongside the successes; absence is valid, malformed presence is not.</instead>
  </avoid>
  <avoid name="template_literal_variant_dictionary">
    <description>A switch or record mapping each variant to a full template literal, with the shared skeleton copied per branch.</description>
    <instead>Typed records for the varying axes plus one formatter that owns the skeleton.</instead>
  </avoid>
  <avoid name="mutable_static_prompt_data">
    <description>Module-level prompt arrays and objects declared without `readonly`, and formatters that demand `string[]`.</description>
    <instead>`as const` with `readonly` element types; formatters accept `readonly T[]`.</instead>
  </avoid>
  <avoid name="whole_prompt_snapshot_as_the_only_test">
    <description>A snapshot of the fully assembled prompt standing in for fragment and builder tests.</description>
    <instead>Fragment tests for text and builder tests for order; a whole-prompt snapshot fails on every copy edit and localizes nothing.</instead>
  </avoid>
  <avoid name="response_shape_asserted_in_orchestration_tests">
    <description>Orchestration tests that stub the model with malformed strings to exercise parser branches.</description>
    <instead>Table-driven parser tests; orchestration tests assert wiring and error propagation only.</instead>
  </avoid>
  <avoid name="opaque_parse_failure">
    <description>One try/catch around extraction, JSON parsing, and validation, producing a single undifferentiated error.</description>
    <instead>Stage-tagged failures so a caller can distinguish chatter from broken syntax from a wrong shape.</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>The output contract has exactly one definition, imported by both the requesting prompt and the validating parser.</rule>
  <rule>Response parsing lives in a module with no dependency on the model client or the builders.</rule>
  <rule>A present-but-wrong-typed optional field is a parse failure, never a silent omission.</rule>
  <rule>Builders compose; they do not contain prose, and they do not parse.</rule>
</rules>

<rules priority="standard">
  <rule>Static prompt literals live in a data module that imports nothing from the other layers.</rule>
  <rule>Variant dictionaries are typed records plus a formatter; the structured-to-string mapping happens at the module boundary.</rule>
  <rule>Static prompt data is `readonly`; formatters accept `readonly T[]`.</rule>
  <rule>Exact-text assertions belong to fragment tests; builder tests assert presence and order via the fragment functions.</rule>
  <rule>Parser tests are table-driven across malformed payloads and schema-shape permutations.</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Find where prompt text, selection, the contract, and parsing are fused</objective>
    <step order="1">
      <action>Locate template literals and long string concatenations inside functions that call a model</action>
      <tool>Grep for the model client's call sites, then read the enclosing functions</tool>
      <output>Inventory of fused prompt sites, with an estimate of prose length versus branching</output>
    </step>
    <step order="2">
      <action>Identify every place the response shape is stated - prompt prose, example objects, validators, and types</action>
      <tool>Grep for the field names of the expected response</tool>
      <output>Contract-duplication map; each duplicate is a drift site</output>
    </step>
  </phase>
  <phase name="extract">
    <objective>Split into the four layers, one boundary at a time</objective>
    <step order="1">
      <action>Move literals to the data module and the contract to a single named constant; leave call sites untouched</action>
      <tool>Edit</tool>
      <output>Data layer with stable public exports</output>
    </step>
    <step order="2">
      <action>Extract repeated formatting into fragment functions; reduce the builder to composition</action>
      <tool>Edit</tool>
      <output>Fragment modules plus a builder whose body is a list of calls</output>
    </step>
    <step order="3">
      <action>Move fence stripping, JSON parsing, and validation into the parser module; reduce orchestration to build-and-parse</action>
      <tool>Edit</tool>
      <output>Parser importing only the shared contract</output>
    </step>
  </phase>
  <phase name="validate">
    <objective>Confirm the split paid for itself in tests</objective>
    <step order="1">
      <action>Add fragment tests for exact text and builder tests for order; delete whole-prompt snapshots the split made redundant</action>
      <tool>Bash (test runner)</tool>
      <output>Wording failures localized to one fragment test</output>
    </step>
    <step order="2">
      <action>Add the table-driven parser suite over malformed payloads and shape permutations; strip response-shape assertions out of orchestration tests</action>
      <tool>Bash (test runner)</tool>
      <output>Parser branches covered without a model client</output>
    </step>
  </phase>
</workflow>

<error_escalation>
  <examples>
    <example severity="low">A fragment that could be split further but is already independently assertable</example>
    <example severity="medium">Builder tests containing copied prose literals, so copy edits break selection tests</example>
    <example severity="high">The requested output shape and the validator held as independent copies</example>
    <example severity="critical">A parser that silently drops malformed optional fields or skips malformed list items, producing plausible short results</example>
  </examples>
</error_escalation>

<constraints>
  <must>Share one output-contract definition between the prompt and the parser</must>
  <must>Keep the parser free of any model-client dependency</must>
  <must>Treat malformed presence as a failure and absence as valid</must>
  <must>Keep builders composition-only and prose in fragments or data</must>
  <avoid>Template-literal prompt blobs inside model-calling functions</avoid>
  <avoid>Mutable static prompt data and `string[]` formatter parameters</avoid>
  <avoid>Whole-prompt snapshots standing in for layered tests</avoid>
</constraints>

<related_skills>
  <skill name="typescript-ecosystem">Compiler configuration, schema-as-single-source-of-truth, and the type mechanics the data and parser layers rely on</skill>
  <skill name="testing-patterns">General test strategy, fixtures, and doubles that the per-layer test targets plug into</skill>
  <skill name="quality-tools">Refactoring operations for the data/logic module split this architecture is an instance of</skill>
  <skill name="effect-ts">Typed error channels and schema-derived types when the parser is written in Effect</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate model call sites, prompt literals, and duplicated contract declarations</agent>
  <agent name="quality-assurance">Review layer boundaries and contract-drift risk</agent>
  <agent name="test">Design the table-driven parser suite and per-layer assertions</agent>
</related_agents>
