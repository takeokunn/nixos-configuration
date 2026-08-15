---
name: llm-prompt-architecture
description: Use when structuring the source code of an LLM-driven feature - separating prompt builders, static prompt data, and response parsers; sharing one output-contract between prompt and parser; fence-stripping and malformed-response handling. Not for wording, tone, or model choice.
version: 3.0.0
---

Prompt code separates into four layers — static data, pure fragment renderers, composition-only
builders, and a response parser — so prompt text is diffable, prompt selection is testable without a
model call, and the requested output shape cannot silently drift from the validator that enforces it.
This is about source structure, not wording: nothing here covers persona, tone, few-shot selection,
chain-of-thought, or model choice. Language mechanics belong to typescript-ecosystem; general test
design (coverage, fixtures, doubles) belongs to testing-patterns — this skill covers only which layer
each assertion belongs to once the prompt code is split.

## Four layers

Dependency direction is strictly downward: data knows nothing, fragments know data, builders know
fragments, orchestration knows builders and the parser. The parser sits beside the builders and shares
only the output contract with them.

**1. Prompt data.** Holds every literal a human would edit — instruction prose, checklist and
procedure text, variant dictionaries, labels, thresholds, output-contract text. Values only, no
functions that do work. Imports nothing from the other layers; a data module that imports a formatter
has become a fragment module. Wording is the single most frequent change to prompt code and the least
likely to need review of executable logic, so isolating literals means a copy edit produces a diff a
reviewer reads as prose, not code.

**2. Fragments.** Pure functions that render one piece of the prompt: data plus arguments in, string
out. One fragment per reusable section (shared preamble, context block, output-contract block). Must
not import builders, the parser, the model client, or do any I/O — a fragment that reads a file or
awaits anything is doing context acquisition, which belongs above it. A fragment is the smallest unit
whose exact output a test can assert; once a section is a named function, a test pins that section's
text without re-validating the whole prompt, so an unrelated wording change elsewhere can't break it.

**3. Builders.** Composition only: choose which fragments apply to this request, call them, join the
results in order. No literal prose of its own, and no parsing — joining separators and section
ordering are the only strings a builder may own. Selection and interpolation are two different
decisions with two different failure modes, and keeping them in one function kills testability: when
one function both selects sections and interpolates their text, **its only observable is the final
multi-hundred-line string**, so every assertion becomes a weak substring match (`toContain` can't
distinguish "the section is present in the right place" from "those words happen to appear somewhere").
The test surface also turns multiplicative rather than additive — with S selectable sections and W
wording variants you must exercise combinations to reach any one branch — and a pure copy edit breaks
tests written to check selection logic, which teaches the team to loosen assertions until they check
nothing. Split until each layer has an observable of its own: fragment tests assert exact text and are
the only tests that mention wording; builder tests assert which fragments appear and in what order, by
calling the fragment functions and comparing, never by re-typing the expected prose.

**4. Parser.** Turns a raw model response into a validated domain value or a failure: strip fences,
parse JSON, validate the shape against the output contract. Must not import builders or the model
client — it imports the shared output contract and nothing else from the prompt side, so it can be
tested with hand-written strings and never needs a prompt to exist. A parser reachable only through a
model call can only be tested through a model call; isolating it turns "what happens when the model
returns a trailing comma" from an integration question into a one-line unit test.

```typescript
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
```

## Static data

**Typed definitions plus a formatter, not template-literal blobs.** A variant dictionary written as
template-literal blobs duplicates the shared skeleton once per variant, burying the axes that actually
differ (a tone, a length limit, required points) inside repeated surrounding prose, so **neither a
reader nor a diff can see what distinguishes one variant from another**, and a shared-skeleton change
must be applied N times with N chances to miss one. Declare variants as typed records with the varying
axes as fields, and render them with one small formatter that owns the skeleton — the type then
enforces that a newly added variant supplies every axis, an omission the template-literal form cannot
detect.

```typescript
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
export const renderTone = (profile: ToneProfile): string =>
  [profile.directive, ...profile.mustCover.map((c) => `- cover ${c}`)].join("\n")
```

**Map structured data to strings at the module boundary.** Converting a prompt module to structured
data is a refactor of its internals and should not be observable to callers. If the structured records
leak out as the public export, every consumer and every consumer's test must be rewritten, which makes
the refactor expensive enough that it does not happen. Keep the structured-to-string mapping inside the
module so the exported symbol keeps the shape it always had; export the typed records additionally,
never instead.

```typescript
// Internal representation changed; the public export did not.
const PROFILE_BY_ID = new Map(TONE_PROFILES.map((p) => [p.id, p]))

export const toneInstruction = (id: string): string => {
  const profile = PROFILE_BY_ID.get(id)
  return profile === undefined ? "" : renderTone(profile)
}
```

**Declare static prompt data immutable.** It is shared, module-level, and read by every request, so a
mutable array or object there is a cross-request mutation hazard with no upside — nothing legitimately
edits prompt copy at runtime. **An `as const` literal has a readonly tuple type, and a formatter whose
parameter is `string[]` will reject it**; the two ways out (copy the array on every call, or drop
`as const` and lose the literal types that made the data self-documenting) are both bad. Declare static
prompt data with `readonly` element types and `as const`, and type every formatter parameter as
`readonly string[]` (or `readonly T[]`) — it is the strictly more permissive parameter type, so a
mutable array is assignable to it and accepting `readonly` never costs a caller anything.

```typescript
// Accepts both `readonly string[]` literals and ordinary arrays.
export const renderChecklist = (items: readonly string[]): string =>
  items.map((item, i) => `${i + 1}. ${item}`).join("\n")
```

## Output contract

**One named constant, two consumers.** The response shape the prompt asks for and the shape the parser
accepts are one decision. When the instruction text and the validator hold independent copies, they
drift the first time either changes, and the type checker cannot catch it because one copy is prose.
**The quiet drift direction is the dangerous one**: if the prompt is updated to request a new field and
the validator is not, the field arrives and is discarded — the feature appears to do nothing, with no
error anywhere. If the validator is tightened and the prompt is not, every response fails validation
and the feature breaks completely, which at least announces itself. The quiet failure is the common
one, because prompts are edited far more often than parsers. Define the contract once in the data
layer as a named export; the fragment that renders the instruction and the parser both import it, so
there is nothing to diverge from. Where the stack allows it, make the machine-readable schema the
source and render the instruction text from it, so adding a field updates the prompt automatically;
where it does not, at minimum co-locate the schema and its prose description in one module so editing
one puts the other in the same diff.

```typescript
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
export const renderContract = (): string => SUMMARY_CONTRACT_TEXT
// summary.parsers.ts
const validated = SUMMARY_SCHEMA.parse(candidate)
```

Where the text cannot be generated from the schema, a contract-drift test is cheap insurance: assert
that every field name in the schema appears in the contract text. It catches the quiet direction — a
schema field the prompt never asks for.

## Response parsing

The parser is its own module with its own tests, and it treats the model as what it is: an untrusted
producer whose output is frequently not quite what was requested.

**Keep the parser separate from orchestration.** Orchestration — build the prompt, call the model, hand
off the response — is thin and hard to test because it needs a model client. Parsing is dense and
trivially testable because it needs only a string. Fusing them imports the hard-to-test dependency into
the easy-to-test logic, so parsing branches end up covered, if at all, through a stubbed model call
that must be re-stubbed for every malformed-input case. Put parsing and structural validation in a
dedicated module that takes a string and returns a validated value or a failure; orchestration becomes
two calls with no branching of its own, and every parser case is a plain function call in a table.

**Make fence-stripping its own step.** Models routinely wrap JSON in code fences, sometimes tag the
fence with a language, and sometimes add a sentence before or after it. Those are three distinct
extraction problems, and they are distinct again from JSON syntax errors and from schema violations.
Collapsing them into one try/catch produces a single opaque "bad response" error, so nobody can tell
whether the model chattered, emitted broken JSON, or returned a well-formed object of the wrong shape —
three different fixes, in the prompt, the retry policy, and the contract respectively. Layer the parser
as extract, then parse, then validate, and make each stage's failure distinguishable in the returned
error. **Keep extraction tolerant (fenced, tagged, bare payloads, prose around the object) while keeping
validation strict** — the tolerance belongs at the transport-shaped stage, not the shape stage.

```typescript
const stripFences = (raw: string): string => {
  const fenced = /```(?:[a-z]+)?\s*([\s\S]*?)```/i.exec(raw)
  return (fenced?.[1] ?? raw).trim()
}

export const parseSummaryResponse = (raw: string): Result<Summary> => {
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
```

**Treat malformed input as the normal case.** A parser over model output is not defensive programming
against an impossible event. Truncation at a token limit, an unrequested preamble, a trailing comma, a
number returned as a string, and an enum value that is a near-synonym of an allowed one are all
routine. Code written on the assumption that the happy path is the overwhelming case gets the
ergonomics backwards — it throws from deep inside and leaves the caller with no way to distinguish
retryable from permanent failure. Model failure as a value in the parser's return type, or throw a
typed error — either is fine, but be consistent, and make the failure carry enough detail for the
caller to decide between retrying, degrading, and surfacing. Enumerate the malformed cases in a
table-driven test rather than adding them one at a time as production incidents reveal them.

**Present-but-wrong-type is fatal.** "Optional" conflates two situations that deserve opposite
handling: the field is absent, which the contract permits, and the field is present with the wrong
type, which means the producer misunderstood the contract. **The lenient reading — drop the field and
continue — converts a producer bug into silent data loss**, and it is worst exactly where it is most
tempting: in list processing, where skipping malformed items yields a plausible-looking short result
that no downstream check can distinguish from a genuinely short one. Absence is valid; malformed
presence is an error. A parser must not omit an optional field it could not decode, and a list parser
must not skip an item it could not decode — it fails the whole parse, or returns the failures alongside
the successes so the caller can see them. Choose that behavior deliberately and write it down; it is a
design decision, not a default.

```typescript
// BAD: an optional field with the wrong type vanishes without trace.
const confidence = typeof raw.confidence === "number" ? raw.confidence : undefined

// GOOD: absent is fine, present-and-wrong is a failure.
if ("confidence" in raw && typeof raw.confidence !== "number") {
  return { ok: false, stage: "shape", field: "confidence" }
}
```

## Testing

The layering exists to make tests cheap. Each layer has one test target, and a test written at the
wrong layer is either brittle or vacuous.

- **prompt_data** — usually no tests. A test asserting that a constant equals its own literal is a
  change detector that fails on every intentional edit and catches nothing. Test invariants over the
  data instead where they exist: every variant has a non-empty directive, two related dictionaries have
  matching key sets, every schema field name appears in the contract text.
- **fragments** — assert the rendered string directly and exactly. This is the only layer whose tests
  mention wording, so a copy edit produces one focused test failure a reviewer reads as an intentional
  confirmation.
- **builders** — assert composition, not content: that the expected fragments are present and in the
  expected order for each selection input. Obtain the expected text by calling the fragment function,
  never by re-typing the prose — a builder test containing a copied literal reintroduces exactly the
  coupling the split removed.
- **parser** — table-driven over two axes: malformed payloads (bare, fenced, language-tagged, prose
  before and after, truncated mid-object, trailing comma, empty string) and schema-shape permutations
  (missing required field, extra unknown field, optional absent, optional present with the wrong type,
  correct type nested at the wrong depth). Assert the distinguishable failure stage, not just that it
  failed.
- **orchestration** — wiring only, against a stub model client: the built prompt was what was sent, the
  raw response was handed to the parser, and a parser failure propagates as the intended outcome. Do
  not re-validate response shape here — that duplicates parser tests, is more expensive to set up, and
  gives a worse failure message when it breaks.

A whole-prompt snapshot standing in for fragment and builder tests fails on every copy edit and
localizes nothing; once the layers exist, delete it rather than maintaining it alongside them.

```typescript
// Builder test: composition asserted via the fragment functions themselves.
it("includes the contract block and the selected tone", () => {
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
])("%s", (_name, raw, expected) => {
  const result = parseSummaryResponse(raw)
  expect(result.ok ? "ok" : result.stage).toBe(expected)
})
```

## Related

- [typescript-ecosystem](../typescript-ecosystem/SKILL.md) — compiler configuration,
  schema-as-single-source-of-truth, and the type mechanics the data and parser layers rely on
- [testing-patterns](../testing-patterns/SKILL.md) — general test strategy, fixtures, and doubles that
  the per-layer test targets plug into
- [quality-tools](../quality-tools/SKILL.md) — refactoring operations for the data/logic module split
  this architecture is an instance of
- [effect-ts](../effect-ts/SKILL.md) — typed error channels and schema-derived types when the parser is
  written in Effect
