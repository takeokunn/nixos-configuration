---
name: technical-documentation
description: Use when writing or auditing technical documentation - README, API docs, design docs, specifications, user guides, in English or Japanese. Also covers auditing docs against code for both over-claims and stale future-work claims, and retired-name registries.
version: 3.0.0
---

Patterns for README, API spec, design doc, and user guide writing, and for auditing existing docs against
the code they describe. Documentation rots in two directions: it over-claims capabilities that were never
built or were removed, and it under-claims by still calling shipped work "planned." Teams reliably audit only
the first direction, so the second accumulates unchecked; the audit pattern below exists to catch both. Not
every rationale belongs in a document: a rationale belongs wherever the reader who needs it will be standing:
a conventions file, a design doc, or a comment at the line itself.

## Document types

Match structure and depth to audience: developers get technical depth, team members get context plus depth,
end users get no jargon and step-by-step instructions.

- **README**: project entry point. Sections: title, one-line description, 3-5 key features, quick
  start/install, basic usage example, documentation links, contributing/license. Keep it under 500 lines;
  link out to detailed docs rather than growing it.
- **API specification**: for REST/GraphQL/SDK interfaces. Sections: overview + authentication, base URL and
  versioning, endpoints (method, path, parameters, response), error codes, rate limits, examples (curl and
  language-specific).
- **Design document**: for a proposed feature or architectural change. Sections: summary (problem/solution/
  scope), background, goals and non-goals, technical design (architecture, data flow, components),
  alternatives considered (with why each was rejected), security/privacy considerations, testing strategy,
  rollout plan.
- **User guide**: for non-technical end users. Sections: getting started, core concepts, step-by-step
  tutorials, feature reference, troubleshooting/FAQ, glossary.

Never publish a document with placeholder content or an unresolved TODO; an unfinished section reads as
finished to anyone who didn't write it.

## Retired vocabulary registry

When a rename or refactor leaves old names in circulation (in memory, old branches, old examples), list the
retired names in the conventions document, **paired with the still-current names they could be confused
with**. The pairing is what makes the list safe to act on: a retired-terms list published alone reliably
produces over-correction, because readers and agents start "fixing" names that merely resemble the retired
ones.

```
**Retired, do not reintroduce**
- `ItemRequest` → use `ItemCommand`
- `processItem()` → use `applyItem()`

**Still current, despite the resemblance**
- `ItemQuery` is current. It is not a retired name and must not be renamed to `ItemCommand`.
- `processItemEvent()` is current; only the non-event `processItem()` was retired.
```

## Status-claim audit (bidirectional)

Use when a document asserts what is or is not implemented: a status page, a roadmap, a security or
compliance capability list. Audit every claim in both directions; the two kinds of error have different
costs, so checking only one leaves the other invisible:

- **"We do X"**: locate the implementing symbol, then confirm it is reachable from a live entry point. A
  function that exists but that nothing calls is still an over-claim; finding the symbol is not enough.
- **"X is not yet implemented"**: search for it anyway. Shipped features routinely stay listed as future
  work because reviewers instinctively hunt only for over-claims.
- **Named mechanism** ("over WebSocket", "via the message queue"): verify the mechanism itself, not just the
  capability. Docs commonly keep the right feature description paired with the wrong transport.

Record each claim's outcome as confirmed, over-claimed, under-claimed, or wrong-mechanism. Over-claims are the
dangerous kind: a stale line saying a safety control exists gets cited as evidence that it does. Under-claims
are the wasteful kind: they cause already-shipped features to be re-planned and rebuilt.

## Reversal rationale at the change site

When a change deliberately deletes, inverts, or narrows something that carried a documented rationale (an
exemption, a suppression, a deliberate gate, a documented workaround), write the reversal's reasoning at the
same site the original rationale occupied, in the same form. Not the commit message, not the pull request, not
a session memory: none of those are visible to the person reading the code a year later. State that the
removal was intentional and what changed to make the original reason no longer apply: enough that a reader
does not "fix" it back. Do not restate what the surrounding code does.

This is the narrow case where a code comment earns its place: an absence carries no evidence, so a
deliberately removed exception is indistinguishable from an accidental deletion. Reviewers converge on
flagging it as a regression, which is a reliable signal that the reader needs the note more than the author
expected.

## Language guidelines

**English**: active voice, present tense, professional but approachable tone. Avoid unnecessarily complex
words and idioms that don't translate.

- Good: "Run this command to start the server."
- Bad: "The server can be started by running the following command."

**Japanese**: です・ます調 (polite form) for user docs, である調 for technical specs. Tone: 丁寧だが簡潔。
避けるもの: 過度なカタカナ語、曖昧な表現。

- Good: 以下のコマンドでサーバーを起動します。
- Bad: サーバーの起動については、下記コマンドを実行することで可能となります。

**Bilingual**: maintain parallel structure between languages, keep code examples identical and translate only
prose, and use consistent terminology (build a glossary if terms recur).

For Japanese prose-quality norms beyond structure (argumentation rigor, LLM-tell avoidance, dramatization
restraint), see technical-writing's `<japanese><prose_norms>`, linked below.

## Other checkable criteria

- Verify every code example compiles and runs, and include its expected output, before publishing.
- Define technical terms on first use or link to a glossary.
- Do not add timestamps to documents; they go stale immediately and the repository already has commit dates.

## Related

- [serena-usage](../serena-usage/SKILL.md): symbol operations for extracting code examples and API signatures
- [context7-usage](../context7-usage/SKILL.md): library documentation lookup for accurate API references
- [investigation-patterns](../investigation-patterns/SKILL.md): analyzing codebases to understand features
  before documenting them
- [technical-writing](../technical-writing/SKILL.md): writing blog posts and tutorials from documentation;
  the canonical source for Japanese prose-quality norms (argumentation rigor, LLM-tell avoidance,
  dramatization restraint, redundancy): apply its `<japanese><prose_norms>` when writing Japanese
  documentation
