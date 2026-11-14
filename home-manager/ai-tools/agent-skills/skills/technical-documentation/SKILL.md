---
name: technical-documentation
description: Use when writing or auditing a README, API docs, design doc, specification, or user guide, including checking docs against the code.
metadata:
  version: "3.0.0"
---

Patterns for README, API spec, design doc, and user guide writing, and for auditing existing docs against
the code they describe. Documentation rots in two directions: it over-claims capabilities that were never
built or were removed, and it under-claims by still calling shipped work "planned." Teams reliably audit only
the first direction, so the second accumulates unchecked; the status-claim audit exists to catch both. Not
every rationale belongs in a document: a rationale belongs wherever the reader who needs it will be standing:
a conventions file, a design doc, or a comment at the line itself.

## Document types

Match structure and depth to audience: developers get technical depth, team members get context plus depth,
end users get no jargon and step-by-step instructions.

- **README**: project entry point. Include the purpose, relevant features, quick start/install, basic usage,
  documentation links, and contributing/license information as needed. Link to detailed docs when they
  distract from getting started; there is no line or feature-count quota.
- **API specification**: for REST/GraphQL/SDK interfaces. Sections: overview + authentication, base URL and
  versioning, endpoints (method, path, parameters, response), error codes, rate limits, examples (curl and
  language-specific).
- **Design document**: for a proposed feature or architectural change. Sections: summary (problem/solution/
  scope), background, goals and non-goals, technical design (architecture, data flow, components),
  alternatives considered (with why each was rejected), security/privacy considerations, testing strategy,
  rollout plan.
- **User guide**: for non-technical end users. Sections: getting started, core concepts, step-by-step
  tutorials, feature reference, troubleshooting/FAQ, glossary.

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

Give each claim one audit verdict: confirmed, over-claimed, under-claimed, wrong-mechanism, or unresolved when
the available evidence cannot settle it. These verdicts are judgement terms for the audit; they are not
evidence tags, so tag the evidence behind each verdict as CLAUDE.md evidence defines. Over-claims are the
dangerous kind: a stale line saying a safety control exists gets cited as evidence that it does. Under-claims
are the wasteful kind: they cause already-shipped features to be re-planned and rebuilt.

## Reversal rationale at the change site

When a change deliberately deletes, inverts, or narrows something that carried a documented rationale (an
exemption, a suppression, a deliberate gate, a documented workaround), leave the reversal's reasoning at that
site if its absence would confuse the next reader. The commit message, the pull request, and session memory
are not visible to someone reading the code later. State that the removal was intentional and what changed so
the original reason no longer applies, enough that a reader does not restore it. Do not restate what the
surrounding code does.

An absence carries no evidence: a deliberately removed exception looks the same as an accidental deletion,
and reviewers flag it as a regression. That is why this case earns a code comment.

## Language guidelines

**English**: active voice, present tense, professional but approachable tone. Avoid unnecessarily complex
words and idioms that don't translate.

- Good: "Run this command to start the server."
- Bad: "The server can be started by running the following command."

**Japanese**: match the existing document's style. For Japanese prose rules, see the yomiyasu skill.

**Bilingual**: maintain parallel structure between languages, keep code examples identical and translate only
prose, and use consistent terminology (build a glossary if terms recur).

## Other checkable criteria

- Validate runnable examples with scoped checks and record what ran. Label illustrative fragments as such;
  do not execute destructive or external-write examples without authorization merely to validate the prose.
- Define technical terms on first use or link to a glossary.

## Related

- [serena-usage](../serena-usage/SKILL.md): symbol operations for extracting code examples and API signatures
- [investigation-patterns](../investigation-patterns/SKILL.md): analyzing codebases to understand features
  before documenting them
- [technical-writing](../technical-writing/SKILL.md): writing blog posts and tutorials from documentation
- yomiyasu skill: Japanese prose rules
