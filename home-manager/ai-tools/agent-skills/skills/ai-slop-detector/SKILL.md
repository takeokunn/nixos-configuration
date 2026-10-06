---
name: ai-slop-detector
description: Use when auditing already-written prose or code for the tells output_discipline (ai-prompts/CLAUDE.md) bans, rather than applying the norm while drafting. Covers grep patterns per tell, the quoted-example false-positive trap, code-artifact slop (dead branches, needless abstraction, restated docstrings, scaffolding, laundering casts, uncontracted shims), and diff cleanup before review.
metadata:
  version: "1.3.0"
---

This skill is the audit procedure for the norm output_discipline states in `ai-prompts/CLAUDE.md`: that file says
what to avoid, this one how to find an instance already in prose or code. Read that contract first. Judge words
in context; the patterns below identify candidates, not banned tokens.

## Prose tells: what greps and what doesn't

Scan lexically, then judge each hit in context before reporting it. Run the scan over the target file, not a
diff, since slop introduced gradually never shows in any single diff. Zero hits is not evidence the text carries
content: prose with every listed token removed can still state nothing, so the judgment pass runs regardless.

| Tell | Pattern (case-insensitive) |
|---|---|
| Announcement / closing restatement | `in this (section\|article\|document)\|^overall,\|in summary\|it is worth noting` |
| Empty intensifier / self-praise | `\b(robust\|comprehensive\|seamless\|successfully\|significantly)\b` |
| Informationless hedge | `\b(essentially\|basically\|arguably)\b` |
| Formulaic parallelism | `not only .* but also\|it('s\| is) not just .*, it('s\| is)` |
| Sycophantic opener | `^(you('re\| are) absolutely right\|great question\|excellent point)` |
| Em dash (English prose) | the em dash character, U+2014: `perl -CSD -ne 'print "$.: $_" if /\x{2014}/'` |
| Decorative emoji | `grep -P '[\x{1F300}-\x{1FAFF}\x{2600}-\x{2604}\x{2606}-\x{27BF}]'` |

U+2605 is carved out of that last range on purpose: this corpus uses the star as the decision-note block marker
that `ai-prompts/output-styles/explanatory-strict.md` mandates, so a range starting at U+2600 flags a character
the system prompt requires. The hook scripts under `ai-prompts/hooks/` likewise use the cross and check marks as
functional stderr status markers, not decoration.

Two patterns need a judgment pass on top of the match. The parallelism row cannot tell a formulaic antithesis
from a sentence that genuinely contrasts two things. The intensifier row fires on any use of its words, and here
most hits are not defects. Three buckets, only the last a finding:

- The rule quoting its own subject, covered in the next section.
- A domain term of art, where the word carries a technical meaning no synonym replaces. "Robust to a single
  outlier" is a statistics term in `performance-benchmarking/SKILL.md`, "robust selectors" is an established
  test-automation term in `agents/test.md`, and "exited successfully" beside the exit status it reports is a
  fact. None stand in for evidence.
- The word asserting quality with nothing behind it: "Successfully implemented a robust solution", or a
  self-description like "provides a comprehensive methodology".

The discriminator is whether deleting the word removes information: in the second bucket it does, in the third
it does not.

Two tells resist a pattern. "Any sentence carrying no fact the reader lacked" is a judgment call: for each
sentence, ask what the reader loses if it is deleted, and cut it if nothing. That is exactly the question
[cold-read](../cold-read/SKILL.md)'s fresh reviewer answers for durable prose, so dispatch it rather than
eyeballing your own draft. Japanese prose has its own token list (LLM-tell avoidance in
[technical-writing](../technical-writing/SKILL.md)); grep for those tokens in that language using that skill's
list. Do not re-derive or restate them here.

## The false-positive trap

A corpus that defines a banned-token rule must quote the token to state it, so a lexical scan flags the rule's
own definition. Distinguish examples of prohibited wording from deliverable prose or code: quotation marks and
code fences alone do not exempt their contents. Read the surrounding paragraph to tell whether the sentence
asserts a claim in that word or names the word as an example; only the former is a finding.

`technical-writing/SKILL.md` quotes the em dash (U+2014) and two related Japanese dash variants as the literal
subject of the rule banning them in Japanese prose. A raw grep for that character over that file returns a
nonzero count that names no defect.

## Code-artifact slop

output_discipline names these shapes; this section is how to find them in written code.

**Defensive branch guarding an unreachable condition.** A null check, type guard, or catch-and-rethrow after the
caller already establishes the invariant. Not lexically detectable in general, since reachability is a property
of the call sites, not the branch's text, but two shapes are: a guard duplicating a check the immediately
enclosing scope already performed, and a `catch ($E) { throw $E; }` (or language equivalent) that adds nothing
over letting the exception propagate. Use the ast-grep skill to match catch-and-rethrow structurally, since
spacing and variable name vary per call site.

**Abstraction introduced for a second case that does not exist.** An interface, strategy, or plugin point with
exactly one implementation, or a config parameter holding the same value at every call site. Detection is a
reference search, not a grep: find implementations and callers with the available symbol tools. One
implementation is a review candidate, not proof of a defect. Check for a present contract, dependency boundary,
or test seam before proposing inlining; do not invent a second implementation to justify it.

**Docstring restating the signature.** A docstring fully recoverable from the function name, parameter names, and
types in the signature, adding no WHY (a constraint, an invariant, a caller-facing gotcha) the signature can't
show. Read the docstring against the signature side by side and name the information it adds; repeated
identifiers alone do not establish redundancy. The ast-grep skill can locate all docstrings of a node kind for
batch review; the restates-or-explains judgment stays manual.

**Scaffolding standing in for the work.** A function body that is a stub, a hardcoded return dressed as computed
output, or an exception meaning "not implemented" left behind a caller that no longer expects one. Search for
direct markers first with `aitools search`: `TODO|FIXME|XXX|not implemented|NotImplementedError|unimplemented!|panic!\("todo"`.
Markers undercount, since a stub can return a plausible-looking constant with no marker; cross-check any
function whose body is disproportionately short against what its name and call sites imply it should do.

**Ceremonial placeholder steps.** A numbered list of steps, or a sequence of log statements, that narrates work
without doing any ("Step 1: Analyze the problem" with no analysis attached, a "Starting process..." log
immediately followed by the next step with no intervening work). A reading heuristic, not a grep: for each step,
name its observable result, including a decision, approval, or report. Report steps that add neither a result
nor a necessary precondition.

**A cast that launders a type.** An escape-type cast (`any`, `interface{}`, `Object`, a raw pointer) or a
widen-then-narrow pair asserts what no check established. List the cast nodes in the changed code with the
ast-grep skill and name the invariant that makes each true; where none can be named, replace the cast with a
check or a correctly typed source.

**A compatibility path with no contract.** A shim, alias, retry, or fallback branch needs a shipped consumer
that depends on it and a removal plan. Search for the consumer; an empty search is the evidence, so report it
with the finding.

**Style that ignores its file.** Naming, import order, error-handling idiom, or control flow that differs from
the neighboring functions in the same file, not from a general style guide.

Single-use helpers and pass-through intermediates are the abstraction entry above; their deletion proof is in
[Removing dead code](../investigation-patterns/SKILL.md#removing-dead-code).

### Cleaning a diff before review

Unlike the whole-file prose scan, a code cleanup may cover only the current change: the author owns it, and a
repository-wide sweep mixes unrelated edits into the review. Check each hunk of the diff against the merge base
for the shapes above; fix only what cannot change behavior and report the rest to the author. Run this before
code review, never instead of it.

## Reporting a finding

Every finding is a file:line and a one-line concrete change, never a direction like "clean this up" or "make
this more concise", which names no defect and gives the writer nothing to act on. State what the current text or
code does, what fact or behavior is missing or wrong, and the specific replacement.

## Related

- [cold-read](../cold-read/SKILL.md): dispatch when the tell is "carries no fact the reader lacked" and needs a
  reader's judgment rather than a pattern match, or when a full document needs a fresh read after this audit.
- [technical-writing](../technical-writing/SKILL.md): the Japanese LLM-tell token list and prose-quality rules
  this skill does not restate.
- ast-grep: structural matching for the code-artifact shapes above where a text pattern would miss or
  overmatch, if that skill is available in this environment.
