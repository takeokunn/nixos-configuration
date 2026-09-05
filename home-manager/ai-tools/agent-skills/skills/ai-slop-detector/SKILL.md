---
name: ai-slop-detector
description: Use when auditing already-written prose or code for the tells output_discipline (ai-prompts/CLAUDE.md) bans, rather than applying the norm while drafting. Covers grep patterns per tell, the quoted-example false-positive trap, and code-artifact slop (dead branches, needless abstraction, restated docstrings, scaffolding).
version: 1.0.0
---

This skill is the audit procedure for the norm output_discipline states in `ai-prompts/CLAUDE.md`. That file
says what is banned; this file says how to find an instance already sitting in a body of prose or code. Do not
restate its banned-token list here; read it, then come back for the detection technique.

## Prose tells: what greps and what doesn't

Most tells resolve to a lexical scan. Run these over the target file, not over a diff, since slop introduced
gradually never shows up in any single diff.

| Tell | Pattern (case-insensitive) |
|---|---|
| Announcement / closing restatement | `in this (section\|article\|document)\|^overall,\|in summary\|it is worth noting` |
| Empty intensifier / self-praise | `\b(robust\|comprehensive\|seamless\|successfully\|significantly)\b` |
| Informationless hedge | `\b(essentially\|basically\|arguably)\b` |
| Formulaic parallelism | `not only .* but also\|it('s\| is) not just .*, it('s\| is)` |
| Sycophantic opener | `^(you('re\| are) absolutely right\|great question\|excellent point)` |
| Em dash (English prose) | the em dash character, U+2014: `perl -CSD -ne 'print "$.: $_" if /\x{2014}/'` |
| Decorative emoji | `grep -P '[\x{1F300}-\x{1FAFF}\x{2600}-\x{2604}\x{2606}-\x{27BF}]'` |

U+2605 is carved out of that last range on purpose. This corpus uses the star as the Insight-block marker that
`ai-prompts/output-styles/explanatory-strict.md` mandates, so a range starting at U+2600 flags a character the
system prompt requires. The hook scripts under `ai-prompts/hooks/` likewise use the cross and check marks as
functional status markers in their stderr, not as decoration.

Two of these patterns need a judgment pass on top of the match rather than being read as findings directly.
The parallelism row cannot tell a formulaic antithesis from a sentence that genuinely contrasts two things.
The intensifier row fires on any use of its words, and on this corpus most of its hits are not defects. Three
buckets, only the last of which is a finding:

- **The rule quoting its own subject**, covered in the next section.
- **A domain term of art**, where the word carries a technical meaning no synonym replaces. "Robust to a
  single outlier" is a statistics term in `performance-benchmarking/SKILL.md`, "robust selectors" is an
  established test-automation term in `agents/test.md`, and "exited successfully" beside the exit status it
  reports is a fact. None of these stand in for evidence.
- **The word asserting quality with nothing behind it.** "Successfully implemented a robust solution", or a
  self-description like "provides a comprehensive methodology". This is the only bucket to report.

The discriminator is whether deleting the word removes information. In the second bucket it does, and in the
third it does not.

Two tells resist a pattern. "Any sentence carrying no fact the reader lacked" is a judgment call, not a string:
for each sentence, ask what the reader loses if it is deleted, and cut it if the answer is nothing. That is
exactly the question [cold-read](../cold-read/SKILL.md)'s fresh reviewer answers for durable prose, so dispatch
it rather than eyeballing your own draft. Japanese prose has its own token list (LLM-tell avoidance in
[technical-writing](../technical-writing/SKILL.md)). Do not re-derive or restate those tokens here; grep for
them using that skill's list, in that language.

## The false-positive trap

A corpus that defines a banned-token rule has to quote the token to state the rule, so a lexical scan over that
corpus flags the rule's own definition. The discriminator: a hit inside a quotation, a code fence, or a
worked/illustrative example is the rule's subject, not a violation of it. Check the enclosing syntax before
logging a finding.

Two real instances in this repo, worth knowing by name so you recognize the shape elsewhere:

- `agent-skills/skills/technical-writing/SKILL.md:116` and `:127` quote the em dash (U+2014) and two related
  Japanese dash variants as the literal subject of the rule banning them in Japanese prose. A raw grep for that
  character over that file returns a nonzero count that names no defect.
- `ai-prompts/CLAUDE.md` and `ai-prompts/output-styles/explanatory-strict.md` each quote "robust" and
  "comprehensive" in the banned-intensifier list itself. Same shape: the hit is the rule stating its own
  subject, not an instance of the padding it forbids. Locate them with
  `grep -n '"robust", "comprehensive"'` rather than by line number, since prose files renumber.

Before reporting a hit, read enough of the surrounding paragraph to tell whether the sentence is asserting a
claim in that word, or naming the word as an example. Only the former is a finding.

## Code-artifact slop

No existing skill covers this; output_discipline names it but not how to find it in an already-written diff.

**Defensive branch guarding an unreachable condition.** A null check, type guard, or catch-and-rethrow after
the caller already establishes the invariant. Not lexically detectable in general, since reachability is a
property of the call sites, not the branch's text, but two shapes are: a guard duplicating a check the
immediately enclosing scope already performed, and a `catch ($E) { throw $E; }` (or language equivalent) that
adds nothing
over letting the exception propagate. Use the ast-grep skill to match the catch-and-rethrow shape structurally
rather than by text, since the exact spacing and variable name vary per call site.

**Abstraction introduced for a second case that does not exist.** An interface, strategy, or plugin point with
exactly one implementation, or a config parameter that holds the same value at every call site. Detection is a
reference count, not a grep: find the interface or base class, then run Serena's `find_implementations` (or
`find_referencing_symbols` for a parameter) and check the count. A count of one is the finding; the fix is
inlining the abstraction into its sole implementor, not adding a second case to justify it.

**Docstring restating the signature.** A docstring whose content is fully recoverable from the function name,
parameter names, and types already in the signature, adding no WHY (a constraint, an invariant, a caller-facing
gotcha) the signature can't show. Detection: read the docstring against the signature side by side; if every
noun in the docstring already appears as an identifier, it restates rather than explains. The ast-grep skill can
locate all docstrings of a given node kind for batch review; the restates-or-explains judgment itself stays
manual.

**Scaffolding standing in for the work.** A function body that is a stub, a hardcoded return dressed as
computed output, or an exception meaning "not implemented" left behind a caller that no longer expects one.
Grep for the direct markers first: `TODO|FIXME|XXX|not implemented|NotImplementedError|unimplemented!|panic!\(
"todo"`. These markers undercount, since a stub can return a plausible-looking constant with no marker at all;
cross-check any function whose body is disproportionately short against what its name and call sites imply it
should do.

**Ceremonial placeholder steps.** A numbered list of steps, or a sequence of log statements, that narrates
work without doing any ("Step 1: Analyze the problem" with no analysis attached, a "Starting process..." log
immediately followed by the next step with no intervening work). This is a reading heuristic, not a grep: for
each step, name the artifact it produces; a step producing nothing is ceremony.

## Reporting a finding

Every finding is a file:line and a one-line concrete change, never a direction like "clean this up" or "make
this more concise", since that names no defect and gives the writer nothing to act on. State what the current
text or code does, what fact or behavior is missing or wrong, and the specific replacement.

## Related

- [cold-read](../cold-read/SKILL.md): dispatch when the tell is "carries no fact the reader lacked" and needs a
  reader's judgment rather than a pattern match, or when a full document needs a fresh read after this audit.
- [technical-writing](../technical-writing/SKILL.md): the Japanese LLM-tell token list and prose-quality rules
  this skill does not restate.
- ast-grep: structural matching for the code-artifact shapes above where a text pattern would miss or
  overmatch, if that skill is available in this environment.
