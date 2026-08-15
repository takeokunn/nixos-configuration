---
name: serena-usage
description: Use for Serena MCP work - semantic symbol search, find references, code navigation, memory read/write, and organizing memory as a linked reference graph rather than a flat list. Also covers recovering a subagent's report from its session transcript when a completion notification is lost.
version: 4.0.0
---

Serena's tool schemas are injected by the harness and are not restated here. This file covers tool *choice*,
the failure modes that make a Serena result misleading, and how a memory corpus stays worth reading.

## Tool choice

Serena owns code intelligence; standard tools own the filesystem.

| Need | Tool |
|---|---|
| Find files by name | Glob |
| Search file contents | Grep for discovery, then Serena for navigation |
| File structure | `get_symbols_overview` — depth 0 first, then depth 1 |
| A named function, class, or method | `find_symbol`; `substring_matching=true` when the name is uncertain |
| Its implementation | `find_symbol` with `include_body=true` |
| A definition, from a call site | `find_declaration` with a regex capturing the symbol — for `obj.process(x)`, pass `"obj\.(process)\("` |
| Implementations of an interface | `find_implementations` (`relative_path` must be a file, not a directory) |
| Callers and impact | `find_referencing_symbols` |
| Replace a whole symbol | `replace_symbol_body` |
| Change a few lines inside one | `replace_content`, regex mode with wildcards — `"beginning.*?end"` — to avoid transcribing large sections. DOTALL and MULTILINE are on; backreferences are `$!1`, `$!2` |
| Rename across the codebase | `rename_symbol` — never update references by hand |
| Delete a symbol | `safe_delete_symbol`; inspect the returned references if it refuses |
| Confirm a file is clean after editing | `get_diagnostics_for_file` with `min_severity=2` to drop hint noise |

Restrict any search with `relative_path` when the scope is known. Read whole files only for non-code — YAML,
JSON, Markdown, config — or when symbol operations have already been tried and were insufficient.

Prefer symbol operations strongly in strongly-typed languages, where LSP resolution is accurate. In dynamic
languages add `substring_matching=true`. For configuration and prose formats, go straight to Grep and Read.

At session start: `initial_instructions`, then `activate_project`, then `check_onboarding_performed`, then
`onboarding` if it returns false.

## When symbol tools cannot work

In a multi-language repository, language detection may fix the project's active language on the dominant one.
Symbol tools then fail for files of a secondary language, with an error naming the active languages and
refusing to extract symbols for the target. `get_current_config` confirms it.

**Treat this as an ongoing constraint of that repository, not a transient glitch to retry against.** Confirm it
once, then commit to the text-based path: locate definitions and references with Grep across source *and*
tests, edit with `replace_content` or the standard Edit tool, and verify with the language's own build or load
step — `get_diagnostics_for_file` is unavailable for an inactive language too.

## The active-project pointer is shared

Concurrent sessions can move it out from under each other. `edit_memory` returning not-found for a memory known
to exist, or `list_memories` returning a small unrelated set, is a routing problem rather than missing data: the
files on disk were never touched, only the pointer moved. Re-run `activate_project` with this session's own
absolute path, then retry.

**A subagent reporting "no relevant memory exists" during a parallel dispatch may be hitting the same
confusion. Do not treat that negative as authoritative** — acting on it means writing a duplicate of an entry
that already exists under the project the pointer drifted away from.

## Recovering a lost subagent report

Completion notifications for parallel subagents can be delayed or lost, so their absence is not evidence the
agent failed — and re-running on that assumption discards completed work and doubles the cost.

1. Look in the session directory's `subagents/` folder for the agent's `agent-*.jsonl` transcript. The final
   report survives there even when the notification did not arrive.
2. Check its mtime and tail. A recent mtime with a terminal assistant message means it finished; a stalled
   mtime mid-run means it did not.
3. Extract the report as the longest assistant text message — agent reports are substantially longer than the
   intermediate status notes around them.

The sibling `.meta.json` records only spawn-time configuration and no completion state, so it cannot answer
whether the agent finished. Read the transcript, not the metadata.

## Memory

### Naming

Names may contain `/` to organize into subtopics; `list_memories` enumerates nested paths transparently.

```
convention-{topic}      forward-looking project conventions
decision-{topic}        architectural decision records
review-{topic}-YYYY-MM  past investigation findings
{feature}-patterns      feature-specific reusable patterns
{domain}-patterns       domain-specific patterns
global/{topic}          shared across all projects — only when the user explicitly asks for
                        a project-independent memory
```

### What earns a memory

Write for: a significant architectural pattern, a reusable debugging insight from a hard bug, a reusable
implementation pattern, a convention or preference the user stated, a transferable refactoring approach.

Do **not** write:

- A note that names one file and would not change what you do in a different file. That is a commit message.
- Anything volatile enough to be wrong within weeks — line numbers, file counts, current status, an in-flight
  branch's state. **Volatility is the load-bearing exclusion**, because it rejects at write time exactly the
  entries the staleness check would otherwise have to catch later.
- Generic language or framework knowledge, and facts a single quick read would establish. A memory earns its
  place by preventing an expensive rediscovery, not by recording something true.
- One-off fixes, temporary preferences, workarounds due to be replaced, anything already documented elsewhere.

The positive trigger list is monotone — every extraction is "a refactoring approach", every fix is "a bug
insight" — so it can only ever argue for writing. A corpus grown from it alone becomes an index no agent can
afford to read, which pushes the next session toward reading nothing at all. Write dense agent notes:
invariants and terse bullets, rationale and worked examples omitted unless they prevent a likely mistake. **A
maintained corpus looks like a handful of short files; an accumulated one looks like dozens of long ones.**

### Frontmatter

```
---
domain: <nixvim | home-manager | ai-prompts | nix | general>
status: active
created: YYYY-MM
last-verified: YYYY-MM
---
```

`status`: active (current and verified), archived (superseded), draft (unverified hypothesis). On
`write_memory`, `last-verified` equals `created`. On `edit_memory`, bump `last-verified` and leave `created`.
Apply to new memories only — do not migrate retroactively, but add frontmatter when editing a memory that
lacks it.

### The body states the present, not a history

The frontmatter constrains metadata; these rules constrain what a reader actually loads.

- **A memory body describes the present state, not a change log.** If an addition invalidates something already
  written, rewriting or deleting that passage is part of the same edit. Appending looks like the safe move — it
  destroys no prior observation — but memories are read top-down under a context budget, so an append-only file
  becomes a document whose truth value decreases with reading order, and the part most likely to be read is the
  part most likely to be wrong.
- **A retraction goes in the lead.** When a task's observations contradict a memory, rewrite its opening line
  to say so before doing anything else with it. A retraction buried under sixteen dated update markers does not
  reach a reader who stops after the first paragraph. This is also what makes `status` expendable: if the lead
  always states current status, nothing depends on a taxonomy maintained separately. A status value that has
  never taken anything but its default across the whole corpus is decoration to route around, not a filter to
  trust.
- **Store the command, never the count.** Never write a figure that moves with the tree — test counts, file
  counts, lines of code, coverage percentage, dependency totals. It is stale one commit later, and a date stamp
  cannot protect it: `last-verified` is honest about when someone looked and useless for deciding whether to
  trust the number today. "The full unit suite is `<command>`, expected to report zero unexpected results"
  survives every commit that a number does not.
- **Record the re-verification command verbatim, next to the claim.** A date tells a later reader when someone
  was satisfied and offers only two options, trust or re-derive; the command makes re-verification a paste.
  Where the memory records an audit, add the commit or date it covered and the command revealing what has
  changed since, so a re-audit becomes a diff.
- **Name what decays fastest** — line numbers and counts always do — so a reader knows which sentences to
  re-check. A memory that does not distinguish its durable claims from its perishable ones gets discarded whole
  once any part is found wrong.
- **Record what constituted done** for the area: the commands that had to pass, and any non-zero output
  accepted as normal. That accepted-warning detail is written nowhere else, and without it the next agent reads
  a pre-existing warning as a fresh regression.
- Where a figure genuinely must be recorded, write it as an observation made at a stated time by a stated
  command — "reported N at `<date>` via `<command>`" ages honestly; "the suite contains N tests" does not, and
  a corpus accumulates several mutually contradictory values of N, each confident and each correct when
  written.

### Editing hygiene

Each of these has a recorded failure behind it, and each failure is silent: the edit reports success and the
damage is visible only to a later reader.

- Read the whole memory before editing, not just the region being changed. Editing from a partial view is how a
  second complete frontmatter block ends up stacked on the first, after which a consumer parsing the first
  block and one parsing the last get different answers.
- After the edit, the file must contain exactly one frontmatter block. Check it rather than assuming it.
- Any programmatic in-place substitution whose replacement contains a metacharacter must be verified by
  *reading the result*, not by the command's exit status. A replacement with a capture-group reference and no
  corresponding group substitutes the empty string silently and truncates the sentence; a replacement written
  through a layer that does not interpret escapes emits the escape sequence literally. Neither fails loudly,
  and both destroy content in a file nobody will re-read.

### Finding the duplicate before writing

"Check `list_memories` first" stops working the moment the namespace is split by domain prefix — which is
exactly when the corpus is large enough for duplication to matter. Working inside one domain, the natural name
for a new memory carries that domain's prefix, so the identical fact filed under a different domain never comes
into view. The bodies diverge in vocabulary too, so neither a name scan nor a grep for the obvious term finds
it. The observed cost is a fact recorded in seven places where the sum carries less information than the best
single copy — each partial, and the decisive detail present in only one.

- **Search by the words describing the symptom**, not the words you would use to name the file. "Tests run
  stale logic", "the edit did not take effect" — a reader hits the memory through the problem they are having,
  never through the taxonomy someone else chose.
- A fact that crosses domains goes in one cross-cutting place, not under whichever domain happened to hit it
  first.
- When you find the duplicate, merge rather than adding another copy, and keep the detail that appears in only
  one of them. That detail is usually the reason the memory is worth having.

### Which memories to read

| Task type | Priority |
|---|---|
| Investigation | `{domain}-patterns`, `architecture-*`, `{project}-conventions` |
| Implementation | `{feature}-patterns`, `{language}-conventions`, `testing-patterns` |
| Review | `{project}-conventions`, `code-quality-*`, `architecture-*` |
| Refactoring | `architecture-*`, `{component}-patterns`, `testing-patterns` |

### The corpus as a graph, not a list

At small scale a flat set is fine, because listing it is cheap. Past a few dozen memories, an agent that must
read everything to find what matters pays that cost on every task, so the corpus needs to be traversable: one
designated root that every session reads first and that holds no domain detail of its own, domain memories
linked from it, and leaf memories reached by traversal rather than by scanning the index.

- A reference carries a description of what the target covers, precise enough to decide whether to follow it.
  The target's name alone tells you the topic, not whether the content bears on the question in hand.
- **A memory should not contain instructions about when to read itself.** That guidance belongs to the
  referrer, the only place with the context to judge relevance. Self-describing read conditions duplicate
  across every referrer and go stale independently.
- Add the reference from the parent in the same edit that adds the memory. An unreferenced memory is
  unreachable by traversal and effectively invisible, however good its content.
- When writing a link, confirm the target exists, or say in the same line that it is a placeholder. **A
  dangling reference reads exactly like a valid one until someone follows it**, so the graph degrades silently.
- When a task finds itself reading more than a handful of memories on one topic, that is the signal to write
  the linking entry that gathers them — not to add another leaf. A cluster held together only by a shared
  filename prefix is not a graph.

The task-type table above is the selection heuristic for a flat corpus or when no root exists; the graph is how
a corpus is navigated once one does. Where both apply, start at the root and let the task type decide which
branches to follow.

### Staleness, checked lazily

Freshness is maintained opportunistically, against **only the memories a task actually read**. Never read a
memory solely to check its freshness — that turns every task into an index sweep, charged to a task that never
needed it. There is deliberately no scheduled full-index pass: a memory nothing has read in months is also a
memory nothing has needed. Consolidating the index is user-initiated.

Signals: `last-verified` more than three months old; frontmatter absent entirely (treat as a stale candidate
and add frontmatter when editing). But **for a memory naming something checkable — a symbol, a file, a path, a
condition — the test that matters is whether the thing it names still exists, not how old the memory is.** Those
have different answers: a memory verified two months ago can name a symbol deleted last week and pass the date
gate untouched. Checking costs one search, far less than the review round that re-derives the item and re-files
it as an action. Apply this especially to any memory carrying forward a deferred work item, which gets
re-proposed on the strength of a name that may no longer resolve.

**Bumping `last-verified` is one edit; verifying content requires reading the code.** If only the first
happens, the stamp comes to mean "recently touched" rather than "checked" — worse for a reader than no stamp at
all, since an unmarked memory invites suspicion and a freshly-dated one invites trust. State what was compared
against what, naming a file path or command output, whenever the date is bumped.

Partial re-verification is the normal case, not the exception. Record the boundary of what was checked in the
body, naming both sides: bumping the date for the whole file after confirming one section lends false freshness
to everything else; not bumping at all sends the confirmed part back through verification next time. Record a
discrepancy found during a scoped check *at the moment it is found*, even when it lies outside what the task
set out to verify — noting that a named function no longer exists costs a line and closes the finding, while
deferring it means an independent investigation later that starts from nothing.

Outcomes: still accurate → bump; partially outdated → correct the stale section and bump; fully superseded →
`rename_memory` with an `-archived` suffix and note the reason.

For a memory whose value is mixed — some claims hold, others are dead — correcting in place erases the audit
trail and archiving discards what was confirmed. Instead keep the file and put a dated banner at the top
stating which claims still hold and explicitly invalidating the point-in-time facts as historical. A later
session quoting a stale number is stopped when it opens the file, rather than after it has built on the number.
The banner also carries status in the text a reader actually loads, which a metadata field does not.

A rename reaches only whoever consults the index next. It leaves nothing for the session that already loaded
the old memory, and nothing for the memories citing it — so **the superseding memory names the old one and
states what it wrongly claimed.** Recording the wrong claim, not just the name, is what lets a reader match it
against what they remember reading. Where the old claim is likely to be re-derived from the same code, keep it
visible with its correction and date rather than deleting it: a silently overwritten memory means the next
session re-derives the superseded claim, reaches the same wrong conclusion, and has no way to know it has been
here before. This holds with particular force for a recorded rejection — "we evaluated this and decided
against it" — which is stored with its reasoning, not just its verdict, because a rejection goes stale when its
premise is invalidated even though its conclusion still sounds right.

When several small memories cover related topics, merge them into one.

## Rules

- Check `list_memories` before implementing, and use `edit_memory` for an existing topic — `write_memory` only
  for a genuinely new one.
- Never `delete_memory` without an explicit user request.
- Never read an entire code file when symbol operations would do.
- Never update symbol references by hand when `rename_symbol` exists.
- Start `get_symbols_overview` at depth 0 and increase only if needed.

## Extending Serena to a new language

An LSP-integration task, not a parser-writing task. Wrap an existing language server — prefer the
single-core-dependency provider when the server is one executable or archive. Register the language in the
`Language` enum with a filename matcher for its extensions and add the factory branch. Provide a minimal test
repository exercising symbols, within-file references, and cross-file references.

**Write tests asserting the actual expected symbol names and reference locations.** Asserting only that a
non-empty list came back is insufficient and is the most common reason such contributions are rejected.

`documentSymbol`, `definition`, and `references` are mandatory: a server missing any of them cannot back
`find_symbol` or `find_referencing_symbols` and should be deferred, or offered only as experimental
completion/hover support. Installation footprint determines adoption cost and CI feasibility.

## Related

- [investigation-patterns](../investigation-patterns/SKILL.md) — investigation methodology using these tools
- [core-patterns](../core-patterns/SKILL.md) — shared escalation and decision-criteria templates
- [execution-workflow](../execution-workflow/SKILL.md) — where memory checks sit in the orchestration
