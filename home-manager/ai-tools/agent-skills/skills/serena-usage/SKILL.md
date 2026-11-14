---
name: serena-usage
description: Use for Serena MCP work - symbol search, references, navigation, substitutes for languages Serena does not support, and reading or writing memory in the repo or global Serena store, also by file when Serena is down. Also covers recovering a lost subagent report.
metadata:
  version: "5.0.0"
---

Serena's tool schemas are injected by the harness and are not restated here. This file covers tool choice,
the failure modes that make a Serena result misleading, where memory lives, and how a memory corpus stays
worth reading. Use the injected catalog as the authority; if an operation is absent, name the limitation and
use a scoped text-based substitute rather than inventing a tool call.

## Tool choice

Serena owns code intelligence; standard tools own the filesystem.

| Need | Tool |
|---|---|
| Find files by name | `aitools find` |
| Search file contents | `aitools search` for discovery, then Serena for navigation |
| File structure | `get_symbols_overview`: depth 0 first, then depth 1 |
| A named function, class, or method | `find_symbol`; `substring_matching=true` when the name is uncertain |
| Its implementation | `find_symbol` with `include_body=true` |
| A definition, from a call site | `find_declaration` with a regex capturing the symbol: for `obj.process(x)`, pass `"obj\.(process)\("` |
| Implementations of an interface | `find_implementations` (`relative_path` must be a file, not a directory) |
| Callers and impact | `find_referencing_symbols` |
| Replace a whole symbol | `replace_symbol_body` |
| Change a few lines inside one | `replace_content`, regex mode with wildcards (`"beginning.*?end"`) to avoid transcribing large sections. DOTALL and MULTILINE are on; backreferences are `$!1`, `$!2` |
| Rename across the codebase | `rename_symbol`: never update references by hand |
| Delete a symbol | `find_referencing_symbols` first; delete only when no reference remains |
| Confirm a file is clean after editing | `get_diagnostics_for_file` with `min_severity=2` to drop hint noise |

Restrict any search with `relative_path` when the scope is known. Read a whole code file only when symbol
operations were tried and were insufficient, or the language is not registered (next section).

Prefer symbol operations strongly in strongly-typed languages, where LSP resolution is accurate. In dynamic
languages add `substring_matching=true`. For configuration and prose formats, go straight to `aitools search`
and a file read.

### Session start

Before the first symbol operation in a session: `initial_instructions`, then `activate_project`. For a
project root inside `.worktrees/`, the repository root is the path before `.worktrees/`, and its memory is
the shared store described under "Where memory lives". Skip `check_onboarding_performed` and `onboarding` for
that worktree. For other project roots, check onboarding (`check_onboarding_performed`, then `onboarding` if
it returns false). Call `list_memories` only when prior knowledge could change the work.

## Languages

### List every language in `project.yml`

When Serena creates a project, it registers only the top detected language, ranked by file count, in
`project.yml`'s `languages` field; the others are offered only in an interactive prompt. Symbol tools then
work for that one language and fail or come back empty for the rest. When you set up or repair a project,
list every language the repository uses under `languages`. The first entry is the default and its server is
the fallback for files no other listed server claims.

### Languages Serena does not support

Serena has no language server for Common Lisp, Emacs Lisp, Org, or SQL. Listing them in `languages` does not
help; use these instead:

| Need | Substitute |
|---|---|
| Definitions in one file | `aitools code outline` (Common Lisp and Emacs Lisp), `aitools org outline` (Org) |
| Where a name is defined or mentioned | `aitools code defs`, `aitools code refs`, `aitools search`, or grep; these match text, not resolved bindings, so search tests too |
| Structural edits to Lisp | paredit-cli skill (`paredit`); `aitools check` for delimiter balance |
| SQL | `aitools search` or grep, then a file read |
| Verification | the language's own build, load, or test step; `get_diagnostics_for_file` does not cover these files |

Language-specific procedures: common-lisp-ecosystem and emacs-lisp-ecosystem skills.

### When symbol tools return nothing

A supported language missing from `languages` can fail quietly: the symbol tools return nothing rather than
an error, and an empty `find_symbol` then reads like a symbol that does not exist. The conclusion drawn from
it ("no references, safe to delete") is wrong in the most expensive direction. Before reading an empty
result as an absence, read `get_current_config` for the active languages and query a symbol you know exists
in the same file type. If that is empty too, fix `languages` if you are allowed to change the project's
configuration; otherwise use the text-based substitutes above and say which claims rest on them.

Failure can also be per-symbol. When a concurrent session is changing the tree, one lookup can succeed and
the next come back empty in the same session. That points to the concurrent change, not to the repository:
name the substitute for the symbol that failed instead of abandoning symbol operations.

## The active-project pointer is shared

Concurrent sessions can move it out from under each other. `edit_memory` returning not-found for a memory
known to exist, or `list_memories` returning an unrelated set, can indicate a routing problem. Inspect the
active project, re-run `activate_project` with this session's own absolute path, then retry. If the expected
memory is still absent, read the store directory directly; pointer drift alone does not prove the data is
intact.

**A subagent reporting "no relevant memory exists" during a parallel dispatch may be hitting the same
confusion. Do not treat that negative as authoritative**: acting on it means writing a duplicate of an entry
that already exists under the project the pointer drifted away from.

## Recovering a lost subagent report

Completion notifications for parallel subagents can be delayed or lost, so their absence is not evidence the
agent failed, and re-running on that assumption discards completed work and doubles the cost.

1. Check the runtime's agent status and queued messages first.
2. If the runtime exposes session transcripts, locate the actual transcript path. Claude Code's
   `subagents/agent-*.jsonl` layout is not a portable assumption for Codex.
3. Read the terminal status and final report in that runtime's transcript format. Message length and file
   mtime do not establish completion: a long intermediate message is not a final report, and a quiet file may
   belong to an agent waiting on a tool.

If neither status nor a transcript establishes completion, report it as unknown before deciding whether to
retry. Inspect possible partial edits before re-dispatching a write-capable agent.

## Memory

The policy (what to record, what never to record, read-only work) is CLAUDE.md `memory_policy`. This section
covers where memory lives, which store a new memory goes to, and how a memory stays correct.

### Where memory lives

Memory is in Serena only; Claude Code's auto-memory is disabled, so no `MEMORY.md` index is loaded and none
should be written. There are two stores:

| Store | Memory name | Directory |
|---|---|---|
| Repository | `<topic>` or `<subtopic>/<topic>` | the project's `.serena/memories` (for a bare clone registered at `<repo>.git/.state`, that is `<repo>.git/.state/.serena/memories`) |
| Global | `global/<topic>` | `memories/global` under Serena's home (`~/.serena` unless `SERENA_HOME` overrides it) |

A name maps to `<directory>/<name>.md`, with `/` becoming subdirectories. Managed worktrees link the
repository store at `.serena/memories`; if the link is absent, use the shared directory.
`list_memories` without a topic returns both stores.

**When Serena is unreachable, read the files directly** from those directories: list them with
`aitools find`, search bodies with `aitools search`, and read the matches. A store you could not reach is
not evidence that no memory exists; say which store you could not read. Writes wait for Serena or go back to
the parent as candidates, under `memory_policy`.

### Repository store or global

- **Repository store** for knowledge that holds only in that repository: its layout, its gates and their
  traps, its decisions. Use it only when the repository is checked out locally; Serena resolves the store
  from the checkout.
- **Global store** for knowledge that holds across repositories: tooling, Nix, jj, agent-runtime behavior,
  the user's preferences. This applies even when the lesson was learned inside one repository.
- When a lesson concerns a repository that is not checked out, put only the part that holds elsewhere into
  global. When a repository was moved or renamed, use the store of its current checkout.
- One fact lives in one store. When the other store needs it, link to it by memory name instead of copying
  the body.

### Update before creating

Search both stores before writing (see "Finding the duplicate before writing"). Use `edit_memory` when an
existing memory covers the topic, even partly, and `write_memory` only for a genuinely new one. When two
memories say the same thing, merge them into one and keep the detail that appears in only one copy. When
several small memories cover related topics, merge them into one.

### What does not earn a memory

Beyond `memory_policy`'s exclusions, do not write:

- Session diffs, progress logs, review chronology, verdicts, or scores. A deferred-finding ledger may keep
  an identifier, location, severity, and deferral reason, not a transcript.
- Anything readable from the code or the VCS history, or from a single quick read. A memory earns its place
  by preventing an expensive rediscovery, not by recording something true.
- Superseded content: deleted tools, finished one-off tasks, fixed bugs with no recurring lesson. Correct or
  archive an existing memory that has become this (see "Staleness, checked lazily").
- Anything volatile enough to be wrong within weeks: line numbers, file counts, current status, an in-flight
  branch's state. **Volatility is the load-bearing exclusion**, because it rejects at write time the entries
  the staleness check would otherwise have to catch later.
- A note that names one file and would not change what you do in a different file. That is a commit message.
- A memory that contradicts the current CLAUDE.md. Follow the rule; a memory does not override it.

What does earn one is a concrete lesson or decision an agent cannot infer and that still holds: a trap, a
non-obvious constraint, a user decision with its reason and the rejected options, a pointer to an external
resource. Write dense agent notes: invariants and terse bullets, rationale and worked examples only where
they prevent a likely mistake. **A maintained corpus looks like a handful of short files; an accumulated one
looks like dozens of long ones.**

### A rule is a prompt candidate, not a memory

Content that tells every future session how to behave (a behavior instruction rather than a fact) belongs in
the prompts, skills, or CLAUDE.md, not in memory. Report it as a candidate addition and name the target file;
CLAUDE.md `hard_rules` decides when it is written. Do not keep a competing rule in memory meanwhile.

### Naming

Name a memory by its durable topic. Names may contain `/` for subtopics; `list_memories` enumerates nested
paths.

```
convention-{topic}      forward-looking project conventions
decision-{topic}        decision records, with the rejected options
{feature}-patterns      feature-specific reusable patterns
{domain}-patterns       domain-specific patterns
global/{topic}          the global store
```

Never put a date, a phase, or a review round in a name: `deduplication-phase2-2026-01` records when someone
worked, which is chronology this store excludes. Name the topic and let `created` hold the date.

### Frontmatter

```
---
domain: <topic area; reuse a value already used in that store>
status: active
created: YYYY-MM
last-verified: YYYY-MM
---
```

`status`: active (current and verified), archived (superseded), draft (unverified hypothesis). On
`write_memory`, set `last-verified` only for content checked against current evidence. On `edit_memory`,
leave `created` unchanged and bump `last-verified` only for reverified content, recording the checked scope.
Add frontmatter when editing a memory that lacks it.

### The body states the present, not a history

This store is a cache of current truth, read top-down under a context budget, so the part most likely to be
read must be the part most likely to be right.

- **Correct in place.** If an addition invalidates something already written, rewriting or deleting that
  passage is part of the same edit; an append-only memory grows less true with reading order.
- **A retraction goes in the lead.** One buried under dated update markers does not reach a reader who stops
  after the first paragraph.
- **A correction reaches every sibling carrying the same claim.** Search the stale claim's distinctive words
  in both stores before finishing, and quote the wrong claim verbatim while negating it, so a reader who
  half-remembers it finds the file that settles it.
- **Store the command, never the count.** Counts, coverage, and line numbers are stale one commit later, and
  `last-verified` cannot protect them. Put the re-verification command next to each claim, behavioural ones
  included. Keep absolute paths out of bodies.
- **Record what constituted done**: the commands that had to pass, and any non-zero output accepted as
  normal. Without the accepted-warning detail, the next agent reads a pre-existing warning as a regression.

### Editing hygiene

Each of these failures is silent: the edit reports success and the damage is visible only to a later reader.

- Read the whole memory before editing, not just the region being changed. Editing from a partial view is
  how a second frontmatter block ends up stacked on the first, after which two consumers parse different
  answers.
- After the edit, the file must contain exactly one frontmatter block. Check it rather than assuming it.
- Verify any programmatic substitution whose replacement contains a metacharacter by reading the result,
  not by the exit status. A capture-group reference with no corresponding group substitutes the empty string
  and truncates the sentence; a layer that does not interpret escapes emits them literally.

### Finding the duplicate before writing

A name scan stops working once the namespace is split by prefix: the natural name for a new memory carries
the current domain's prefix, so the same fact filed under another domain, or in the other store, never comes
into view. Duplicates can each carry only part of the evidence.

- **Search by the words describing the symptom**, not the words you would name the file with. A reader
  reaches a memory through the problem they are having, not through the taxonomy.
- A fact that crosses domains goes in one cross-cutting place, usually the global store, not under whichever
  domain hit it first.
- **Record that the search happened and came back empty** in the memory you then write, so a later reader
  can tell a new fact from one whose author never looked.

### Point at an external standard rather than copying it

A convention owned outside the repository (an organization-wide policy, a sibling repository's standard) is
named and located, never restated: a copy stops tracking its original the day either changes. Give the
document's name and the command that fetches it. Where the project's own docs already restate such a policy,
say so in the memory, since those restating sites are the ones that go stale.

### The corpus as a graph, not a list

Past a few dozen memories, an agent that must read everything to find what matters pays that cost on every
task, so the corpus needs to be traversable: one root that every session reads first and that holds no
domain detail of its own, domain memories linked from it, and leaf memories reached by traversal.

- A reference carries a description of what the target covers, precise enough to decide whether to follow
  it.
- **A memory should not contain instructions about when to read itself.** That belongs to the referrer, the
  only place with the context to judge relevance.
- Add the reference from the parent in the same edit that adds the memory. An unreferenced memory is
  unreachable by traversal.
- When writing a link, confirm the target exists, or say in the same line that it is a placeholder. **A
  dangling reference reads exactly like a valid one until someone follows it.**
- When a task reads more than a handful of memories on one topic, write the linking entry that gathers them
  rather than another leaf. A cluster held together only by a filename prefix is not a graph.

Without a root, read only the memories matching the task type; with one, start at the root and let the task
type decide which branches to follow.

### Staleness, checked lazily

Freshness is maintained against **only the memories a task actually read**. Never read a memory solely to
check its freshness, and do not run a scheduled full-index pass; consolidating the index is user-initiated.

Signals: `last-verified` more than three months old; frontmatter absent. But **for a memory naming something
checkable (a symbol, a file, a path, a condition), the test that matters is whether the thing it names still
exists, not how old the memory is.** A memory verified two months ago can name a symbol deleted last week.
Apply this especially to a memory carrying forward a deferred work item, which gets re-proposed on the
strength of a name that may no longer resolve.

**Bumping `last-verified` is one edit; verifying content requires reading the code.** State what was
compared against what, naming a file path or a command, whenever the date is bumped. When only part was
checked, record the boundary in the body, naming both sides. Record a discrepancy found during a scoped
check when it is found, even outside what the task set out to verify.

Outcomes: still accurate, bump; partially outdated, correct the stale section and bump; fully superseded,
`rename_memory` with an `-archived` suffix and note the reason. For a memory of mixed value, keep the verified
claims, replace stale ones in place, and state which earlier claim the correction negates.

A rename reaches only whoever consults the index next, so **the superseding memory names the old one and
states what it wrongly claimed.** Where the old claim is likely to be re-derived from the same code, keep it
visible with its correction. A recorded rejection keeps its reasoning, not just its verdict: it goes stale
when its premise is invalidated, even though its conclusion still sounds right.

Never `delete_memory` without an explicit user request.

## Extending Serena to a new language

An LSP-integration task, not a parser-writing task. Wrap an existing language server: prefer the
single-core-dependency provider when the server is one executable or archive. Register the language in the
`Language` enum with a filename matcher for its extensions and add the factory branch. Provide a minimal test
repository exercising symbols, within-file references, and cross-file references.

**Write tests asserting the actual expected symbol names and reference locations.** Asserting only that a
non-empty list came back is insufficient and is the most common reason such contributions are rejected.

`documentSymbol`, `definition`, and `references` are mandatory: a server missing any of them cannot back
`find_symbol` or `find_referencing_symbols` and should be deferred, or offered only as experimental
completion/hover support. Installation footprint determines adoption cost and CI feasibility.

## Related

- [investigation-patterns](../investigation-patterns/SKILL.md): investigation methodology using these tools
- [execution-workflow](../execution-workflow/SKILL.md): where memory checks sit in the orchestration
- [common-lisp-ecosystem](../common-lisp-ecosystem/SKILL.md) and
  [emacs-lisp-ecosystem](../emacs-lisp-ecosystem/SKILL.md): the languages Serena cannot index
