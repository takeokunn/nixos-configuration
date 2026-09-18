---
name: workflow-patterns
description: Patterns for output formats, reflection checkpoints, agent references, and self-evaluation shared across agents and commands. Use when authoring an agent or command, not when executing one.
metadata:
  version: "4.0.0"
---

Structures for authoring agents and commands. Those files are markdown with YAML frontmatter, and the templates
below are given in that form.

Before adding a template, identify its consumer and verify that the runtime supports its fields.

## Output

Every finding carries the evidence that backs it, so a reader can check the report rather than trust a number
it asserts about itself.

- **status**: success | warning | error
- **summary**: what was asked, what was found, what remains unchecked
- **verification**: the exact command(s) run and their exit status, or "none run". Never omitted.
- **evidence**: each finding with its claim, its tier (verified | inferred | assumed), the file:line or command
  behind it, and the detail
- **gaps**: anything asked for that was not done, and why; omit only when empty

Add next actions only when they help the caller decide what to do.

### Where the status criteria live

CLAUDE.md defines status and the output contract, and it is in context on every request, so
a command or agent names it rather than restating it, and the reference resolves.

This is the one exception to the rule that a pointer delivers nothing. That rule holds for *skill* references,
because a skill body is absent until it is loaded through the runtime's skill mechanism or read in full:
pointing at core-patterns for the status
definitions leaves the field asserting a standard neither writer nor reader ever saw. The distinguishing
condition is simply **whether the target is resident**. Resident: name it. Not resident: write it in, or load
it first.

## Checkpoints

A gate between phases, cleared by an artifact (a path, a command, a name) so that failing it is visible in
the transcript.

```markdown
### Checkpoint on analysis quality

Per gate_discipline in CLAUDE.md. Name:

- The files read and the specific lines the conclusion rests on.
- What is still unknown, or that nothing material is.

Unmet: gather the missing evidence before proceeding. If only the user can supply it, ask with AskUserQuestion
rather than assuming.
```

Place it where it belongs in the sequence: a checkpoint between two phases goes between them, and a heading
between numbered items means the surrounding workflow uses `### Phase` subsections rather than one flat list.

**Phrase every check so it can fail.** "Have I gathered sufficient evidence?" cannot: it is answered yes by
whatever evidence was gathered. "Name the files read" can.

## The prepare phase

Load the governing skill first, because every step after it is written against guidance that has not arrived
yet. Then initialize Serena and read only the memories the task type calls for.

```markdown
### Prepare

1. Load the skill this workflow depends on through the runtime's skill mechanism, or read its SKILL.md in full
   when that mechanism is unavailable. Load serena-usage for memory or symbol operations. Return the skills
   loaded, by name.
2. Activate the project, list memories, and read only the entries matching this task type, using Serena's
   activate_project, list_memories and read_memory. Return the memories read, or an explicit "nothing matched".
```

**Name the loaded skills in the output.** "Loaded the governing skill" with no name is not checkable against
the transcript, which is the only thing distinguishing a real load from an intention to load.

Scale the preamble to the work. A memory read that returns nothing useful still costs a round trip before any
work starts, so a lookup-shaped task reads the index and stops there.

## Self-evaluation

A final pass that looks for **what is missing** from the report (something a model can actually do) rather
than rating what is present, which it cannot.

1. Re-read the report and tag each finding. Any tagged verified must name the command or file:line behind it;
   if it cannot, downgrade it.
2. List anything the request asked for that the report does not answer, and why: not attempted, blocked, or
   judged out of scope.
3. Set the status from what those two found, and name **the weakest claim** with what would confirm it.

"Which part of this is most likely wrong" has an answer the model can find; "how good is this out of 100" does
not.

## Failure handling

Give the exceptional paths their own steps rather than leaving them implicit: a failed tool call retries the
stated alternative once and then reports the blocker by name; unavailable data is documented as a gap and the
analysis continues within a stated bound; contradictory evidence is surfaced as a question rather than averaged
into a hedge.

## Mechanical conventions need enforcement

For a mechanically decidable convention, define its enforcement mechanism as well as its prose. Distinguish
automated gates from policies that require human or agent judgment.

- Style and idiom → the project's formatter and linter configuration, not review comments.
- Import and layering constraints → a dependency or import-boundary checker.
- Dead exports and unreachable code → a detector, so a removal convention stays true over time.
- Rules no off-the-shelf tool knows → a test in the normal suite, scanning emitted output (format strings,
  generated text) rather than whole-file text, since a whole-file scan flags a comment or doc line that merely
  describes the old idiom as if it were the idiom itself.

A check built as regex over source text cannot distinguish a real violation from an identifier, comment, or
string literal that only mentions it, so treat a hit as evidence to investigate rather than proof of one, and
avoid naming local identifiers after whatever the check forbids, in directories it governs. A gate is only
worth having while its precision keeps it trusted: a check that produces false positives loses that trust
faster than it earns it, and one reviewers have learned to skip past is worse than no gate at all.

For a rule that requires judgment, name who checks it and what evidence they must provide. Do not describe it
as mechanically enforced.

Adding "all new modules must declare explicit exports" is *not* done when the rule is written in the
conventions document. It is done when the rule is written **and** a lint rule fails on a module that violates
it.

## Related

- [core-patterns](../core-patterns/SKILL.md): decision criteria, evidence tiers, and the escalation shape
- [parallelization-patterns](../parallelization-patterns/SKILL.md): independence, consensus, and retry
- [serena-usage](../serena-usage/SKILL.md): the memory and symbol operations the prepare phase performs
