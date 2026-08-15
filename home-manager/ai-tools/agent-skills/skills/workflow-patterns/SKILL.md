---
name: workflow-patterns
description: Patterns for output formats, reflection checkpoints, agent references, and self-evaluation shared across agents and commands. Use when authoring an agent or command, not when executing one.
version: 4.0.0
---

Structures for authoring agents and commands. Those files are XML, so the templates below are given in that
form even though this file is markdown.

**Templates follow their consumers.** Two prescribed here previously had none: an `<agent ref="...">`
reference syntax (every command uses `subagent_type=` instead) and a per-agent `<parallelization>` block. Both
are gone. Before adding a template, check that something will actually carry it.

## Output

Every finding carries the evidence that backs it, so a reader can check the report rather than trust a number
it asserts about itself.

- **status** — success | warning | error
- **summary** — what was asked, what was found, what remains unchecked
- **verification** — the exact command(s) run and their exit status, or "none run". Never omitted.
- **findings** — each with its claim, its tier (verified | inferred | assumed), the file:line or command
  behind it, and the detail
- **gaps** — anything asked for that was not done, and why
- **next_actions**

`gaps` is not optional. **An empty list is a claim that nothing was left undone, and it is checkable; omitting
the field hides the question.**

### Where the status criteria live

The resident configuration defines status and the output contract, and it is in context on every request — so
a command or agent names it rather than restating it, and the reference resolves.

This is the one exception to the rule that a pointer delivers nothing. That rule holds for *skill* references,
because a skill body is absent until an explicit Skill call loads it: pointing at core-patterns for the status
definitions leaves the field asserting a standard neither writer nor reader ever saw. The distinguishing
condition is simply **whether the target is resident**. Resident: name it. Not resident: write it in, or load
it first.

## Checkpoints

A gate between phases, cleared by an artifact — a path, a command, a name — so that failing it is visible in
the transcript.

```xml
<reflection_checkpoint id="analysis_quality">
  <gate>Per gate_discipline in CLAUDE.md.</gate>
  <check>Name the files read and the specific lines the conclusion rests on.</check>
  <check>Name what is still unknown, or state that nothing material is.</check>
  <on_unmet>Gather the missing evidence before proceeding. If only the user can supply it, ask with
    AskUserQuestion rather than assuming.</on_unmet>
</reflection_checkpoint>
```

**Phrase every check so it can fail.** "Have I gathered sufficient evidence?" cannot — it is answered yes by
whatever evidence was gathered. "Name the files read" can.

## The prepare phase

Load the governing skill first, because every step after it is written against guidance that has not arrived
yet. Then initialize Serena and read only the memories the task type calls for.

```xml
<phase name="prepare">
  <step order="1">
    <action>Load the skill this workflow depends on with the Skill tool, and serena-usage if the
      workflow performs memory or symbol operations.</action>
    <tool>Skill</tool>
    <output>The skills loaded, by name</output>
  </step>
  <step order="2">
    <action>Activate the project, list memories, and read only the entries matching this task type.</action>
    <tool>Serena activate_project, list_memories, read_memory</tool>
    <output>The memories read, or an explicit "nothing matched"</output>
  </step>
</phase>
```

**Name the loaded skills in the output.** "Loaded the governing skill" with no name is not checkable against
the transcript, which is the only thing distinguishing a real load from an intention to load.

Scale the preamble to the work. A memory read that returns nothing useful still costs a round trip before any
work starts, so a lookup-shaped task reads the index and stops there.

## Self-evaluation

A final pass that looks for **what is missing** from the report — something a model can actually do — rather
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

## A convention is not adopted until a gate enforces it

A rule that lives only in a document is advisory, and **it erodes at the rate new code is written.** The
definition of done for adding a convention includes its enforcement mechanism, not just the prose.

- Style and idiom → the project's formatter and linter configuration, not review comments.
- Import and layering constraints → a dependency or import-boundary checker.
- Dead exports and unreachable code → a detector, so a removal convention stays true over time.
- Rules no off-the-shelf tool knows → a test in the normal suite. See
  [quality-tools](../quality-tools/SKILL.md) for authoring one without it becoming noisy.

If a rule cannot be mechanically checked, reconsider stating it: an unenforceable rule costs review attention
on every change and buys compliance only while someone remembers it.

Adding "all new modules must declare explicit exports" is *not* done when the rule is written in the
conventions document. It is done when the rule is written **and** a lint rule fails on a module that violates
it.

## Related

- [core-patterns](../core-patterns/SKILL.md) — decision criteria, evidence tiers, and the escalation shape
- [parallelization-patterns](../parallelization-patterns/SKILL.md) — independence, consensus, and retry
- [serena-usage](../serena-usage/SKILL.md) — the memory and symbol operations the prepare phase performs
- [quality-tools](../quality-tools/SKILL.md) — the gates that make a convention enforceable
