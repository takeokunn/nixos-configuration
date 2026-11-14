---
name: workflow-patterns
description: Use when authoring an agent, a command, or a skill description (output, checkpoints, decision criteria, escalations, skill loading, self-evaluation), when escalating a consequential finding into an independent refutation pass, or when two instructions appear to contradict and must be reconciled.
metadata:
  version: "5.3.0"
---

Structures for authoring agents and commands, plus the patterns that keep them honest. Those files are markdown
with YAML frontmatter, and the templates below are given in that form.

Before adding a template, identify its consumer and verify that the runtime supports its fields. **These
templates follow their consumers rather than leading them.** When the corpus changes shape, this file changes
with it: a template prescribing a structure no consumer uses is worse than none, because it invites someone to
reintroduce it.

## Evidence-informed prompt changes

Start with an observed failure or a requested behavior, not a longer rule set. Check relevant project memories
against current source; a past verdict is not an oracle. Keep universal contracts resident, task procedures
in triggered skills, and runtime-specific capabilities in adapters.

(Claude Code only.) The `cclens` command analyzes Claude Code session logs; it does not observe Codex sessions.
Before using its counts to remove a skill, establish the analysis window, analyzer version, source freshness,
runtime coverage, and parent/subagent coverage. Distinguish a Skill tool invocation from a body loaded by a
file read. A missing record means unobserved use, not proven non-use. The
[session-format specification](https://github.com/lambdalisue/cclens/blob/main/docs/specs/session-format.md)
describes what the analyzer can observe; verify the installed implementation before relying on newer fields.

Measure the catalog actually emitted for each client, separately from resident instructions and dynamically
loaded bodies. Source-tree byte counts and invocation counts do not measure session token cost or task quality.
Do not prune a required safety or verification skill merely because its observed use is rare.

A skill's `description` is resident in every session's catalog, and its body is not. Open the description with
the condition under which the skill applies, in the words a user request or a symptom would use, and spend the
rest on what separates it from neighboring skills.

For a behavior-changing instruction, compare the old and new versions on a representative observed failure
and an unaffected control task. Declare the criterion first, such as correct scope, preserved authorization,
or valid verification, and check for regressions. Hold task inputs, runtime, model, and available tools fixed
where possible; report differences and remaining confounders rather than a self-assigned quality score.
The [Codex prompting guide](https://developers.openai.com/cookbook/examples/gpt-5/codex_prompting_guide#troubleshooting--metaprompting)
and [Claude Code best practices](https://code.claude.com/docs/en/best-practices#write-an-effective-claudemd)
support iteration against observed behavior, not copying every vendor's instructions into every runtime.

Inspect the generated client artifacts as well as source text. Formatting, evaluation, and packaging checks
establish delivery, not better agent behavior. If a behavior comparison cannot be run, name that gap and keep
the proposed improvement explicitly unmeasured.

Before a comparison run, check that the skill's `description` and its body cover the same scope; when they
differ, the run measures an undefined scope. Fix the pass/fail
checklist before the first run, with at least one item marked critical, and do not relabel items after seeing
results. Classify each confusion a run reports by the phase where it arose: understanding, planning, execution,
or formatting. A scenario that needs several times the tool calls of its peers signals a prompt that is not
self-contained. Compare two versions in both presentation orders and adopt a change only when both orders
agree; when only one order could run, the result stays unmeasured.

## Output

A command or agent writes "Use output_contract" and does not restate the fields: CLAUDE.md is in context on
every request, so the reference resolves.

This is the one exception to the rule that a pointer delivers nothing. That rule holds for *skill* references,
because a skill body is absent until it is loaded through the runtime's skill mechanism or read in full:
pointing at a skill for the status definitions leaves the field asserting a standard neither writer nor reader
ever saw. The distinguishing condition is simply **whether the target is resident**. Resident: name it. Not
resident: write it in, or load it first.

Statuses and evidence tiers: CLAUDE.md output_contract and evidence. A template does not redefine them.

## Decision criteria

Factors are **ordered, not weighted**. A model can apply "if these disagree, this one wins"; it cannot compute
a calibrated weighted average of qualities it just judged. Ordering is also auditable: a reader can check that
the winning factor really was the first unmet one.

List position carries the precedence, so the numbering is the ordering and there is nothing else to keep in sync:

```markdown
## Decision criteria

1. **Evidence completeness.** A file the decision depends on has not been read in this session. Read it before
   deciding: a summary of a file is not the file.
2. **Scope clarity.** The request admits two readings that lead to different work. Whether to ask: CLAUDE.md
   work_selection.
3. **Reversibility.** The action cannot be undone from the repository alone: it deletes, publishes, or mutates
   shared state. Approval: CLAUDE.md hard_rules.

Resolution: the first criterion whose condition holds decides; later ones are not consulted.
```

Every entry states an **observable** unmet condition: something a reader could check against the transcript, not
a quality to be rated. If two entries could each independently block, they are separate entries, not one weighted
score.

Numeric weights and scores: CLAUDE.md evidence.

## Escalations

Conditions that change what happens next, stated per domain. The escalation block names conditions and
responses, not severities.

```markdown
## Escalations

| Condition | Response |
|---|---|
| The observable condition | What to do instead of proceeding |
```

## Checkpoints

A gate between phases, cleared by an artifact so that failing it is visible in the transcript. The clearing rule
is in the Gates section of [execution-workflow](../execution-workflow/SKILL.md).

```markdown
### Checkpoint on analysis quality

Name:

- The files read and the specific lines the conclusion rests on.
- What is still unknown, or that nothing material is.

Unmet: gather the missing evidence before proceeding. When only the user can supply it, whether to ask:
CLAUDE.md work_selection.
```

Place it where it belongs in the sequence: a checkpoint between two phases goes between them, and a heading
between numbered items means the surrounding workflow uses `### Phase` subsections rather than one flat list.

**Phrase every check so it can fail.** "Have I gathered sufficient evidence?" cannot: it is answered yes by
whatever evidence was gathered. "Name the files read" can. **A check that cannot fail is not a check.**

## The prepare phase

Load the governing skill first, because every step after it is written against guidance that has not arrived
yet. Then, when prior knowledge could change the work, read the memories the task type calls for.

```markdown
### Prepare

1. Load the skill this workflow depends on through the runtime's skill mechanism, or read its SKILL.md in full
   when that mechanism is unavailable. Load serena-usage for memory or symbol operations. Return the skills
   loaded, by name.
2. When prior knowledge could change the work, read only the memories matching this task type, by the
   procedure in serena-usage. Return the memories read, or an explicit "nothing matched".
```

**Name the loaded skills in the output.** "Loaded the governing skill" with no name is not checkable against
the transcript, which is the only thing distinguishing a real load from an intention to load. Name the skill
in the step text rather than in a separate field, so a reader cannot see one without the other.

Nothing resolves a reference automatically. Register a skill dependency as a row in the orchestrator's load
table, naming **the condition that fires the load**: "Writing or evaluating tests", "Any Serena memory or
symbol operation", not a taxonomy the skill belongs to. A category label cannot fire; a condition can.

Do not compose a file from another's sections with reference markup (`refs`, `inherits="skill#anchor"`): the
referenced body never enters the context, so the agent applies only what the referencing file restates. A
trigger row plus an explicit skill load (a Skill tool call in Claude Code, a full SKILL.md read where no skill
tool exists) is checkable: either the load appears in the transcript, or the content was never there.

Scale the preamble to the work. A memory read that returns nothing useful still costs a round trip before any
work starts, so a lookup-shaped task reads the index and stops there.

## Self-evaluation

A final pass that looks for **what is missing** from the report (something a model can actually do) rather
than rating what is present, which it cannot.

1. Re-read the report and tag each finding per CLAUDE.md evidence; downgrade any tag its citation does not
   support.
2. List anything the request asked for that the report does not answer, and why: not attempted, blocked, or
   judged out of scope.
3. Set the status from what those two found, and name **the weakest claim** in evidence, with the check that
   would confirm it.

"Which part of this is most likely wrong" has an answer the model can find; "how good is this out of 100" does
not.

## Failure handling

Give the exceptional paths their own steps rather than leaving them implicit: unavailable data is documented
as a gap and the analysis continues within a stated bound; contradictory evidence is surfaced as a question rather than averaged
into a hedge.

## When two rules appear to contradict

In a corpus of cross-referencing skills, apparent conflicts arise as it grows. The reflex is to pick a winner
and weaken the loser, which loses real guidance.

1. Assume both are correct and look for the distinguishing condition separating their domains. **Most apparent
   conflicts are two correct rules stated without their preconditions.**
2. Add a reconciling note to the affected section naming that condition. This restores consistency without
   changing the substance of either rule: the smallest edit that fixes the problem.
3. Only if no distinguishing condition exists is one of them actually wrong. Weakening or removing a rule is
   the last resort, not the first move.

Prefer a condition already present in the material over one invented to settle the dispute; an invented axis
tends to be unmemorable and will not be applied consistently later.

## Escalating a review into a refutation

When a single-pass review is not enough, escalate to an independent skeptical refutation rather than asking the
same or another agent to "review" again.

Use it when the claim is plausible-sounding but consequential if wrong: a security or data-integrity finding,
a claim grounded in nothing the checker re-derived, a report the original author is invested in defending. A
routine style or naming observation does not need it.

- **Independence**: run in a context the original work did not shape: a fresh agent invocation given only the
  claim and its cited evidence, never the producing agent's reasoning, memory, or session.
- **Skeptical framing**: instruct the checker to *refute* the claim, not to review or double-check it. A
  reviewer confirms; a refuter is rewarded for finding the flaw, which is the behavior actually wanted.
- **Grounding**: the refutation rests on a primary source re-examined now: a command re-run, a file re-read, a
  doc fetched from a source *the orchestrator* names. Which sources count as independent grounding: load the
  fact-check skill.

Known failure modes, all of which are properties of the technique rather than reasons to skip it:

- **False positives.** A skeptical refuter is tuned to find fault and will surface objections that do not
  warrant a fix. A refutation is an input to a decision, not the decision.
- **Lazy validation.** The inverse: a checker asked to "review" with no skeptical framing tends to rubber-stamp
  plausible-looking work. This is the default failure this pattern escalates away from.
- **Cost.** Each refutation adds an investigation. Reserve it for findings whose cost of being wrong is high,
  and bound both the findings sent for refutation and the work assigned to each.
- **Shared blindspot.** Dispatching the same underlying model as both producer and refuter does not buy true
  independence: identical models tend to miss the same category of error. A known limitation, not a guarantee
  it does not have.

Report the outcome per CLAUDE.md evidence.

## Mechanical conventions need enforcement

For a mechanically decidable convention, define its enforcement mechanism as well as its prose. Distinguish
automated gates from policies that require human or agent judgment.

- Style and idiom → the project's formatter and linter configuration, not review comments.
- Import and layering constraints → a dependency or import-boundary checker.
- Dead exports and unreachable code → a detector, so a removal convention stays true over time.
- Rules no off-the-shelf tool knows → a test in the normal suite, scanning emitted output (format strings,
  generated text) rather than whole-file text, since a whole-file scan flags a comment or doc line that merely
  describes the old idiom as if it were the idiom itself.

A lesson that recurs a third time, in memory, review findings, or user corrections, warrants proposing one of
these checks rather than another record. Approval for adding it: CLAUDE.md hard_rules. The memory entry until
then: CLAUDE.md memory_policy.

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

## Never

- **A numeric self-assessment**: a confidence score, a factor weight, a threshold the agent gates itself on
  (CLAUDE.md evidence). State the condition that must hold in observable terms, and the action when it does
  not.
- **A reference in place of content.** Writing "see workflow-patterns" where the content belongs, on the
  assumption that something resolves it. Nothing does, outside CLAUDE.md. The file then carries an empty
  slot that reads to every later reader as if it were filled, worse than an obviously missing section.
- **A ceremonial placeholder.** Structure filled with generic text to satisfy a template: a `<tool>` element
  reading "task-specific analysis tools", a step whose output is "Step completed". Name the actual tool, or
  drop the element. Filler in prose: the explanatory-strict output style.

## Related

- [serena-usage](../serena-usage/SKILL.md): the memory and symbol operations the prepare phase performs
- [test-integrity](../test-integrity/SKILL.md): the false-green failure a promoted tier produces
- [execution-workflow](../execution-workflow/SKILL.md): where these gates run during a task
