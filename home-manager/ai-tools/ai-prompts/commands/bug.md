---
argument-hint: [error-message]
description: Root cause investigation command
---

Trace an error or anomalous behavior to its cause from evidence: an explanation the user acts on, not a fix.

## Rules

Critical:

- Never modify files or memories or apply a fix. Return memory candidates in the response for an authorized
  writer.
- Judge from the logs and code, not the user's account: the reported location is where the symptom surfaced, often
  not where the defect lives.
- Report honestly when the cause can't be identified: a named-but-unevidenced cause is worse than an open
  question, since it ends the search and the next reader inherits it as settled.

Standard:

- Logs are the primary source: they record what happened, code only what can happen.
- Track the occurrence path chronologically, and find every location sharing the defect's shape before
  recommending anything.

## Investigation hazards

- **call_site_role.** A call site proves a path exists, not its role: debug hooks and QA controls are simpler and
  more findable than the production implementation, so "the only calls I can find are manual" often wrongly reads
  as unimplemented rather than owned elsewhere, so the fix then builds a second one.
- **generated_document_as_source.** Where a committed document and its generator both exist, the generator is the
  evidence, the document only a claim: a checked-in schema snapshot or generated client answers in the exact form
  asked and goes stale silently.
- **changed_error_is_not_progress.** A fix that turns one error into another has usually just cleared a surface
  obstacle in front of the real constraint, so reading that as progress produces the loop of trying successive
  flags and tokens. The test: whether the cause condition is gone, not whether the output differs.

## Workflow

### Prepare

1. Load investigation-patterns (it governs hypothesis discharge, bisection, and evidence handling here), plus
   fact-check when the failure implicates an external library or API contract, using Skill. Return the skills
   loaded, and why fact-check was skipped if it was.
2. Activate the Serena project, call list_memories, and read entries matching this failure's domain
   ({domain}-patterns, architecture-*, {project}-conventions), or none if none match, using Serena
   activate_project, list_memories, read_memory. Return the memories read, or "nothing in the index matched".

### Analyze

1. Observe: the artifact the symptom ran against (module, binary, or daemon, path/mtime/hash) versus the tree's
   build output; and how many failures arrived together versus how many things changed. Reason: subject unclear
   until both are known: if the runtime loads from an install prefix, long-lived daemon, container image, compiled
   bundle, or package cache rather than the working tree, the code read may not be the code that ran, and every
   later step then reads to a confident wrong answer since the removed line is still in the artifact. The
   failure-count ratio is the harness-versus-code question, cheapest form. Act: name the artifact and state
   whether it matches current source; if not, stop and report that instead of investigating: nothing read
   afterward describes what ran. State whether the subject is code or harness, and what decided it.
2. Classify the error (syntax, runtime, logic, config) from the message, exception type, and stack trace. Record
   the primary location as file:line with the call chain, marking symptom site versus cause site: the deepest
   non-library frame is where it surfaced, not where it originated. Return the classification, symptom site, call
   chain.
3. Read the log lines around and before the error, and the events immediately before, during, and after it: this
   separates a new failure from a recurring pattern, and a transient condition (race, resource exhaustion) from a
   deterministic defect. Return a timeline with state anomalies; the failure classified deterministic or
   condition-dependent.

### Investigate

1. Scale the dispatch to the failure: a stack trace pointing at one readable file isn't agent work. Where the
   failure spans subsystems, dispatch in one message (quality-assurance for the mechanism and ranked hypotheses,
   explore for the error site and every recurrence, general-purpose for the log timeline and dependency state),
   and name what you skipped and why, using Agent. Return findings with file:line, or the reason no agent was
   needed.
2. Read the failing code in full, then its dependency and import chain, then config values in effect and recent
   changes touching them, verifying any external contract against Context7 or the vendored source rather than
   recall, using Read, Grep, Serena find_symbol and find_referencing_symbols, Bash (git log, git diff), Context7.
   Return the failing line identified, its chain, and the config and changes bearing on it.
3. When a reproduction, probe, or slice fails, decide before recording it whether the failure describes the
   subject or the reproduction: a slice can cut through an incomplete form, a wrapper can resolve a relative path
   against its own location, a probe can reference a symbol never defined in the reduced file. Tell: a failure
   arriving before the suspect work starts requires checking the reproduction setup first. Return each failure
   labelled subject-side, reproduction-side, or unresolved, with the evidence supporting that classification.

Iteration limit: 3. Narrowing assumes a stable oracle: if the boundary moves between probes (different file, form,
or line each time), the oracle is noisy and every step fits that noise, and a narrowing loop never runs out of
next moves, so it won't stop on its own. After three steps without a reproducing boundary, stop: re-run one
identical probe for reproducibility, then report what's ruled out and hand the scope decision to the user.

### Checkpoint: investigation quality

Per gate_discipline in CLAUDE.md. Name:

- The evidence chain as symptom → mechanism → cause, citing a file:line or log line at each link.
- The link that was inferred rather than read, or that every link was read.
- The other locations sharing this cause, or that the search ran and found none.

Unmet: continue on the unsupported link. If it cannot be established from the repository, say so rather than
presenting the chain as complete.

### Self evaluate

Tag each link per CLAUDE.md's evidence rules, downgrading any that can't name the command run or file:line read.
Where a link rests on a document, scope the tier to the passage checked: one section can be accurate while another
describes classes, columns, or features nowhere else. List what the reported error raises that this report doesn't
answer, and set the status. Return the tagged chain, downgrades, gaps, status.

### Memory handoff

If memory_policy identifies a reusable finding, search the memory index by topic and return the candidate with
any matching entry for an authorized writer. Do not persist it in this read-only command.

## Checkpoint: group consistency

- Any workflow phase skipped, and why.
- That no file or memory was modified and no fix applied.

Unmet: resolve the gap before returning the report.

## Agents

Read-only, dispatched by need. Each finding carries a file:line.

| Agent | Returns |
|---|---|
| quality-assurance | Failure mechanism and hypotheses ranked by the evidence separating them. |
| explore | Error site with surrounding code and call chain, plus every location sharing the same defect shape. |
| general-purpose | Log timeline, environment anomalies, dependency state. |

## Decision criteria

1. **root_cause_certainty.** The named cause was never observed producing the symptom: no reproduction, no log
   line, no code path read end to end. Present it as a ranked hypothesis, not the root cause.
2. **evidence_chain.** A link has no file:line or log line behind it. Read it, or mark it inferred and say what
   would close it.
3. **fix_viability.** The suggested fix was not checked against every location sharing the cause. Run the
   recurrence search, or state that the recommendation covers only the reported site.

## Output

Follows output_contract in CLAUDE.md. Add:

- root_cause: direct cause, underlying cause, and the conditions it fires under, with the chain tagged link by
  link.
- impact: scope, and the other locations sharing this cause.
- recommendations: fix suggestions without implementation, and prevention.
- fix_scope_bracket: the smallest change that could resolve the cause and the largest the evidence would justify,
  as two ends: everything between conditioned on what must be shown first, e.g. "include the storage change only
  if evidence shows cross-world reads are involved." State the failure at each end: what an under-fix leaves
  wrong, an over-fix risks. The reader's next decision is how far to go, not what the cause is: this investigation
  holds that evidence.
- subject: whether the investigation treated the code or the harness as its subject, the artifact the symptom was
  observed against, and whether it matched current source.
