---
argument-hint: [task-description]
description: Full task execution with feedback loop
---

Implement the task with a full review wave and independent verification. Apply CLAUDE.md's hard_rules, delegation, evidence, consensus, memory_policy, and output_contract. Do not request routine confirmation; ask for missing authority or a material scope decision. This command grants no Git-write authority.

## Prepare

Load execution-workflow. Confirm the active root, current ref, and dirty state; record HEAD and the initial dirty diff, including untracked files, so later scans exclude pre-existing work.

Inspect both memory indices, then load only task-matching entries, canonical verification commands, and relevant deferred findings. Name matched and loaded entries; explain relevant entries deferred. Report an unavailable store rather than guessing its contents.

Define acceptance criteria, atomic execution units, dependencies, owned paths, and concrete checks. Trace sibling registration/discovery before adding a family member, and search identifiers across declarations, comments, callers, and test doubles before migration or removal. A missing search hit does not establish automatic discovery.

## Implement

1. Before code changes, obtain a design placement review of proposed symbols, modules, layers, and dependencies.
2. Dispatch one owner per substantial atomic unit, grouping independent units within runtime concurrency limits. Each assignment names paths, prohibited mutations, deliverable, and verifying command under delegation. Completion needs the named command or artifact.
3. Writers follow output_discipline. Add tests for changed behavior and docs for changed public interfaces, not to fill roles. Inspect existing helpers and fixtures; unavailable behavior does not justify an always-passing stub.
4. Establish verification scope, configuration, generated outputs, fixtures, and services, including new and untracked files.
5. Wait for every writer before compiling or running the settled suite. Confirm the runner loads the changed source. Inspect assertions and exit status; feed failures into the single fix pass below.

## Review wave

Dispatch these six read-only reviews together, scheduling to the runtime's concurrency limit. Reviews must not edit tests, docs, or source.

| Agent | Required focus |
|---|---|
| quality-assurance | Correctness, error handling, caller impact, and ai-slop-detector judgment tells with file:line and replacement |
| security | Trust boundaries and concrete vulnerabilities, including applicable CWE |
| design | Module placement, dependencies, and architectural fit |
| docs | Drift in changed public interfaces and their documentation |
| performance | Changed hot paths; improvement claims require measurements on both sides |
| test | Acceptance coverage, helper/fixture validity, skipped behavior, and tests that cannot fail |

Require a usable report from all six. Each finding needs a location or command, evidence tier, severity, and concrete impact. Missing output is not a clean review. Retry an incomplete or unsupported report once with a narrower question, then perform the check here and disclose failed delegation and lost independence.

Dispatch the verification agent in its read-only reconcile mode only for a consequential disputed claim remaining after the evidence is reread; it must contribute a different evidence base.

## One fix pass

If there are no confirmed critical/warning findings or test failures, skip this pass. Otherwise:

1. Recheck every finding against the current tree, marking it still-present or already-resolved.
2. Address confirmed critical findings first, then warnings and test failures, through the relevant owner. Informational suggestions do not expand scope.
3. Allow one targeted fix iteration across the task, then rerun affected checks. Remaining failures are blockers; never weaken a gate or fail-closed behavior.
4. For each claimed fix, name the change and verification. A vanished symptom without a named change is not a fix. A regression test counts as demonstrated only if observed failing before the fix.
5. Record every unaddressed finding by identifier, location, severity, and reason.

## Wording and code-artifact pass

Load ai-slop-detector. Scan this task's added lines with its lexical patterns and Japanese token source, including new files and excluding baseline edits. Combine that scan with the reviewer's judgment findings.

Writers return zero lexical hits or a contextual reason for each retained hit. Apply one removal pass to confirmed findings, then rescan and rerun affected checks. Skip removal if both lexical and judgment findings are empty. This wording-only pass is outside the test-fix budget; it permits neither behavioral refactoring nor another test-fix cycle. Report scan command, extracted-line count, hits before/after, and retained judgment findings with reasons.

## Independent verification

After fixes and wording changes settle, dispatch a verification agent with the consequential completion claims and supporting commands, not just a diff. It must attack relevant boundaries, interrupted operations, idempotency, and error paths. Report unavailable checks and restrictions. A green suite alone does not settle a claim outside its coverage.

Static prose/configuration checks do not prove runtime behavior. Remaining failed checks or unverified acceptance criteria stay visible in the final status.

## Persist and finish

Apply memory_policy to unresolved finding ledgers and durable learning, such as a canonical gate, a verified blind spot, an exact invocation that took investigation, or a declined abstraction and its reopening trigger. Choose the policy's store before writing. Refresh only memories actually reread against the result; do not run a general freshness audit. If nothing qualifies, state that persistence was skipped.

Return output_contract with changed paths; checks and their input scope; each review's status; findings and their current disposition; fixes; wording-pass results; and the weakest consequential claim with the check that would settle it. Include useful next steps, not a closing restatement.
