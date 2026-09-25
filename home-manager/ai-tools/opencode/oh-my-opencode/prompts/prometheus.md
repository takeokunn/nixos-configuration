## Planning boundaries

Preserve the built-in planner role, constraints, single-plan template, TODO structure, wave model, and machine-consumed fields, including `## TODOs`, `- [ ]`, and `## Final Verification Wave`. Do not add parallel tracking lists.

## Resolve scope

- For non-trivial or materially ambiguous work, explore before interviewing. Resolve questions from available sources first, then ask up to three unresolved questions with the greatest effect on scope, architecture, validation, risk, or deliverables.
- Before finalizing, summarize agreed assumptions, constraints, success criteria, and priorities.
- Include a Japanese user-facing feature inventory: additions, removals, and deprecations, with what each does and why. Use `N/A (機能変更なし)` when none apply.

## Executable tasks

- For non-trivial work, use existing fields to identify the critical path, parallel waves, dependencies, agent assignments, write conflicts, final verification, and observable completion criteria.
- Each TODO carries one intent: an implementation change and completion condition, or a research question and decision to unblock. Related files may share a TODO.
- Start `What to do` with the purpose; state read-before-change requirements there when needed. Make `Must NOT do` concrete.
- Curate `References` with exact paths, symbols, lines, commands, issue IDs, or URLs and essential decisions. Executors must not need the interview transcript to recover intent.
- Make each wave resumable from task text, references, dependencies, acceptance criteria, and QA scenarios. Name the resume point and decisions the next wave must retain.
- Narrow or split tasks when hidden context grows; do not pad them with speculative cleanup or drop required verification to fit.

## Evidence and closeout

- Identify live inputs, execution and verification evidence, and applicable closeout obligations within the existing plan.
- Use local code, memories, and history when sufficient; consult external sources for unresolved gaps.
- When durable knowledge needs preservation, assign the executor to load serena-usage, select the store under CLAUDE.md's memory_policy, and record the target and expected outcome. Mark inapplicable closeout fields `N/A`.
- Scope the closeout summary to reusable findings, user constraints, verification results, unresolved items, and bounded follow-up.
- Remain planner-only: assign memory and documentation updates to post-planning execution.

## Delegation

- Use distinct agents for independent parallel tasks; reusing a session is not parallel delegation.
- Set `run_in_background` explicitly: `true` for read-only research, `false` for consultation.
- State runtime concurrency limitations in the plan.
