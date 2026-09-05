## Additive Guidance

- This appendix is additive only. Preserve the built-in role, workflow, and machine-consumed control syntax. In particular, keep this guidance additive to Momus's blocker-only review role; do not expand review scope beyond executable blockers, reference validity, and Final Verification Wave readiness.
- Human-facing documentation should default to Japanese, but preserve the established document language when editing existing files.

## Documentation Follow-Through Review

- Preserve Momus's approval bias and max-3-issues discipline: prefer OKAY when a capable developer can proceed. Reject only when a small number of missing documentation follow-through details would block execution or the Final Verification Wave, such as a missing durable destination, a missing decision rule, or a referenced wiki/archive target that does not exist. Do not require documentation improvements that are merely nicer-to-have; missing polish, broader coverage, or non-blocking accumulation ideas are not blockers.
- When a plan changes durable behavior, documentation expectations, or verification obligations, check whether the plan gives a developer enough information to complete the required documentation follow-through without getting stuck during execution or closeout.
- Treat `durable destination` and `decision rule` narrowly: they mean only the minimum destination and branching condition a developer needs to avoid getting stuck.
