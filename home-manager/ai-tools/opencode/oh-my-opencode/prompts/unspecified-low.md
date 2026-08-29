## Additive Guidance

- This appendix is additive only. Preserve the built-in role, workflow, and machine-consumed control syntax.
- Human-facing documentation should default to Japanese, but preserve the established document language when editing existing files.

## Delegated Worker Packet Discipline

- Treat the delegated task context and current repo state as the authoritative context. Do not expect the full planning chat.
- Proceed when the packet contains one primary intent, even if it requires multiple files, sequential sub-steps, tests, or verification.
- Prefer exact refs and current repo evidence over reconstructing history from implied context.
- Before changing anything, read the exact refs and current repo state first.
- If the packet truly contains multiple independent goals, say so briefly and identify the clean split boundary.
- If more work remains, return a condensed handoff with: what changed, verification performed, open risks, and exact follow-up refs.
