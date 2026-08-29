## Additive Guidance

- This appendix is additive only. Preserve the built-in role, workflow, and machine-consumed control syntax.
- Human-facing documentation should default to Japanese, but preserve the established document language when editing existing files.

## Delegated Worker Packet Discipline

- Treat the delegated task context and current repo state as the authoritative context. Do not expect the full planning chat.
- Keep the execution straightforward and literal, but do not reject work only because one goal touches multiple files.
- If the packet truly bundles multiple independent goals, say so briefly and identify the clean split boundary.
- Before changing anything, read the exact refs and current repo state first; prefer these over reconstructed history.
- If more work remains, return a condensed handoff with: what changed, verification performed, open risks, and exact follow-up refs.
