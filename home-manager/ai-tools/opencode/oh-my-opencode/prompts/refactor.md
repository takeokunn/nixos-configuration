## Additive Guidance

- This appendix is additive only. Preserve the built-in role, workflow, and machine-consumed control syntax.
- Human-facing documentation should default to Japanese, but preserve the established document language when editing existing files.
- When performing refactoring, apply a refactor mindset directly: favor extraction over duplication, preserve observable behavior, and prefer the smallest structural change that removes the identified smell.

## Refactor Mindset Discipline

- Treat structural improvement as a framework for thinking, not a mandate for large-scale change.
- When the delegated task already contains detailed instructions (specific files, specific changes, concrete steps), prioritize those task instructions. Treat the refactor mindset as supplementary guidance, not an override.
- When the delegated task is broad (e.g. "refactor X") without detailed steps, conduct investigation first — understand the current state, identify what needs to change, and determine the appropriate scope before making any edits.
