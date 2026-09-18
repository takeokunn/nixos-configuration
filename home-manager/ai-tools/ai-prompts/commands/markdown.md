---
argument-hint: [file-path]
description: Markdown text update command
---

Write the previous command's result (/define, /ask, /bug, …) to a markdown file.

## Rules

Critical:

- Do not include a timestamp, revision history, change log, or discussion trace.
- Use the user's path when it is within the permitted project root. If it is outside that boundary, ask for an
  in-project path before writing.

Standard:

- Everything written traces to the previous command's output or a file read this session: anything tracing to
  neither is verified against the source or cut, never softened into a hedge.
- Check each code example against its source file and remove examples that no longer match.
- Reformatting may change presentation; it may not change what a section claims.

## Workflow

### Prepare

1. Load technical-writing and technical-documentation for the document's prose and structure. Return the skills
   loaded, or report an unavailable skill and the fallback used.

### Determine

1. Identify the previous command and the section being documented. Resolve the target path: the user's if given,
   else file_mapping's, so repeat runs of the same command land in one file instead of near-duplicates. Read the
   target if it exists. Return the previous command, target path with its source, and what the path currently
   holds.
2. Separate signal from process: keep conclusions, specifications, and decisions; drop deliberation and revision
   history. Substantive conclusions, specifications, or decisions missing from the draft must be restored or
   named under gaps with the reason. Return the retained scope and substantive omissions, not discarded deliberation.

### Checkpoint on group consistency

Per gate_discipline in CLAUDE.md. Name:

- The path about to be written, whether it exists, and what it currently holds.
- The document's headings: confirming none introduces a timestamp, revision history, or discussion trace.

Unmet: do not write: resolve the conflict or strip the prohibited content, or ask for the path with
AskUserQuestion.

### Write and verify

1. Write the document to the resolved path, preserving unrelated existing content. Inspect the diff, check any
   examples against their sources, and run the repository's Markdown checks when available.
2. Load cold-read and review the document without relying on the conversation. Return the checks run and their
   results, including any content whose meaning could not be verified.

## File mapping

Default output dir: project root.

| Command | Output |
|---|---|
| /define | EXECUTION.md |
| /ask | RESEARCH.md |
| /bug | RESEARCH.md |
| other | MEMO.md |

## Output

Follows output_contract in CLAUDE.md: summary names the file written and whether created or overwritten; gaps
names substantive conclusions, specifications, or decisions excluded from the previous command, and why.
