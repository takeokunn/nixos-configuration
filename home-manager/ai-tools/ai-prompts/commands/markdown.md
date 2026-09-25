---
argument-hint: [file-path]
description: Markdown text update command
---

Save the previous command's substantive output as Markdown. Preserve its conclusions, specifications, and decisions, not its deliberation or execution history.

## Target

Use the user's path only inside the active project root; otherwise ask for an in-project path. Without a path, write at that root:

| Previous command | File |
|---|---|
| /define | EXECUTION.md |
| /ask or /bug | RESEARCH.md |
| Other | MEMO.md |

## Write

1. Load technical-writing and technical-documentation. Identify the previous command and the content to save.
2. Resolve the target and read any existing file immediately before editing. Preserve unrelated content.
3. Use only claims present in that output or sources read for this task. Verify or omit new claims; check code examples against the repository. Improve presentation without changing meaning.
4. Restore omitted substantive content from the source output, or report the gap. Do not add timestamps, revision history, changelogs, discussion traces, or invented details.
5. Before writing, name the target and check the proposed headings for those prohibited sections. Write the document, inspect its diff and examples, and run the repository's applicable Markdown checks.
6. Run cold-read and correct supported findings, then rerun affected checks.

Return CLAUDE.md's output_contract with the created or updated path, verification results, and any substantive omission with its reason.
