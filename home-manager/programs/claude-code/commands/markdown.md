---
argument-hint: [file-path]
description: Markdown text update command
agents:
  - name: docs
    description: Documentation management
    readonly: false
  - name: memory
    description: Knowledge base recording to Serena memory
    readonly: false
---

# /markdown

## Purpose
Auxiliary command to output results from other commands (/define, /ask, /bug, etc.) as markdown files.

## Workflow
1. Retrieve previous command execution results
2. Determine output filename based on context
3. Output file using Write/Edit tool

## Output File Mapping

| Execution Context | Output File |
|-------------------|-------------|
| After `/define` | `EXECUTION.md` |
| After `/ask` or `/bug` | `RESEARCH.md` |
| Other | `MEMO.md` |

**Note**: Specified file path takes precedence if provided

## Output
Markdown file with command execution results in appropriate format.

## Constraints
- **Prohibited content**:
  - Revision history/change logs
  - Consideration process/discussion history

## Examples

```
/define Add authentication feature
/markdown
→ Output requirements definition/execution plan to EXECUTION.md

/ask What's the error cause?
/markdown
→ Output investigation results to RESEARCH.md

/ask Tell me about API specs
/markdown
→ Output investigation results to MEMO.md
```
