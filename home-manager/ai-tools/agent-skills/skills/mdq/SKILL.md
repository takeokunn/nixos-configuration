---
name: mdq
description: Use for querying Markdown with mdq - jq-style selectors to extract headings, sections, tasks, code blocks, links, or tables; checklist validation and CI documentation scripts.
version: 3.0.0
---

mdq applies jq-style querying to Markdown files: selectors mirror Markdown syntax itself, so a heading
selector looks like a heading and a task selector looks like a task. Results output as Markdown (default),
JSON, or plain text.

```
mdq [OPTIONS] [SELECTORS] [FILE...]
```

Reads stdin if no file is given; multiple files are concatenated.

## Selectors

| Element | Syntax | Example |
|---|---|---|
| Heading/section | `# title` | `mdq '# installation' README.md` |
| Heading (regex) | `# /pattern/i` | `mdq '# /getting.started/i' README.md` |
| Unordered list item | `- item` | `mdq '- getting started' README.md` |
| Ordered list item | `1. item` | `mdq '1.' steps.md` |
| Task, unchecked | `- [ ] text` | `mdq '- [ ]' tasks.md` |
| Task, checked | `- [x] text` | `mdq '- [x]' tasks.md` |
| Task, any state | `- [?] text` | `mdq '- [?]' tasks.md` |
| Code block | ` ```language text` | `mdq '```rust' guide.md` |
| Link | `[display](url)` | `mdq '[install](*)' README.md` |
| Image | `![alt](url)` | `mdq '![](*)' README.md` |
| Table row | `:-: column :-: row` | `mdq ':-: /Name/ :-: *' schedule.md` |
| Blockquote | `> text` | `mdq '> note' docs.md` |
| Paragraph | `P: text` | `mdq 'P: /deprecated/' CHANGELOG.md` |

**A heading selector matches the heading plus all content until the next same-level heading** — the match
extends past the heading line itself, so `# usage` pulls the whole section. A table selector takes two
`:-:` clauses: the first matches the column header, the second matches row content.

## String matching

- Unquoted (`installation`) — case-insensitive, must start with a letter.
- Quoted (`"Getting Started"`) — case-sensitive, supports escape sequences.
- Regex (`/getting.started/i`) — fancy-regex syntax.
- Wildcard (`*` or empty) — matches anything.
- Anchors (`^start`, `end$`) — position-anchored.

## Chaining

Pipe selectors with `|` to scope a child match to a parent's result instead of searching the whole document:

```
# Extract unordered lists from the "Usage" section
mdq '# usage | -' README.md

# Get code blocks from the "Examples" section
mdq '# examples | ```' guide.md

# Find checked tasks in the "Release" section
mdq '# release | - [x]' CHANGELOG.md
```

**An unchained child selector matches that element anywhere in the document, not just under the heading you
meant** — always chain with `|` when the target is nested under a section.

## Output

- `--output markdown` / `-o markdown` — default, valid Markdown.
- `--output json` / `-o json` — JSON array, for piping to `jq`.
- `--output plain` / `-o plain` — plain text, no Markdown formatting.
- `--quiet` / `-q` — suppresses stdout; exit code 0 if a match was found, 1 if not.
- `--link-format inline` — inline links instead of reference-style.
- `--link-format keep` — preserve the source link format (default converts to reference-style).

**mdq's default Markdown output rewrites links to reference-style, so a CI script that parses stdout for a
match breaks on that reformatting** — use `-q` and check the exit code instead of parsing output.

## Patterns

Checklist validation (exit 0 once every task is checked):

```
mdq -q '- [ ]' pull_request_template.md && echo "All done" || echo "Incomplete tasks"
```

Extract all links as JSON, then post-process with jq:

```
mdq '[](*)' --output json docs.md | jq '.[].url'
```

Extract a section to a new file:

```
mdq '# api reference' README.md > api.md
```

Extract code examples by language:

```
mdq '```python' tutorial.md --output plain
```

Count checked tasks in a section via jq:

```
mdq '# changelog | - [x]' CHANGELOG.md --output json | jq 'length'
```

**mdq only parses CommonMark** — pointing it at AsciiDoc, RST, or HTML expecting Markdown-like matches
produces wrong or empty results; use a format-specific tool for those instead.

## Related

- [exploration-tools](../exploration-tools/SKILL.md) — locate and search files in a codebase before querying
  them with mdq
- [technical-documentation](../technical-documentation/SKILL.md) — general documentation authoring and
  structure, for when the extracted content feeds back into a doc
