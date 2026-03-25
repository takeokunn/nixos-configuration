---
name: exploration-tools
description: "Use when the agent needs to 'search codebase', 'find files', 'grep for pattern', 'navigate symbols', or 'explore code' using Glob, Grep, Read, and LSP tools. Provides standardized tool definitions, usage patterns, and search strategies for efficient codebase exploration."
---

Standardized tool definitions and usage patterns for codebase exploration. The agent should reference this skill instead of maintaining inline tool definitions.

## Tools Reference

### Glob - File Pattern Matching

Find files by name pattern when the file extension or naming convention is known.

```
Glob(pattern="**/*.tsx", path="src/")
# Returns: All TypeScript React files under src/
```

**Parameters**: `pattern` (required), `path` (optional, defaults to cwd)

### Grep - Content Search

Search file contents for patterns, keywords, or code constructs.

```
Grep(pattern="useState", glob="**/*.tsx", output_mode="content", -C=3)
# Returns: All useState usages in React components with 3 lines of context
```

**Parameters**: `pattern` (required), `path`, `glob`, `type` (js/py/rust), `output_mode` (content/files_with_matches/count), `-A`/`-B`/`-C` (context lines)

### Read - File Contents

View file contents after locating with Glob or Grep.

```
Read(file_path="/path/to/file.ts", offset=50, limit=30)
# Returns: Lines 50-80 of the file
```

**Parameters**: `file_path` (required, absolute), `offset`, `limit`

### LSP Tools - Symbol Navigation

- **LSP_goToDefinition**: Navigate to where a function, class, or variable is defined (`file`, `line`, `character`)
- **LSP_findReferences**: Find all usages of a symbol (`file`, `line`, `character`)
- **LSP_documentSymbol**: Get overview of all symbols in a file (`file`)

## Search Patterns

### 1. File Discovery

1. Use Glob with pattern to find candidate files
2. Filter results by relevance
3. Read specific files for details

### 2. Content Search

1. Use Grep with pattern and file type filter
2. Review matches with context (`-C` flag)
3. Follow up with Read for full file context

### 3. Symbol Navigation

1. Use LSP_goToDefinition to find symbol source
2. Use LSP_findReferences to find all usages
3. Use LSP_documentSymbol for file-level overview

## Tool Selection Decision Tree

| Need | Tool |
|------|------|
| Find files by name pattern | Glob |
| Search file contents | Grep |
| Find symbol definition | LSP_goToDefinition or Serena find_symbol |
| Find symbol usages | LSP_findReferences or Serena find_referencing_symbols |
| View file contents | Read |

## Search Scope Strategy

Start narrow, expand if needed:

- **Narrow**: Single file or directory
- **Medium**: File type across project
- **Broad**: All files in project (use only when necessary)

## Result Ranking

- **High relevance**: Exact matches, definition sites
- **Medium relevance**: Usage sites, related patterns
- **Low relevance**: Comments, test files, generated code

## Anti-Patterns

- **Blind broad search**: Always start with file type or directory filters
- **Reading without searching**: Use Glob/Grep to locate files before reading
- **Ignoring context**: Use `-C` flag with Grep or read surrounding lines

## Constraints

- The agent must return file paths with line numbers for all findings
- Limit results to manageable size
- Maintain read-only operations during exploration
- Filter out binary and generated files

## Related Skills

- `serena-usage`: Alternative symbol navigation via Serena MCP
- `investigation-patterns`: Evidence collection methodology using these tools
