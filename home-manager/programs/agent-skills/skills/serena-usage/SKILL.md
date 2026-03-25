---
name: serena-usage
description: "Use when performing 'semantic search', 'symbol analysis', 'find references', 'code navigation', 'Serena MCP', or 'memory management'. Provides Serena tool usage patterns for symbol-level code operations, persistent memory, and orchestration integration."
---

Patterns for effective use of Serena MCP tools for semantic code operations, persistent memory management, and orchestration workflow integration. The agent should prefer Serena navigation tools over Glob/Grep/Read for code files.

## Session Initialization

Every session must begin with these steps:

1. Activate the project: `activate_project project="project-name"`
2. Verify onboarding: `check_onboarding_performed`
3. If not onboarded, run: `onboarding`
4. Check available memories: `list_memories`

## Tool Reference

### Navigation Tools

| Tool | Purpose | Key Parameters |
|------|---------|---------------|
| `get_symbols_overview` | High-level view of symbols in a file | `relative_path`, `depth` (0 = top-level) |
| `find_symbol` | Find symbols by name path pattern | `name_path_pattern`, `include_body`, `substring_matching` |
| `find_referencing_symbols` | Find all references to a symbol | `name_path`, `relative_path` |
| `search_for_pattern` | Regex search across codebase | `substring_pattern`, `relative_path`, `context_lines_before/after` |
| `list_dir` | List directory contents | `relative_path`, `recursive` |
| `find_file` | Find files by name pattern | `file_name_pattern`, `relative_path` |

### Editing Tools

| Tool | Purpose | Key Parameters |
|------|---------|---------------|
| `replace_symbol_body` | Replace entire symbol definition | `name_path`, `relative_path`, `body` |
| `insert_before_symbol` | Insert content before a symbol | `name_path`, `relative_path`, `body` |
| `insert_after_symbol` | Insert content after a symbol | `name_path`, `relative_path`, `body` |
| `rename_symbol` | Rename symbol across codebase | `name_path`, `relative_path`, `new_name` |

### Memory Tools

| Tool | Purpose | Key Parameters |
|------|---------|---------------|
| `list_memories` | List all available memory files | None |
| `read_memory` | Read content of a memory file | `memory_file_name` |
| `write_memory` | Write to a memory file | `memory_file_name`, `content` |
| `edit_memory` | Replace content in a memory file | `memory_file_name`, `needle`, `repl`, `mode` |
| `delete_memory` | Delete a memory file (requires user permission) | `memory_file_name` |

### Reflection Tools

| Tool | When to Use |
|------|------------|
| `think_about_collected_information` | After 3+ search operations |
| `think_about_task_adherence` | Before any code modification |
| `think_about_whether_you_are_done` | Before reporting task completion |

## Core Workflows

### Explore a File

```
1. get_symbols_overview relative_path="src/main.ts" depth=0
2. get_symbols_overview relative_path="src/main.ts" depth=1
3. find_symbol name_path_pattern="MyClass/myMethod" include_body=true
```

### Trace Dependencies

```
1. find_symbol name_path_pattern="processData" relative_path="src/processor.ts"
2. find_referencing_symbols name_path="processData" relative_path="src/processor.ts"
3. Repeat for each caller to build complete dependency tree
```

### Safe Refactoring

```
1. find_symbol name_path_pattern="MyClass/oldMethod" include_body=true
2. find_referencing_symbols name_path="MyClass/oldMethod" relative_path="src/myclass.ts"
3. think_about_task_adherence
4. replace_symbol_body name_path="MyClass/oldMethod" relative_path="src/myclass.ts" body="..."
5. Update calling sites if interface changed
```

## Tool Selection Decision Tree

| Need | Primary Tool | Fallback |
|------|-------------|----------|
| Find files by name | `find_file` | Glob (if Serena unavailable) |
| Search file contents | `search_for_pattern` | Grep (if non-code or Serena unavailable) |
| List directory | `list_dir` | `ls` (if Serena unavailable) |
| View file structure | `get_symbols_overview` (depth=0, then 1) | Read (for YAML, JSON, MD) |
| Find specific symbol | `find_symbol` with `name_path_pattern` | `substring_matching=true`, then Grep |
| Find all usages | `find_referencing_symbols` | Grep with symbol name pattern |

### Language-Specific Guidance

- **Strongly typed (TypeScript, Go, Rust, Java)**: Strongly prefer symbol operations; LSP provides accurate resolution
- **Dynamic (Python, JavaScript, Ruby)**: Use symbol operations with `substring_matching=true`
- **Configuration (Nix, YAML, JSON, TOML)**: Use `search_for_pattern` or Read
- **Markup (Markdown, RST, HTML)**: Use `search_for_pattern` or Read

## Memory Management

### Naming Convention

```
{project}-conventions    # Project-wide conventions
{feature}-patterns       # Feature-specific patterns
{domain}-patterns        # Domain-specific patterns
architecture-{decision}  # Architecture decisions
```

### When to Create Memories

**Create when:** discovering significant architectural patterns, resolving complex bugs with reusable insights, completing features with transferable patterns, or when the user mentions a convention.

**Skip when:** one-off fixes, temporary preferences, workarounds to be replaced, or information already documented elsewhere.

### Memory Priorities by Task Type

| Task | Priority 1 | Priority 2 | Priority 3 |
|------|-----------|-----------|-----------|
| Investigation | `{domain}-patterns` | `architecture-*` | `{project}-conventions` |
| Implementation | `{feature}-patterns` | `{language}-conventions` | `testing-patterns` |
| Review | `{project}-conventions` | `code-quality-*` | `architecture-*` |
| Refactoring | `architecture-*` | `{component}-patterns` | `testing-patterns` |

## Critical Rules

- The agent should always activate the project and check onboarding at session start
- The agent should always check memories before implementing new features
- The agent should use symbol operations over reading entire files for code
- The agent should call `think_about_task_adherence` before any code modification
- The agent should call `think_about_collected_information` after 3+ search operations
- The agent should call `think_about_whether_you_are_done` before the final response
- The agent should prefer Serena navigation tools before Glob/Grep/Read
- The agent should record significant patterns with `write_memory` after discovery
- The agent should never delete memories without explicit user request

## Anti-Patterns

- **Reading entire files**: Use `get_symbols_overview` and `find_symbol` with `include_body` instead
- **Unscoped searches**: Use `relative_path` to restrict search to known files or directories
- **Ignoring memories**: Always check `list_memories` and `read_memory` before implementation
- **Manual refactoring**: Use `rename_symbol` for consistent renaming with automatic reference updates
- **Excessive depth**: Start with `depth=0`, incrementally increase if needed
- **Skipping reflection**: Always call reflection tools at mandatory checkpoints
- **Using Glob/Grep directly**: Prefer Serena navigation tools when the project is activated
