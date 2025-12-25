# Agent Principles

## Role Distribution

| Role | Responsibilities |
|------|-----------------|
| Parent Agent (You) | Policy decisions, judgment, requirements definition, specification design |
| Sub-agents | Task execution (research, documentation, code generation) |

**Sub-agent Priority**:
1. Custom sub-agents (project-specific)
2. General-purpose sub-agents (Task)

## Serena MCP Usage

### Memory Management

**Before Implementation**:
1. `list_memories` - Get memory list
2. `read_memory` - Check relevant memories
3. Strictly follow recorded patterns/conventions

**Recording New Patterns** (use `write_memory`):

| Pattern | Naming Convention |
|---------|------------------|
| Project conventions | `{project}-conventions` |
| Feature patterns | `{feature}-patterns` |
| Domain patterns | `{domain}-patterns` |
| Layer conventions | `{layer}-conventions` |
| Architecture decisions | `architecture-{decision}` |
| API specs | `{service}-api-spec` |
| Troubleshooting | `{issue}-solution` |
| Refactoring | `refactoring-{target}` |

**Maintenance**: Delete obsolete memories with `delete_memory`. Keep names concise and searchable.

### Code Operations

| Tool | Use Case |
|------|----------|
| `find_symbol` | Symbol search |
| `get_symbols_overview` | File structure overview |
| `find_referencing_symbols` | Dependency analysis |
| `replace_symbol_body` | Replace entire function/class |
| `insert_before_symbol` / `insert_after_symbol` | Insert code around symbols |
| `search_for_pattern` | Cross-codebase pattern search |

**Principle**: Prefer symbol-level operations over reading entire files.

## Pre-Implementation Checklist

| Check | Action |
|-------|--------|
| Existing patterns | Review existing code/docs before implementing |
| Library specs | Use Context7 for latest library documentation |
| Duplicate prevention | Check existing code before custom implementations |
| Memory check | Use `list_memories` for past patterns/decisions |

## Constraints

| Category | Rule |
|----------|------|
| Git operations | Only on user request |
| Config files | Require permission before changes |
| Backups | Not needed (Git-managed) |

## Text Processing

**Required**: Always use `perl` for text processing. Never use `sed` or `awk`.

```bash
# Correct
perl -pi -e 's/old/new/g' file.txt

# Incorrect
sed -i 's/old/new/g' file.txt
```

## Documentation Rules

- No timestamps
- Keep it concise (no effort/backward-compatibility considerations)
- Comments: Only for complex logic, in Japanese, specific TODOs
