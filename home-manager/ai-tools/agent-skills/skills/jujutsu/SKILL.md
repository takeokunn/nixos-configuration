---
name: jujutsu
description: "Use when running version-control commands in a repository with a `.jj/` directory, where jj replaces git for history, commits, bookmarks, and pushes. Covers the git-to-jj command map, non-interactive flags, the auto-snapshotted working copy, and the gitleaks scan jj's missing Git hooks require before a push."
metadata:
  version: "1.0.0"
---

# Jujutsu (jj)

`jj root` succeeding, or a `.jj/` directory at the repository root, means jj is in use: run version-control
operations through jj. Without `.jj/`, use git; never run `jj git init` to convert a repository unasked.
GitHub operations stay on `gh`.

Repositories here are colocated: `.jj/` and `.git/` share one Git object store, jj exports its changes to Git
refs, and Git's HEAD is left detached. Read-only git commands still work; git writes (commit, rebase, checkout)
bypass jj's model and are replaced by the jj commands below.

## The working copy is a commit

jj has no staging area. Every jj command first snapshots the files on disk into the working-copy revision `@`,
so `jj st` records edits as well as reporting them. In a checkout shared with other sessions, that snapshot
absorbs their edits into `@` too: before describing, squashing, or pushing `@`, read `jj diff --git` and
confirm every hunk is yours. Pass `--ignore-working-copy` to a read-only command that must not snapshot.

A change has a stable change ID (letters k-z) and a commit ID that changes on every rewrite. Refer to changes
by change ID across rewrites.

## Command map

| Git | jj |
|---|---|
| `git status` | `jj st` |
| `git log --oneline --graph` | `jj log` |
| `git diff` / `git diff --cached` | `jj diff --git` (no staging, one diff) |
| `git show <rev>` | `jj show --git <rev>` |
| `git add` + `git commit -m` | `jj commit -m "<msg>"` (describes `@`, starts a new empty `@`) |
| `git commit --amend -m` | `jj describe -m "<msg>"` |
| `git commit --amend --no-edit` (fold in edits) | `jj squash` (moves `@` into its parent) |
| `git checkout -b <b>` | `jj bookmark create <b> -r @` |
| `git branch -f <b> <rev>` | `jj bookmark set <b> -r <rev>` (`-B` to move it backwards or sideways) |
| `git fetch` | `jj git fetch` |
| `git push origin <b>` | `jj git push -b <b>` (see Pushing) |
| `git rebase <onto>` | `jj rebase -b @ -o <onto>` |
| `git revert <sha>` | `jj revert -r <rev> -o @` |
| `git checkout -- <path>` | `jj restore <path>` |
| `git blame <file>` | `jj file annotate <file>` |
| `git reflog` | `jj op log` |

Revsets name revisions: `@` is the working copy, `@-` its parent, `main@origin` a remote bookmark,
`trunk()` the default branch.

## Non-interactive use

- Always pass `-m` to `jj commit` and `jj describe`; without it they open an editor. For `jj squash`, pass `-m`
  or `-u` (keep the destination's message) when both sides carry descriptions.
- Never run `jj split`, `jj diffedit`, `jj resolve`, or any `-i`/`--interactive` form: each opens an interactive
  tool.
- Add `--no-pager` to output-producing commands, and `--git` to diff commands whose output will be parsed:
  the default diff formatter here is difftastic.
- For scripted reads, `jj log --no-graph -r <revset> -T '<template>'` gives stable output.

## Shared working copy

The guardrail hook blocks commands that rewrite the files under other sessions: `jj edit`, `jj next`,
`jj prev`, `jj new` onto another revision, `jj abandon`, `jj restore` without paths, `jj undo`, `jj redo`, and
`jj op restore`/`revert`. The equivalents are to start work with plain `jj new`, discard one file with
`jj restore <path>`, and undo a revision with `jj revert`. Spell the subcommand literally: one produced by
`$(...)` or a variable is blocked because the hook cannot see it. Every jj write (commit, describe, squash,
rebase, bookmark, push) needs the same authorization a git write does. Branch and worktree isolation still
follows execution-workflow's git procedure.

## Pushing

jj runs no Git hooks, so the pre-commit checks (gitleaks, conflict markers, editorconfig, author identity) never
ran on commits made with jj. Push only through `jj git push`, never `git push`, which no hook here gates.
Before `jj git push`:

1. `jj git push --dry-run -b <bookmark>` to list what would move.
2. Scan exactly that range with the configured rules:
   `gitleaks git --config ~/.config/gitleaks/config.toml --log-opts="<remote>/<bookmark>..<commit>"`, or for a
   new bookmark `--log-opts="<commit> --not --remotes"`. A finding stops the push.
3. `jj log -r '<range>' -T 'author.email() ++ "\n"'` to confirm the author identity.
4. Push with the override prefix the hook asks for: `ALLOW_DESTRUCTIVE_GIT=1 jj git push -b <bookmark>`.

A bookmark does not follow new commits. Move it first (`jj bookmark set <b> -r @-`), or push a change directly
with `jj git push -c <change>`, which creates a bookmark named after it.
