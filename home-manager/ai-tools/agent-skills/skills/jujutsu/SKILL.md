---
name: jujutsu
description: "Use when planning or running version-control operations. jj is the default for status, history, diffs, commits, bookmarks, fetch, and push; an uninitialized repository requires confirmation, not a Git fallback. Covers non-interactive use, shared working copies, workspaces, and pre-push checks."
metadata:
  version: "1.1.0"
---

# Jujutsu (jj)

A `.jj/` directory at the root of the checkout you work in means jj is in use: run version-control operations
through jj. Without one, use git; never run `jj git init` to convert a repository unasked. GitHub operations
stay on `gh`. A git worktree under a bare repository's `.worktrees/` has no `.jj/` of its own, so it stays on
git even though `jj root` succeeds there: `jj root` names the bare root, whose jj sees none of the worktree's
files, and `jj st` shows no changes.

Two layouts carry `.jj/`:

- A non-bare clone is colocated: `.jj/` and `.git/` share one Git object store, jj exports its changes to Git
  refs, and Git's HEAD is left detached. Read-only git commands still work; git writes (commit, rebase,
  checkout) bypass jj's model and are replaced by the jj commands below.
- A bare `<repo>.git/` holds jj's store at its root in an empty-sparse `default` workspace that tracks no
  files. Work happens in jj workspaces under `.worktrees/`, which have no `.git` of their own: git commands
  there resolve to the bare repository, so `git status` fails and `git log` reads the bare HEAD, not `@`.

Use jj for routine version control and `gh` for GitHub. Check initialization with `jj root` or the repository's
`.jj/` directory. If jj is not initialized, report it and ask before initialization or migration; do not
silently fall back to Git. A prompt update does not authorize repository migration or version-control writes.

Inspect the actual layout rather than assuming colocation. In a colocated repository, jj and Git share an
object store, and jj exports changes to Git refs. Necessary read-only Git plumbing remains available;
Git writes bypass jj's model and are not an alternate workflow. Any exception needs an explicit request.


## The working copy is a commit

jj has no staging area. Repository commands normally snapshot files on disk into the working-copy revision `@`,
so `jj status` records edits as well as reporting them. In a checkout shared with other sessions, that snapshot
absorbs their edits into `@` too: before describing, squashing, or pushing `@`, read `jj diff --git` and
confirm every hunk is yours. Pass `--ignore-working-copy` to read-only commands to avoid snapshotting shared
edits. Their view may be stale: inspect on-disk changes and untracked files separately before relying on it.

A change has a stable change ID (letters k-z) and a commit ID that changes on every rewrite. Refer to changes
by change ID across rewrites.

## Command map

| Git | jj |
|---|---|
| `git status` | `jj status` |
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
`jj restore <path>`, and undo a revision with `jj revert`. Spell the built-in subcommand in full and
literally: one produced by `$(...)` or a variable is blocked, and so is any name that is not a built-in,
including the default aliases `b`, `ci`, `desc`, and `st`, because a config file can repoint an alias at any
command. Every jj write (commit, describe, squash, rebase, bookmark, fetch, push, workspace creation) needs
current authorization. Workspace isolation follows execution-workflow's jj procedure; never substitute Git
branch or worktree writes. A repository with `.jj/` at its root isolates with a jj workspace instead of
`git worktree add`, which would create a git worktree that the root's jj cannot see:
`jj -R <repo> workspace add --sparse-patterns full --name <dir> -r <rev> <repo>/.worktrees/<dir>`, after
`mkdir -p <repo>/.worktrees`. Without `--sparse-patterns full` the workspace copies the bare root's empty
sparse set and checks out no files.

## Pushing

jj runs no Git hooks, so the pre-commit checks (gitleaks, conflict markers, editorconfig, author identity) never
ran on commits made with jj, and `jj git push` skips the pre-push hook too. Two checks still hold: the jj config
sets `git.private-commits = "~mine()"`, so `jj git push` refuses any commit the remote lacks whose author is not
you; and a plain `git push` runs the pre-push hook, which scans the pushed range with gitleaks. The author check
only holds while the user config is the real one, so the hook blocks jj commands that pass `--config user.*`,
`--config git.private-commits`, `--config-file`, or `--allow-private`, or set `JJ_USER`, `JJ_EMAIL`, or
`JJ_CONFIG`; it likewise blocks `--no-verify` and `core.hooksPath` overrides on git. The secret scan before
`jj git push` is on you:

1. `jj git push --dry-run -b <bookmark>` to list what would move.
2. Scan exactly that range with the configured rules:
   `gitleaks git --config ~/.config/gitleaks/config.toml --log-opts="<remote>/<bookmark>..<commit>"`, or for a
   new bookmark `--log-opts="<commit> --not --remotes"`. A finding stops the push.
3. `jj log --ignore-working-copy --no-pager -r '<range>' -T 'author.email() ++ "\n"'` to confirm the author identity.
4. When authorized and all checks pass, push with the override prefix the guardrail hook names for this step:
   `ALLOW_DESTRUCTIVE_GIT=1 jj git push -b <bookmark>`. The hook blocks every unprefixed `jj git push` so the
   scan above cannot be skipped; the prefix is for that scanned push only. Never switch to Git to push.

A rejection naming a private commit means a commit by someone else is about to be pushed; stop and ask rather
than overriding it.

A bookmark does not follow new commits. Move it first (`jj bookmark set <b> -r @-`), or push a change directly
with `jj git push -c <change>`, which creates a bookmark named after it.
