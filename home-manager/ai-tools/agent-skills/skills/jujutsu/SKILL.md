---
name: jujutsu
description: "Use when planning or running version-control operations. jj is the default for status, history, diffs, commits, bookmarks, fetch, and push; an uninitialized repository requires confirmation, not a Git fallback. Covers non-interactive use, precise history edits (split, squash, absorb, rebase), conflicts recorded in commits, revset checks, the operation log, shared working copies, workspaces, and pre-push checks."
metadata:
  version: "1.2.0"
---

# Jujutsu (jj)

A `.jj/` directory at the root of the checkout you work in means jj is in use: run version-control operations
through jj and keep GitHub operations on `gh`. Without one, use git; never run `jj git init` to convert a
repository unasked, and if jj is expected but not initialized, report it and ask. A git worktree under a bare
repository's `.worktrees/` has no `.jj/` of its own, so it stays on git even though `jj root` succeeds there:
`jj root` names the bare root, whose jj sees none of the worktree's files, and `jj status` shows no changes.

Two layouts carry `.jj/`:

- A non-bare clone is colocated: `.jj/` and `.git/` share one Git object store, jj exports its changes to Git
  refs, and Git's HEAD is left detached. Read-only git commands still work; git writes (commit, rebase,
  checkout) bypass jj's model and are replaced by the jj commands below.
- A bare `<repo>.git/` holds jj's store at its root in an empty-sparse `default` workspace that tracks no
  files. Work happens in jj workspaces under `.worktrees/`, which have no `.git` of their own: git commands
  there resolve to the bare repository, so `git status` fails and `git log` reads the bare HEAD, not `@`.
  A gate that enumerates files through git (`git ls-files`, treefmt's default walker) then checks nothing and
  exits 0. Confirm its input count, or run it with a filesystem walk (`treefmt --walk filesystem`).

Every jj write (commit, describe, split, squash, absorb, rebase, bookmark, fetch, push, workspace creation)
needs current authorization.

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
| `git add -p` + commit (some files) | `jj split -m "<msg>" <paths>` |
| `git commit --fixup` + autosquash | `jj absorb [<paths>]` |
| `git checkout -b <b>` | `jj bookmark create <b> -r @` |
| `git branch -f <b> <rev>` | `jj bookmark set <b> -r <rev>` (`-B` to move it backwards or sideways) |
| `git fetch` | `jj git fetch` |
| `git push origin <b>` | `jj git push -b <b>` (see Pushing) |
| `git rebase <onto>` | `jj rebase -b @ -o <onto>` (`-o` is `--onto`) |
| `git cherry-pick <sha>` | `jj duplicate <rev> -o <dest>` |
| `git revert <sha>` | `jj revert -r <rev> -o @` |
| `git checkout -- <path>` | `jj restore <path>` |
| `git blame <file>` | `jj file annotate <file>` |
| `git reflog` | `jj op log` |

## Non-interactive use

- Always pass `-m` to `jj commit`, `jj describe`, and `jj split`; without it they open an editor. For
  `jj squash`, pass `-m` or `-u` (keep the destination's message) when both sides carry descriptions.
- `jj split` with paths selects those files without a diff editor; without paths it opens one. Never run
  `jj split` without paths, `jj diffedit`, `jj resolve` without `--list`, or any `-i`/`--interactive` form.
  The guardrail hook does not catch these; an editor waiting for input hangs the call.
- Add `--no-pager` to output-producing commands, and `--git` to diff commands whose output will be parsed:
  the default diff formatter here is difftastic.
- For scripted reads, `jj log --no-graph -r <revset> -T '<template>'` gives stable output.

## Editing history precisely

Each command below rewrites mutable commits and rebases their descendants automatically; none touches
`immutable()` commits (by default `trunk()`, tags, and untracked remote bookmarks) without `--ignore-immutable`,
which needs its own explicit request.

- **Move part of a change:** `jj squash --from <src> --into <dest> <paths>` moves only those paths. Without
  paths it moves everything and abandons an emptied, undescribed source; `-k` keeps it.
- **Distribute fixups:** `jj absorb` sends each hunk of `@` to the closest mutable ancestor that last modified
  those lines, and leaves a hunk in place when the destination is ambiguous. Review where every hunk went
  with `jj op show -p` before building on the result; move a misplaced one with
  `jj squash --from <wrong> --into <right> <path>`, since the guardrail hook blocks `jj undo`.
- **Rebase by scope:** `-r <rev>` moves one commit and reparents its children onto its parent; `-s <rev>` moves
  it with its descendants; `-b <rev>` moves the whole branch relative to the destination. `-A`/`-B` insert
  instead of stacking on top. Choosing `-r` when `-s` was meant strands the descendants on the old base.
- **Reorder independent commits:** `jj parallelize <a>::<b>` makes a linear run into siblings.

## Conflicts are commits, not stops

A rebase or squash that conflicts **still exits 0**: jj records the conflict inside the commit and carries on,
and every descendant inherits it. An exit status is therefore no evidence a rewrite succeeded. After any
rewrite, and before any push, run `jj log --no-graph -r 'conflicts()'` and `jj resolve --list`; an empty result
from both is the check.

When `@` is the conflicted commit, edit the conflict markers in the files directly and the next snapshot
records the resolution. When the conflict sits in an ancestor, jj suggests `jj new <conflicted>`, a `jj new`
onto another revision, which the guardrail hook blocks. Report the conflicted change IDs and ask the user
to resolve them or to authorize a separate workspace for it, instead of working around the block.

## Revsets that answer checks

Confirm what `trunk()` resolves to (`jj log -r 'trunk()'`) before using it as a range bound. A clone may carry
a per-repository alias (`jj config list 'revset-aliases."trunk()"'` shows it); otherwise the built-in tries
`main`, `master`, and `trunk` on `origin` and then `upstream`, and with none of them it resolves to the root
commit, so `trunk()..@` silently spans the whole history.

| Question | Revset |
|---|---|
| My unpushed stack | `trunk()..@` |
| Anything conflicted | `conflicts()` |
| One change rewritten by two sessions | `divergent()` |
| Leftover empty commits | `empty() & mutable() & ~merges() & ~working_copies()` |
| Commits touching a path | `files("<path>")` |
| Find by message | `description(substring:"<text>")` |
| Tips of mutable work | `heads(mutable())` |

`divergent()` turns nonempty when concurrent sessions rewrite the same change: two commits then share one
change ID. Do not abandon either side to tidy it; report both commit IDs and let the user choose.

## The operation log

Every jj command records an operation, including those another session ran in the same repository.
`jj op log` lists them, `jj op show -p <op>` shows what one changed as a diff, and `jj op diff --from <a> --to
<b>` compares two points. Read these to establish what took effect after an interrupted or surprising command.
Restoring or reverting an operation is blocked by the guardrail hook; name the operation ID and ask.

## Shared working copy

The guardrail hook blocks commands that rewrite the files under other sessions: `jj edit`, `jj next`,
`jj prev`, `jj new` onto another revision, `jj abandon`, `jj restore` without paths, `jj undo`, `jj redo`, and
`jj op restore`/`revert`. The equivalents are to start work with plain `jj new`, discard one file with
`jj restore <path>`, and undo a revision with `jj revert`. Spell the built-in subcommand in full and
literally: one produced by `$(...)` or a variable is blocked, and so is any name that is not a built-in,
including the default aliases `b`, `ci`, `desc`, and `st`, because a config file can repoint an alias at any
command. Workspace isolation follows execution-workflow's jj procedure; never substitute Git branch or
worktree writes. A repository with `.jj/` at its root isolates with a jj workspace instead of
`git worktree add`, which would create a git worktree that the root's jj cannot see:
`jj -R <repo> workspace add --no-colocate --sparse-patterns full --name <dir> -r <rev> <repo>/.worktrees/<dir>`, after
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
2. Confirm `jj log --no-graph -r 'conflicts() & (trunk()..<commit>)'` is empty: jj pushes a conflicted commit
   without complaint.
3. Scan exactly that range with the configured rules:
   `gitleaks git --config ~/.config/gitleaks/config.toml --log-opts="<remote>/<bookmark>..<commit>"`, or for a
   new bookmark `--log-opts="<commit> --not --remotes"`. A finding stops the push.
4. `jj log --ignore-working-copy --no-pager -r '<range>' -T 'author.email() ++ "\n"'` to confirm the author identity.
5. When authorized and all checks pass, push with the override prefix the guardrail hook names for this step:
   `ALLOW_DESTRUCTIVE_GIT=1 jj git push -b <bookmark>`. The hook blocks every unprefixed `jj git push` so the
   scan above cannot be skipped; the prefix is for that scanned push only. Never switch to Git to push.

A rejection naming a private commit means a commit by someone else is about to be pushed; stop and ask rather
than overriding it.

A bookmark does not follow new commits. Move it first (`jj bookmark set <b> -r @-`), or push a change directly
with `jj git push -c <change>`, which creates a bookmark named after it. For a stack of pull requests, give each
commit its own bookmark and push them together with repeated `-b`; after rewriting a lower commit, every
bookmark above it moves with its rebased descendant and needs pushing again.
