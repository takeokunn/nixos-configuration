---
name: jujutsu
description: "Use when planning or running version-control operations: jj for every write and history edit, git for reads only. Includes history edits and recovery, conflicts, workspaces, identity and signing, bookmarks, and pre-push checks."
metadata:
  version: "1.4.0"
---

# Jujutsu (jj)

A `.jj/` directory at the root of the checkout you work in means jj is in use. Which tool performs which
operation, and what to do when `.jj/` is missing: CLAUDE.md `environment_facts`. A `.worktrees/` directory
under a bare repository without its own `.jj/` is not a jj workspace: `jj root` succeeds there but names the
bare root, whose jj sees none of the directory's files, and `jj status` shows no changes.

Two layouts carry `.jj/`:

- A non-bare clone is colocated: `.jj/` and `.git/` share one Git object store, jj exports its changes to Git
  refs, and Git's HEAD is left detached. Write through jj; read-only git subcommands also work.
- A bare `<repo>.git/` holds jj's store at its root in an empty-sparse `default` workspace that tracks no
  files. Work happens in jj workspaces under `.worktrees/`, which have no `.git` of their own (no-colocate).

In a no-colocate workspace, tooling that depends on git misbehaves in two ways:

- A gate that enumerates files through git (`git ls-files`, a git-based pre-commit run, a linter fed by
  `git diff`, treefmt's git walker) sees zero files and exits 0. Confirm its input count is nonzero before
  trusting it. Run treefmt with `--walk filesystem`; for a script that lists files through git, feed it
  `jj file list '<fileset>'` instead, and confirm the probe fails on a known-bad input first.
- A script that calls `git rev-parse` fails as run outside a git repository, and a build script that
  locates the repository through git can find the bare parent and fail. Run the underlying command
  directly, or run the recipe in a copy of the tree outside the workspace.

`nix develop <workspace>` there copies the whole tree, untracked directories such as `node_modules`
included, into the store on every call. When the flake is unchanged, write the environment once with
`nix print-dev-env 'git+file://<bare root>?rev=<rev>' > <file>` and source that file for each command.

Which jj writes need no separate authorization, and which need the user's confirmation (push, the default
branch, a shared working copy): CLAUDE.md `hard_rules`.

## Identity and signing

Jujutsu does not read Git's conditional `includeIf` configuration, so a repository that requires a different
identity or signing key than the global jj config gets the global ones unless its jj repo config
(`jj config path --repo`) overrides `user.email`, `user.name`, and `signing.key`. Before the first commit in a
repository, run `jj config get user.email` and `jj config get signing.key` there and confirm both.

Every rewrite (rebase, squash, split, describe, duplicate, `jj sign`) sets the committer to the configured user
and re-signs with the configured key; the author stays. With the wrong configuration, rewriting existing
commits produces three symptoms:

- GitHub shows the commits as Unverified (`reason=unknown_key`), while jj's local `signature.status()` still
  reports `good`: a valid signature by a key that the account has not registered.
- The committer becomes the global identity.
- `jj git push` refuses commits whose author is not `mine()` as private.

Check before pushing rewritten commits:
`jj log --no-graph -r '<range>' -T 'committer.email() ++ " " ++ if(signature, signature.key(), "unsigned") ++ "\n"'`,
and compare the key with `ssh-keygen -lf <public key>`. After pushing, `gh api
repos/<owner>/<repo>/commits/<sha> --jq .commit.verification` shows what GitHub accepted.

To fix it, set the values in the jj repo config (a configuration change: CLAUDE.md `hard_rules`), then
re-sign the unpushed range with `jj sign -r '<range>'`, which also resets the committer. Confirm the content is
unchanged with `jj diff --stat --from <old> --to <new>` (0 files). Commits whose author carries the other
identity need `jj metaedit --update-author -r '<range>'`, with the user's approval because it rewrites
authorship. The guardrail hook blocks per-command `--config user.*`, `JJ_EMAIL`, and `--allow-private`, so
the repo config is the route.

## The working copy is a commit

jj has no staging area. Repository commands normally snapshot files on disk into the working-copy revision `@`,
so `jj status` records edits as well as reporting them. In a checkout shared with other sessions, that snapshot
absorbs their edits into `@` too: before describing, squashing, or pushing `@`, read `jj diff --git` and
confirm every hunk is yours. Pass `--ignore-working-copy` to read-only commands to avoid snapshotting shared
edits, but their view may be stale. To read on-disk state, run `jj status` or `jj diff --git` without the flag:
before committing, to separate your changes from existing ones, or at completion to confirm the changed files,
not as a routine first step.

Confirm a remote ref before relying on a fetched one: compare `jj log --ignore-working-copy -r '<b>@<remote>'
--no-graph -T 'commit_id'` with `gh api repos/<owner>/<repo>/git/ref/heads/<branch> --jq .object.sha` (tags:
`git/ref/tags/<tag>`). A remote-tracking ref left by a fetch without explicit `--remote` and `--branch` is not
freshness evidence.

A change has a stable change ID (letters k-z) and a commit ID that changes on every rewrite. Refer to changes
by change ID across rewrites.

## Command map

Run only the jj column.

| Git habit | jj |
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
| `git fetch` | `jj git fetch --remote <r> --branch <b>` (always name both) |
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
- For machine-readable output, pass `--no-ext-diff` to `git diff` so configured external diff drivers cannot
  alter its output.
- For scripted reads, `jj log --no-graph -r <revset> -T '<template>'` gives stable output.

## Reading without side effects

These read the repository without moving `@` or rewriting a commit. Each still snapshots the files on disk
into `@` first unless you add `--ignore-working-copy`:

- `-T 'json(self) ++ "\n"'` prints one JSON object per commit (change and commit IDs, parents, description,
  author, committer). For a narrower line, combine the keywords `change_id.shortest()`, `commit_id.short()`,
  `description.first_line()`, `bookmarks`, `empty`, `conflict`, `divergent`, and `immutable`.
- Path arguments are filesets: `jj diff --git -r <rev> 'glob:"**/*.nix" & ~glob:"flake.nix"'` limits a diff
  to the matching files. A bare path matches that file or directory relative to the current directory.
- `jj file show -r <rev> <path>` prints a file as it was in a revision, and `jj file list -r <rev>` lists the
  tracked files there. Neither touches the files on disk.
- `jj --at-op <op> log -r <revset>` shows the repository as it was after an earlier operation, without
  restoring it. Take `<op>` from `jj op log`.

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
- **Edit a commit other than `@`:** most rewrites take `-r`. `jj describe -r <rev> -m "<msg>"` rewords it,
  `jj split -r <rev> -m "<msg>" <paths>` splits it (`--parallel` makes the two halves siblings), and
  `jj squash -r <rev>` folds it into its parent.
- **Insert without moving `@`:** `jj new --no-edit -A <rev> -m "<msg>"` creates an empty commit after `<rev>`
  and rebases `<rev>`'s children onto it; `-B` inserts before. `jj new --no-edit <a> <b> -m "<msg>"` creates a
  merge of the two. `--no-edit` is what the guardrail hook allows: without it, `jj new` onto another revision
  moves `@` and is blocked.
- **Copy content between revisions:** `jj restore --from <src> --into <dest> <paths>` gives those paths in
  `<dest>` the content they have in `<src>`. Paths are required, as in every `jj restore` the hook allows.
- **Clean up merges:** `jj simplify-parents -r <rev>` drops a parent that is also an ancestor through another
  parent, without changing any content.

## Recovering an earlier version

The guardrail hook blocks `jj undo` and `jj op restore`, so recover by reading the old state and rewriting
forward:

1. `jj evolog -r <change> -p --git` lists each earlier commit of the change with its patch. `jj op log` and
   `jj --at-op <op> log` locate the state before a bad operation. A rewritten commit stays addressable by its
   old commit ID, as a hidden commit.
2. `jj interdiff --from <old-commit-id> --to <change> --git` shows how the two versions' changes differ.
3. `jj restore --from <old-commit-id> --into <change> <paths>` brings back the old content of those paths,
   or `jj duplicate <old-commit-id> -o <dest>` recreates the whole old version as a new change.

## Conflicts are commits, not stops

A rebase or squash that conflicts **still exits 0**: jj records the conflict inside the commit and carries on,
and every descendant inherits it. An exit status is therefore no evidence a rewrite succeeded. After any
rewrite, compare the old and new tips with `jj interdiff --from <old-tip> --to <new-tip> --git`, then run
`jj log --no-graph -r 'conflicts()'` before pushing. The conflict query must print nothing. In a clean `@`,
`jj resolve --list` exits 2 with "No conflicts found", so its nonzero exit is the passing result there.

When `@` is the conflicted commit, edit the conflict markers in the files directly and the next snapshot
records the resolution. When the conflict sits in an ancestor, jj suggests `jj new <conflicted>`, which the
guardrail hook blocks. If one side is right for a whole file, resolve it in place with
`jj resolve -r <conflicted> --tool :ours <path>` (`:theirs` for the other side). In Claude Code, a permission
rule makes any `--tool` form prompt the user. For a manual resolution, keep `@` on its current descendant,
edit the conflict markers in its file, then run `jj squash --into <conflicted> <paths>` from the repository root. The squash
moves the resolution into the ancestor and leaves a new empty `@` child. Run
`jj log --no-graph -r 'conflicts()'` to confirm that no conflicts remain. Otherwise report the conflicted
change IDs; whether to ask: CLAUDE.md `work_selection`.

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
| Commits whose diff mentions a string | `diff_lines("*<text>*", "<path>")` |
| Another workspace's working copy | `<workspace-name>@` (the trailing `@` is part of the syntax) |
| A state as of an earlier operation | `at_operation(<op>, <revset>)` |

Wrap a revset that must name one commit in `exactly(<revset>, 1)`: it errors instead of silently acting on
several commits or none.

`divergent()` turns nonempty when concurrent sessions rewrite the same change: two commits then share one
change ID. Report both commit IDs. Abandoning either side needs approval: CLAUDE.md `hard_rules`.

## The operation log

Every jj command records an operation, including those another session ran in the same repository.
`jj op log` lists them, `jj op show -p <op>` shows what one changed as a diff, and `jj op diff --from <a> --to
<b>` compares two points. Read these to establish what took effect after an interrupted or surprising command.
Restoring or reverting an operation is blocked by the guardrail hook; name the operation ID in the report
(approval: CLAUDE.md `hard_rules`).

## Shared working copy

The guardrail hook blocks commands that rewrite the files under other sessions: `jj edit`, `jj next`,
`jj prev`, `jj new` onto another revision, `jj abandon`, `jj restore` without paths, `jj undo`, `jj redo`,
`jj op restore`/`revert`, and `jj sparse set`/`reset`/`edit`. It also blocks every git write, and
`ALLOW_DESTRUCTIVE_GIT=1` does not lift that verdict: the prefix applies to the jj verdicts only. Approval
and prefix use: CLAUDE.md `hard_rules`. The equivalents are to start work with plain `jj new`, discard one
file with `jj restore <path>`, and undo a revision with `jj revert`. Spell the built-in subcommand in full and
literally: one produced by `$(...)` or a variable is blocked, and so is any name that is not a built-in,
including the default aliases `b`, `ci`, `desc`, and `st`, because a config file can repoint an alias at any
command. Workspace isolation follows execution-workflow's jj procedure, not `git worktree`. A repository with `.jj/` at its
root isolates with a jj workspace:
`jj -R <repo> workspace add --no-colocate --sparse-patterns full --name <dir> -r <rev> <repo>/.worktrees/<dir>`,
after `mkdir -p <repo>/.worktrees`. Without `--sparse-patterns full` the workspace copies the bare root's empty
sparse set and checks out no files, and `jj status` reports nothing; list the directory before running
anything in a new workspace. `jj sparse reset`, run in that workspace, populates it; the hook blocks it like
the other `jj sparse` writes.

When another workspace rewrites the commit this workspace's `@` sits on, jj reports this working copy as stale.
`jj workspace update-stale` rewrites this workspace's files to match its new `@`.

`jj bisect run` makes each candidate revision the working copy in turn, so the guardrail hook blocks it like
`jj edit`. In a workspace created for the bisect and used by no other session, run
`ALLOW_DESTRUCTIVE_GIT=1 jj bisect run --range '<good>..<bad>' -- <command>`.

## Pushing

jj runs no Git hooks, so the pre-commit checks (gitleaks, conflict markers, editorconfig, author identity) never
ran on commits made with jj, and `jj git push` skips the pre-push hook too. One check still holds: the jj config
sets `git.private-commits = "~mine()"`, so `jj git push` refuses any commit the remote lacks whose author is not
you. The author check only holds while the user config is the real one, so the hook blocks jj commands that
pass `--config user.*`, `--config git.private-commits`, `--config-file`, or `--allow-private`, or set
`JJ_USER`, `JJ_EMAIL`, or `JJ_CONFIG`. The secret scan before `jj git push` is on you:

1. `jj git push --dry-run -b <bookmark>` to list what would move.
2. Confirm `jj log --no-graph -r 'conflicts() & (trunk()..<commit>)'` is empty: jj pushes a conflicted commit
   without complaint.
3. Scan exactly that range with the configured rules (`gitleaks git` is a gitleaks subcommand, not git):
   `gitleaks git --config ~/.config/gitleaks/config.toml --log-opts="<remote>/<bookmark>..<commit>"`, or for a
   new bookmark `--log-opts="<commit> --not --remotes"`. A finding stops the push.
4. `jj log --ignore-working-copy --no-pager -r '<range>' -T 'author.email() ++ "\n"'` to confirm the author
   identity, and for rewritten commits the committer and signing key (Identity and signing).
5. When the push is approved (CLAUDE.md `hard_rules`) and all checks pass, push with
   `ALLOW_DESTRUCTIVE_GIT=1 jj git push -b <bookmark>`. The hook blocks every unprefixed `jj git push` so the
   scan above cannot be skipped.

A rejection naming a private commit means its author is not the configured user: someone else's commit, or
one of yours written under another identity (Identity and signing). Report it; `--allow-private` is blocked.

A bookmark does not follow new commits. Move it first (`jj bookmark set <b> -r @-`), or push a change directly
with `jj git push -c <change>`, which creates a bookmark named after it. For a stack of pull requests, give each
commit its own bookmark and push them together with repeated `-b`; after rewriting a lower commit, every
bookmark above it moves with its rebased descendant and needs pushing again.

## Bookmarks and stacks

- `jj bookmark list --all-remotes` shows each bookmark with its remote counterparts; `--conflicted` lists the
  ones that point at several commits after concurrent moves or a fetch. Settle a conflicted bookmark by
  choosing one target with `jj bookmark set <b> -r <rev>`.
- A remote bookmark fetched but not tracked stays immutable and gets no local bookmark.
  `jj bookmark track <b>@<remote>` creates the local one and keeps it updated on later fetches.
- After trunk moves, restack the whole stack in one rewrite:
  `jj git fetch --remote <r> --branch <default>`, then `jj rebase -s 'roots(trunk()..@)' -o 'trunk()'`.
  Check for conflicts as above before pushing.
