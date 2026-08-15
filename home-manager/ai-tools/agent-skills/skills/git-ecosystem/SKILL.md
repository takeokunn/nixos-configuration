---
name: git-ecosystem
description: Use when Git is invoked programmatically — sanitizing subprocess environment against config injection, identifying a repo with git rev-parse --git-common-dir, parsing diff/status machine-readably, or using git worktree for isolation. Branching and PR submission belong to execution-workflow.
version: 3.0.0
---

Git as a tool other programs drive. Each rule leads with the mechanism that makes it necessary, because **most
of these failures are silent** — the wrong answer looks like a clean result rather than an error.

Out of scope: where work is placed before it starts (branch versus worktree, and the risk signals that force
one) belongs to [execution-workflow](../execution-workflow/SKILL.md); the prohibition on shared-tree mutation
belongs to [core-patterns](../core-patterns/SKILL.md); pull-request submission mechanics — branch naming from
an issue, rebasing onto the upstream default branch, commit splitting, closing keywords, and
`--force-with-lease` versus `--force-if-includes` — live in the `git_mechanics` section of the `/upstream`
command and are not restated here. Conflict resolution and commit organization are workflow decisions rather
than tool surface.

## Identity

**A repository is its common directory.** A linked worktree has its own git directory, so
`git rev-parse --git-dir` returns a distinct path per worktree. What they share is the common directory: the
object database, packed refs, the configuration, and the maintenance lock. **Two worktrees are two working
trees over one repository, not two repositories.**

Deduplicate by the absolute path from `git rev-parse --path-format=absolute --git-common-dir`. Never by
`--git-dir`, and never by the working-directory path — using `--git-dir` makes a tool visit one object store
once per worktree, repacking the same objects repeatedly and contending on the same lock, and **because each
pass succeeds, nothing reports a problem.** Both `--git-dir` and `--git-common-dir` may return a path relative
to the current directory (a plain `.git` at top level), so two different repositories can produce identical
strings — resolve before comparing.

**Ask Git; do not stat the path.** Path-based detection assumes `.git` is a directory at a checkout's root. In
a linked worktree it is a *file* holding a `gitdir:` pointer; in a submodule likewise; a bare repository has no
`.git` entry at all and *is* the git directory; and a path may be deep inside a repository without being its
root. Every one of these is a normal repository that a `stat` on `.git/` classifies incorrectly. Decide
membership from Git's **exit status**, not from the filesystem and not by pattern-matching stdout.

Check these before treating a directory as actionable. Each distinguishes "not a repository" from "a repository
in a state where the operation is meaningless" — different outcomes deserving different reports.

| Check | Meaning |
|---|---|
| `git rev-parse --git-dir` exits zero | Non-zero is "not a repository", not a tool failure |
| Ownership | Git refuses a repository owned by another user, reporting dubious ownership. Recognize and report it; **do not broaden `safe.directory` on the user's behalf** |
| `--is-bare-repository` | No working tree, so any working-tree operation is a category error rather than an empty result |
| `--is-inside-git-dir` | A directory walk can descend into the git directory; acting there corrupts internals |
| `rev-parse --verify HEAD` | Fails with no commits. History inspection, commit-graph writing, and repacking have nothing to do there |
| `--is-shallow-repository` | History-spanning queries return truncated answers on a shallow clone, silently |

## Invoking Git from a program

### Config injection is command execution

`GIT_CONFIG_PARAMETERS` carries configuration overrides into Git *and onward to the processes Git spawns*.
Anything expressible as configuration is expressible there — and several keys name a command Git will execute:
`core.sshCommand` (run for every ssh-transport operation), `core.pager`, `core.editor`, `core.fsmonitor`,
`diff.external`, `credential.helper`, `core.hooksPath`. **Injecting `core.sshCommand` turns an ordinary fetch
into local command execution.**

Any Git invocation with an inherited environment is therefore an execution vector reachable by anything
upstream that can set a variable: a CI job definition, a wrapper script, a parent process, a config file whose
values get exported. **The invocation does not have to touch the network for this to apply — it only has to be
Git.**

`GIT_CONFIG_PARAMETERS` outranks the `GIT_CONFIG_COUNT` / `GIT_CONFIG_KEY_<n>` / `GIT_CONFIG_VALUE_<n>` family,
so `GIT_CONFIG_COUNT=0` does not neutralize it. Forcing the count to zero is worth doing for read-only
invocations, but **only in addition to removing `GIT_CONFIG_PARAMETERS`, never instead of it.**

### Build the child environment; do not filter it

A denylist of known-dangerous names is a claim about a version of Git you are not pinned to. New environment
variables are added over time, and a filter written today passes the ones added tomorrow. **An allowlist
inverts the failure direction:** an unfamiliar variable is dropped rather than honored.

Construct the subprocess environment explicitly from the few variables the operation needs — `PATH`, `HOME` if
credentials or global config are genuinely required, locale, terminal settings. Where an existing codebase
filters instead, these are the minimum to remove:

- **Configuration injection** — `GIT_CONFIG_PARAMETERS`; `GIT_CONFIG_COUNT` and its key/value pairs;
  `GIT_CONFIG_GLOBAL` and `GIT_CONFIG_SYSTEM`, which redirect the global and system config to attacker-chosen
  paths.
- **Repository redirection** — `GIT_DIR`, `GIT_WORK_TREE`, `GIT_COMMON_DIR`, which silently point the command
  at a different repository than the directory it runs in; `GIT_INDEX_FILE`, `GIT_OBJECT_DIRECTORY`,
  `GIT_ALTERNATE_OBJECT_DIRECTORIES`, `GIT_NAMESPACE`; `GIT_CEILING_DIRECTORIES`, where an inherited discovery
  boundary produces "not a repository" for a directory that plainly is one; `GIT_DISCOVERY_ACROSS_FILESYSTEM`.
- **Direct execution** — `GIT_SSH`, `GIT_SSH_COMMAND`, `GIT_PROXY_COMMAND`, `GIT_EXTERNAL_DIFF`, `GIT_PAGER`,
  `GIT_EDITOR`, `GIT_ASKPASS`, `SSH_ASKPASS`.
- **Set rather than remove** — `GIT_TERMINAL_PROMPT=0`, so a non-interactive invocation fails rather than
  blocking forever on a credential prompt.

### Command-scoped safeguards

Pass as `-c key=value`. These outrank repository configuration — which the repository's author controls, not
you — and leave the user's config files untouched. Git propagates them to spawned processes through
`GIT_CONFIG_PARAMETERS`, so they do reach transport helpers and submodule operations.

| Safeguard | Why |
|---|---|
| `protocol.ext.allow=never` | The `ext::` transport takes the command to run **from the remote URL itself**, and its default policy permits direct invocation — which is exactly what a program shelling out looks like. An untrusted URL is otherwise a command |
| `core.sshCommand=ssh` | Pins the transport to the real client, so no configuration at any level can substitute an executable |
| `core.hooksPath=/dev/null` | Hooks are executables supplied *by the repository*. Inspecting a repository must not run code that repository chose |
| `core.fsmonitor=false` | Names a command Git runs when refreshing the index — an execution path on operations with no obvious connection to hooks or transport |
| `credential.helper=` | An empty value resets the list, so no helper runs and no credentials are handed to one |
| `core.pager=cat` | Removes the pager as both an execution path and a formatting source. `--no-pager` is equivalent |
| `gc.auto=0` | Stops an incidental read-only command triggering repacking, which turns a fast inspection into a long write holding a lock others wait on |
| `--no-optional-locks` | Not a config key, but belongs here: stops read-only commands taking the index lock to refresh it, which is what lets inspection run concurrently with an active session |

## Parsing Git output

Reading Git output in a program is a different activity from reading it as a person. **The output a person sees
is shaped by their configuration, and that configuration is frequently machine-global rather than
repository-local.**

**An external diff replaces the format entirely.** `diff.external`, `GIT_EXTERNAL_DIFF`, and per-path drivers
declared through attributes all hand rendering to a third-party program, and structural diff viewers are
commonly enabled globally — so the substitution applies to every repository on that machine, including ones
whose own configuration is clean. Pass `--no-ext-diff` on every `git diff`, `git show`, and `git log -p` a
program parses. **Without it the failure is the dangerous kind: the checker's patterns match nothing in the
replacement format, it finds no problems, and it reports the change clean.** Nothing errors, so nothing prompts
anyone to look. Add `--no-textconv` when the concern is content rather than layout.

**Pin every aspect of the format you parse.** Ordinary configuration alters both shape and substance:
`color.ui` injects escape sequences when output is a terminal; `diff.noprefix` and `diff.mnemonicPrefix` change
or remove the `a/` and `b/` prefixes a patch parser keys on; `core.quotePath` C-quotes non-ASCII paths; and
`diff.renames` and `diff.algorithm` change *which changes are reported at all*. None of these announce
themselves. Specify `--no-color`, `-z` for NUL-delimited paths so filenames containing spaces or newlines
cannot corrupt the split, `--porcelain` (or `=v2`) for status because that format carries a stability guarantee
while `--short` does not, and explicit rename and algorithm settings where the analysis depends on them.

**Prefer plumbing when a program is the reader.** `rev-parse`, `for-each-ref --format=…`, `ls-files`,
`rev-list`, `cat-file`. Their formats are contractual across versions; porcelain output is permitted to
improve, which for a parser is indistinguishable from breaking.

## Maintenance

`git maintenance` decomposes upkeep into individual tasks, which means **the caller, not Git, owns the ordering
and the preconditions.**

`incremental-repack` operates on existing pack files. A repository whose objects are all loose — freshly
created, or never packed — has no `.pack` in `objects/pack`, and the task **fails rather than doing nothing**.
Run `loose-objects` first, confirm a `.pack` now exists, and only then invoke it. In a bulk runner this
ordering is the difference between a clean pass and failures reported for every young repository.

A repository with no commits has nothing to pack or summarize. That is an expected state, not a fault — detect
it and skip, so genuine failures stay visible instead of drowning in expected errors the operator learns to
ignore.

**The maintenance lock is per repository, not per worktree**, because it lives in the common directory with the
object database. A runner enumerating worktrees as separate targets serializes against itself and repeats the
same work — the practical consequence of the identity rule above.

| Task | Note |
|---|---|
| `loose-objects` | Packs loose objects; the prerequisite for incremental-repack |
| `incremental-repack` | Consolidates packs gradually; requires an existing pack |
| `commit-graph` | Accelerates history traversal; nothing to write without commits |
| `pack-refs` | Consolidates loose refs; cheap and independent |
| `prefetch` | Reaches the network, so it belongs under the transport safeguards |
| `gc` | The heavyweight all-in-one. The granular tasks exist so scheduled maintenance can avoid it; scheduling both duplicates work and multiplies lock contention |

`git maintenance register` and `start` write to the user's **global** configuration and install a background
schedule through the platform's service manager. That is a persistent change to the machine, not a repository
operation — confirm before doing it on someone's behalf. And `--auto` governs whether a task is *worth*
running, not whether it *can*; check the preconditions yourself.

## Worktrees

A linked worktree is an independent working tree and index over a shared object store — the isolation primitive
this collection uses in place of operations that mutate a shared working tree.

**Know what is shared.** Objects, most refs, and the configuration live in the common directory and belong to
every worktree. HEAD, the index, and a few per-worktree refs are private. A change to a shared item made from
inside a worktree is a change to every worktree, **which is easy to forget precisely because the working tree
feels separate.** Enumerate with `git worktree list --porcelain`.

**Mirror content back rather than mutating shared state.** When work in a worktree needs to appear in the main
checkout, the reflex is a command that moves the shared tree — and those are exactly the operations that can
absorb or discard a concurrent session's uncommitted work. Copying file content sidesteps the question: it
changes only the destination's files, touching no ref, no index, no shared metadata.

```
rsync -a --delete --exclude='.git' --exclude='<nested worktree dir>' SOURCE/ DEST/
```

This carries unstaged, staged, and untracked changes alike, because at the filesystem level those distinctions
do not exist. **`--delete` makes the destination match the source exactly**, so anything present only in the
destination is removed — confirm the destination is the intended checkout and that the exclusions cover the Git
directory and any nested worktree directories. Getting source and destination the wrong way round destroys the
work being rescued.

**Removal has preconditions.** Removing a worktree discards its working tree, and if that state has not
actually arrived somewhere durable it is gone — a partially-succeeded mirror-back looks identical to a complete
one until someone reads the result. Remove only once the main worktree has no unmerged paths and its complete
diff against the target branch is empty, and keep the branch ref until the reflected state is committed.
`git worktree remove` refuses a dirty tree by design: **that refusal is the safety check**, so treat it as a
signal to investigate rather than something to override. Use `git worktree prune` for metadata left by a
directory removed outside Git.

A worktree created **inside** the repository appears to every other worktree as a large tree of untracked
files. Exclude the containing directory, or tooling that reports or cleans untracked content will act on an
entire second checkout.

## Related

- [core-patterns](../core-patterns/SKILL.md) — the prohibited shared-tree operations these mechanics serve
- [execution-workflow](../execution-workflow/SKILL.md) — when to cut a branch versus isolate in a worktree
- [quality-tools](../quality-tools/SKILL.md) — the layer running checks that depend on the format pinning above
- [investigation-patterns](../investigation-patterns/SKILL.md) — history inspection, where an unpinned format silently invalidates the evidence
- [testing-patterns](../testing-patterns/SKILL.md) — asserting on process exit status, the same class of failure
