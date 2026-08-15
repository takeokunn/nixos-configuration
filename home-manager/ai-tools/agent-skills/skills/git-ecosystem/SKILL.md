---
name: git-ecosystem
description: Use when Git is invoked programmatically — sanitizing subprocess environment against config injection, identifying a repo with git rev-parse --git-common-dir, parsing diff/status machine-readably, or using git worktree for isolation. Branching and PR submission belong to execution-workflow.
version: 2.3.0
---

<purpose>
  Provide durable knowledge about Git as a tool that other programs drive: how to establish
  what a repository is, how to invoke Git without inheriting an execution vector, how to read
  history mechanically without a user's configuration silently rewriting the output, and how
  to run maintenance and worktrees correctly. Each rule leads with the mechanism that makes it
  necessary, because most of these failures are silent — the wrong answer looks like a clean
  result rather than an error.
</purpose>

<scope>
  <focus>Git as a tool: repository identity, invoking Git safely from programs, machine-readable inspection of history, repository maintenance, and worktrees as an isolation primitive</focus>
  <defer_to skill="execution-workflow#branch_isolation_procedure">
    Where work is placed before it starts: determining the default branch, cutting a feature branch from fetched remote state, and the risk signals that force a worktree instead of an in-place branch. This skill supplies the worktree mechanics; that procedure decides when to invoke them.
  </defer_to>
  <defer_to command="upstream#git_mechanics">
    Pull-request submission mechanics: branch naming from an issue, rebasing onto the upstream default branch, squashing to one reviewable commit, closing keywords, and the --force-with-lease vs --force-if-includes distinction. Not restated here.
  </defer_to>
  <defer_to skill="core-patterns#parallel_project_isolation">
    The prohibition on shared-tree mutation across concurrent sessions and the list of prohibited operations. This skill adds only the worktree mechanics that pattern depends on.
  </defer_to>
  <out_of_scope>
    Conflict resolution, commit organization, and hooks as quality gates are workflow decisions rather than tool surface. This skill covers the tool surface underneath them and does not state the decisions.
  </out_of_scope>
  <unique_coverage>
    Worktree-aware repository identity via --git-common-dir, environment sanitization for Git subprocesses and the config-injection execution vector, format-pinning for parsed Git output, and git maintenance task preconditions.
  </unique_coverage>
</scope>

<concepts>
  <concept name="common_dir">The directory holding state shared by every worktree of a repository: the object database, most refs, and the repository configuration. Linked worktrees have their own git directory but the same common directory.</concept>
  <concept name="config_is_executable">Several configuration keys name a command that Git will run. Anything that can set configuration can therefore run code, which makes configuration an untrusted input whenever its source is untrusted.</concept>
  <concept name="porcelain_vs_plumbing">Porcelain commands are shaped for humans and are influenced by user configuration; plumbing commands have documented, stable output. A parser should consume plumbing, or pin the porcelain format explicitly.</concept>
  <concept name="silent_wrongness">The characteristic Git-automation failure is not an error but an empty or partial result that reads as success: a diff a checker cannot parse, a repository visited twice, a task skipped.</concept>
</concepts>

<tools>
  <tool>Bash - Invoke Git with an explicitly constructed environment and command-scoped overrides</tool>
  <tool>Read - Inspect configuration files and hook directories before trusting a repository</tool>
  <tool>Grep - Locate places where a project shells out to Git and check how it builds the child environment</tool>
</tools>

<repository_identity>
  <description>
    Before a program can act on a repository it must decide what "a repository" is and whether
    two paths refer to the same one. Both questions have answers that look obvious and are wrong
    in the presence of worktrees, submodules, and bare repositories.
  </description>

  <principle name="identity_is_the_common_dir">
    <why>
      A linked worktree has its own git directory, typically under
      &lt;main&gt;/.git/worktrees/&lt;name&gt;, so `git rev-parse --git-dir` returns a distinct path for
      each worktree. What they share is the common directory: the object database, packed refs,
      the repository configuration, and the maintenance lock. Two worktrees are two working
      trees over one repository, not two repositories.
    </why>
    <implication>
      Deduplicate by the absolute path from `git rev-parse --git-common-dir`, never by
      `--git-dir` and never by the working-directory path. Using `--git-dir` makes a tool visit
      one object store once per worktree: it repacks the same objects repeatedly and contends on
      the same lock, and because each pass succeeds, nothing reports a problem.
    </implication>
    <detail>
      `--git-dir` and `--git-common-dir` may return a path relative to the current directory
      (plain `.git` at the top level), so two different repositories can produce identical
      strings. Resolve before comparing: `git rev-parse --path-format=absolute --git-common-dir`,
      or absolutize the result yourself against the directory the command ran in.
    </detail>
  </principle>

  <principle name="ask_git_do_not_stat_the_path">
    <why>
      Path-based detection assumes `.git` is a directory at the root of a checkout. In a linked
      worktree `.git` is a *file* containing a `gitdir:` pointer; in a submodule it is likewise a
      file; a bare repository has no `.git` entry at all and *is* the git directory; and a path
      may be deep inside a repository without being its root. Every one of these is a normal
      repository that a `stat` on `.git/` classifies incorrectly.
    </why>
    <implication>
      Determine repository membership by running Git and checking its *exit status*, not by
      inspecting the filesystem and not by pattern-matching its stdout. Git already implements
      discovery, including the cases a hand-rolled check omits.
    </implication>
  </principle>

  <preconditions>
    <description>Check these before treating a directory as an actionable repository. Each one distinguishes "not a repository" from "a repository in a state where the intended operation is meaningless", which are different outcomes and deserve different reporting.</description>
    <check name="is_a_repository">`git rev-parse --git-dir` exits zero. Treat a non-zero exit as "not a repository", not as a failure of the tool.</check>
    <check name="ownership">Current Git refuses to operate on a repository owned by a different user, reporting dubious ownership. A tool running as another user hits this on every repository. Recognize and report it; do not respond by broadening `safe.directory` on the user's behalf.</check>
    <check name="bare_or_worktree">`git rev-parse --is-bare-repository`. A bare repository has no working tree, so any working-tree operation is a category error rather than an empty result.</check>
    <check name="inside_git_dir">`git rev-parse --is-inside-git-dir`. A directory walk can descend into the git directory itself; acting there corrupts repository internals.</check>
    <check name="has_commits">`git rev-parse --verify HEAD` fails on a repository with no commits. Many operations — history inspection, commit-graph writing, repacking — have nothing to do there and should be skipped rather than attempted.</check>
    <check name="shallow">`git rev-parse --is-shallow-repository`. History-spanning queries return truncated answers on a shallow clone, silently.</check>
  </preconditions>
</repository_identity>

<invoking_git_from_programs>
  <description>
    Defensive guidance for hardening your own tooling. A program that shells out to Git and lets
    the child inherit its environment has handed control of Git's configuration to whoever can
    influence that environment.
  </description>

  <principle name="config_injection_is_command_execution">
    <why>
      `GIT_CONFIG_PARAMETERS` carries configuration overrides into Git and onward to the
      processes Git itself spawns. Anything expressible as configuration is expressible there —
      and several configuration keys name a command Git will execute: `core.sshCommand` (run for
      every operation over the ssh transport), `core.pager`, `core.editor`, `core.fsmonitor`,
      `diff.external`, `credential.helper`, and `core.hooksPath`. Injecting `core.sshCommand`
      turns an ordinary fetch into local command execution.
    </why>
    <implication>
      Any Git invocation with an inherited environment is an execution vector reachable by
      anything upstream that can set a variable: a CI job definition, a wrapper script, a parent
      process, a configuration file whose values get exported. The invocation does not have to
      touch the network or a remote for this to apply — it only has to be Git.
    </implication>
    <detail>
      `GIT_CONFIG_PARAMETERS` outranks the `GIT_CONFIG_COUNT` / `GIT_CONFIG_KEY_&lt;n&gt;` /
      `GIT_CONFIG_VALUE_&lt;n&gt;` family, so setting `GIT_CONFIG_COUNT=0` does not neutralize it.
      Forcing the count to zero is worth doing for read-only invocations, but only in addition to
      removing `GIT_CONFIG_PARAMETERS`, never instead of it.
    </detail>
  </principle>

  <principle name="build_the_child_environment_do_not_filter_it">
    <why>
      A denylist of known-dangerous variable names is a claim about a version of Git you are not
      pinned to. New environment variables are added over time, and a filter written today
      passes the ones added tomorrow. An allowlist inverts the failure direction: an unfamiliar
      variable is dropped rather than honored.
    </why>
    <implication>
      Construct the subprocess environment explicitly from the few variables the operation needs
      (`PATH`, `HOME` if credentials or global config are genuinely required, locale, terminal
      settings) rather than copying the parent environment and subtracting. Where an existing
      codebase filters, the categories below are the minimum to remove.
    </implication>
  </principle>

  <environment_sanitization>
    <category name="configuration_injection">
      <variable>GIT_CONFIG_PARAMETERS — carries arbitrary overrides and outranks the count-based family</variable>
      <variable>GIT_CONFIG_COUNT and its GIT_CONFIG_KEY_&lt;n&gt; / GIT_CONFIG_VALUE_&lt;n&gt; pairs — force the count to zero for read-only work</variable>
      <variable>GIT_CONFIG_GLOBAL and GIT_CONFIG_SYSTEM — redirect the global and system config files to attacker-chosen paths</variable>
    </category>
    <category name="repository_redirection">
      <variable>GIT_DIR, GIT_WORK_TREE, GIT_COMMON_DIR — silently point the command at a different repository than the directory it runs in</variable>
      <variable>GIT_INDEX_FILE, GIT_OBJECT_DIRECTORY, GIT_ALTERNATE_OBJECT_DIRECTORIES, GIT_NAMESPACE — redirect index, object storage, and the visible ref namespace</variable>
      <variable>GIT_CEILING_DIRECTORIES — an inherited discovery boundary can hide a repository the caller configured explicitly, producing "not a repository" for a directory that plainly is one</variable>
      <variable>GIT_DISCOVERY_ACROSS_FILESYSTEM — changes which enclosing repository a path is judged to belong to</variable>
    </category>
    <category name="direct_execution">
      <variable>GIT_SSH, GIT_SSH_COMMAND, GIT_PROXY_COMMAND — name the transport program</variable>
      <variable>GIT_EXTERNAL_DIFF, GIT_PAGER, GIT_EDITOR — name programs Git runs on output or content</variable>
      <variable>GIT_ASKPASS and SSH_ASKPASS — name a program run to obtain credentials</variable>
    </category>
    <category name="set_rather_than_remove">
      <variable>GIT_TERMINAL_PROMPT=0 — a non-interactive invocation should fail rather than block forever on a credential prompt</variable>
    </category>
  </environment_sanitization>

  <command_scoped_safeguards>
    <description>
      Pass these as `-c key=value` on the invocation. Command-scoped overrides take precedence
      over repository configuration — which the repository's author controls, not you — and they
      leave the user's config files untouched. Git propagates them to the processes it spawns
      through `GIT_CONFIG_PARAMETERS`, the same mechanism described above, so they do reach
      transport helpers and submodule operations.
    </description>
    <safeguard key="protocol.ext.allow=never">The `ext::` transport takes the command to run from the remote URL itself. Its default policy permits direct invocation, and a program shelling out looks exactly like direct invocation. A URL from an untrusted source is otherwise a command.</safeguard>
    <safeguard key="core.sshCommand=ssh">Pins the transport program to the real client so no configuration reachable at any level can substitute a different executable.</safeguard>
    <safeguard key="core.hooksPath=/dev/null">Hooks are executables supplied by the repository. Inspecting a repository must not run code that repository chose.</safeguard>
    <safeguard key="core.fsmonitor=false">The fsmonitor setting names a command Git runs when refreshing the index — an execution path that opens on operations with no obvious connection to hooks or transport.</safeguard>
    <safeguard key="credential.helper=">An empty value resets the helper list, so no configured helper is executed and no credentials are handed to one during an operation that should not need them.</safeguard>
    <safeguard key="core.pager=cat">Removes the pager as both an execution path and a source of formatting. `--no-pager` on the invocation is equivalent and shorter.</safeguard>
    <safeguard key="gc.auto=0">Prevents an incidental read-only command from triggering automatic repacking, which turns a fast inspection into a long write and takes a lock other sessions are waiting on.</safeguard>
    <safeguard key="--no-optional-locks">Not a config key but belongs with them: it stops read-only commands from taking the index lock to refresh it, which is what lets inspection run concurrently with an active session in the same tree.</safeguard>
  </command_scoped_safeguards>
</invoking_git_from_programs>

<mechanical_inspection>
  <description>
    Reading Git output in a program is a different activity from reading it as a person. The
    output a person sees is shaped by their configuration, and that configuration is frequently
    machine-global rather than repository-local.
  </description>

  <principle name="external_diff_replaces_the_format_entirely">
    <why>
      `diff.external`, the `GIT_EXTERNAL_DIFF` variable, and per-path diff drivers declared
      through attributes all hand rendering to a third-party program. Structural diff viewers are
      commonly enabled globally rather than per repository, so the substitution applies to every
      repository on that machine — including ones whose own configuration is clean.
    </why>
    <implication>
      Pass `--no-ext-diff` on every `git diff`, `git show`, and `git log -p` whose output a
      program parses. The failure without it is the dangerous kind: the checker's patterns match
      nothing in the replacement format, it finds no problems, and it reports the change clean.
      Nothing errors, so nothing prompts anyone to look. Add `--no-textconv` when the concern is
      content rather than layout, since textconv filters replace file contents before diffing.
    </implication>
  </principle>

  <principle name="pin_every_aspect_of_the_format_you_parse">
    <why>
      Beyond external diff, ordinary user configuration alters both the shape and the substance
      of porcelain output: `color.ui` injects escape sequences when the output is a terminal,
      `diff.noprefix` and `diff.mnemonicPrefix` change or remove the `a/` and `b/` path prefixes
      a patch parser keys on, `core.quotePath` C-quotes non-ASCII paths, and `diff.renames` and
      `diff.algorithm` change which changes are reported at all. None of these announce themselves.
    </why>
    <implication>
      Specify what you depend on rather than inheriting it: `--no-color`, `-z` for
      NUL-delimited paths so filenames containing spaces, quotes, or newlines cannot corrupt the
      split, `--porcelain` (or `--porcelain=v2`) for status because that format carries a
      stability guarantee while `--short` does not, and explicit rename and algorithm settings
      when the analysis depends on them.
    </implication>
  </principle>

  <principle name="prefer_plumbing_when_a_program_is_the_reader">
    <why>
      Plumbing commands exist precisely to have output that does not change with the reader's
      preferences, and their formats are contractual across versions. Porcelain output is
      permitted to improve, which for a parser is indistinguishable from breaking.
    </why>
    <implication>
      Reach for `rev-parse`, `for-each-ref --format=...`, `ls-files`, `rev-list`, and `cat-file`
      in preference to scraping their porcelain equivalents. Where only porcelain will do, pin
      the format as above and treat the parser as version-sensitive.
    </implication>
  </principle>
</mechanical_inspection>

<maintenance>
  <description>
    `git maintenance` decomposes repository upkeep into individual tasks so scheduled work can be
    incremental. The decomposition means the caller, not Git, owns the ordering and the
    preconditions.
  </description>

  <principle name="incremental_repack_requires_an_existing_pack">
    <why>
      `git maintenance run --task=incremental-repack` operates on existing pack files. A
      repository whose objects are all loose — freshly created, or one that has never been packed
      — has no `.pack` in its `objects/pack` directory, and the task fails rather than doing
      nothing.
    </why>
    <implication>
      Run `loose-objects` first, which packs loose objects and thereby establishes the
      precondition. Then check that `objects/pack` actually contains a `.pack` file, and only
      then invoke `incremental-repack`. In a bulk runner this ordering is the difference between
      a clean pass and a run that reports failures for every young repository it touches.
    </implication>
  </principle>

  <principle name="an_empty_repository_is_a_skip_not_a_failure">
    <why>
      A repository with no commits has no objects to pack and no history to summarize, so
      commit-graph and repack tasks have nothing to act on. This is an expected state — a
      just-initialized repository, or one cloned from an empty remote — not a fault.
    </why>
    <implication>
      Detect it with the `has_commits` precondition above and skip, so genuine failures stay
      visible instead of being drowned in expected errors that a bulk runner's operator learns
      to ignore.
    </implication>
  </principle>

  <principle name="the_maintenance_lock_is_per_repository_not_per_worktree">
    <why>
      Maintenance locks in the common directory, because that is where the object database lives.
      Every worktree of a repository therefore contends for one lock.
    </why>
    <implication>
      A runner that enumerates worktrees as separate targets serializes against itself and does
      the same work repeatedly. This is the practical consequence of the `--git-common-dir`
      identity rule, and the reason that rule is worth enforcing rather than treating as pedantry.
    </implication>
  </principle>

  <task_notes>
    <note task="loose-objects">Packs loose objects. The prerequisite for incremental-repack on a repository that has never been packed.</note>
    <note task="incremental-repack">Consolidates pack files gradually. Requires at least one existing pack.</note>
    <note task="commit-graph">Writes the commit-graph file that accelerates history traversal. Nothing to write without commits.</note>
    <note task="pack-refs">Consolidates loose refs. Cheap and independent of the pack tasks.</note>
    <note task="prefetch">Updates remote-tracking data without changing local refs. Reaches the network, so it belongs under the transport safeguards above.</note>
    <note task="gc">The heavyweight all-in-one. The granular tasks exist so scheduled maintenance can avoid it; scheduling both duplicates work and multiplies lock contention.</note>
  </task_notes>

  <caution>
    `git maintenance register` and `git maintenance start` write to the user's global
    configuration and install a background schedule through the platform's service manager. That
    is a persistent change to the machine, not a repository operation — confirm before doing it
    on someone's behalf. Note also that `--auto` governs whether a task is worth running, not
    whether it can run; check the preconditions yourself rather than expecting `--auto` to
    absorb them.
  </caution>
</maintenance>

<worktrees>
  <description>
    A linked worktree is an independent working tree and index over a shared object store. It is
    the isolation primitive this collection uses in place of operations that mutate a shared
    working tree. See `core-patterns#parallel_project_isolation` for the prohibitions; what
    follows is the mechanics.
  </description>

  <principle name="know_what_is_shared_and_what_is_not">
    <why>
      Objects, most refs, and the repository configuration live in the common directory and are
      shared by every worktree. HEAD, the index, and a handful of per-worktree refs are private
      to each. A change to a shared item made from inside a worktree is a change to every
      worktree, which is easy to forget precisely because the working tree feels separate.
    </why>
    <implication>
      Treat worktrees as isolated for file content and checkout state, and as shared for
      everything that lives in the common directory. Enumerate them with
      `git worktree list --porcelain`, which is the machine-readable form.
    </implication>
  </principle>

  <principle name="mirror_content_back_rather_than_mutating_shared_state">
    <why>
      When work done in a worktree needs to appear in the main checkout, the reflex is a command
      that moves the shared tree — and those are exactly the operations that can absorb or
      discard a concurrent session's uncommitted work. Copying file content sidesteps the
      question: it changes only the destination's files, touching no ref, no index, and no
      shared metadata.
    </why>
    <implication>
      Propagate with a content sync that excludes Git's own state, for example
      `rsync -a --delete --exclude='.git' --exclude='&lt;nested worktree dir&gt;' SOURCE/ DEST/`.
      This carries unstaged, staged, and untracked changes alike, because at the filesystem level
      those distinctions do not exist.
    </implication>
    <detail>
      `--delete` makes the destination match the source exactly, so anything present only in the
      destination is removed. Before running it, confirm the destination is the intended checkout
      and that the exclusions cover the Git directory and any nested worktree directories.
      Getting the source and destination the wrong way round destroys the work being rescued.
    </detail>
  </principle>

  <principle name="removal_has_preconditions">
    <why>
      Removing a worktree discards its working tree. If the state it held has not actually
      arrived somewhere durable, it is gone — and a mirror-back that partially succeeded looks
      identical to one that fully succeeded until someone reads the result.
    </why>
    <implication>
      Remove a linked worktree only once the main worktree has no unmerged paths and its complete
      working-tree diff against the intended target branch is empty. Keep the branch ref until the
      reflected state is committed. `git worktree remove` refuses a dirty tree by design; that
      refusal is the safety check, so treat it as a signal to investigate rather than something to
      override. Use `git worktree prune` to clean up metadata for a directory removed outside Git.
    </implication>
  </principle>

  <principle name="a_nested_worktree_must_be_ignored">
    <why>
      A worktree created inside the repository appears to every other worktree as a large tree of
      untracked files.
    </why>
    <implication>
      Exclude the containing directory, or tooling that reports or cleans untracked content will
      act on an entire second checkout.
    </implication>
  </principle>
</worktrees>

<best_practices>
  <practice priority="critical">Build the environment for a Git subprocess explicitly; never let it inherit the parent environment wholesale.</practice>
  <practice priority="critical">Apply the command-scoped safeguard set on programmatic invocations: no external transports, pinned ssh command, no hooks, no fsmonitor, no credential helper, no automatic gc.</practice>
  <practice priority="critical">Pass --no-ext-diff whenever a program parses diff output.</practice>
  <practice priority="high">Identify and deduplicate repositories by the absolute --git-common-dir, resolved before comparison.</practice>
  <practice priority="high">Decide repository membership from Git's exit status, not from the presence of a .git directory.</practice>
  <practice priority="high">Run loose-objects before incremental-repack, and verify a pack file exists in between.</practice>
  <practice priority="high">Use worktrees for isolation and mirror content back with an excluded-metadata file sync.</practice>
  <practice priority="medium">Prefer plumbing commands for parsed output; where porcelain is unavoidable, pin color, prefixes, path quoting, and rename detection.</practice>
  <practice priority="medium">Use -z for any output containing paths.</practice>
  <practice priority="medium">Treat an empty or shallow repository as a distinct, reportable state rather than a failure.</practice>
</best_practices>

<anti_patterns>
  <avoid name="inherited_environment_for_git_subprocess">
    <description>Spawning Git with the parent process environment passed through unchanged.</description>
    <instead>Construct the child environment from an allowlist, and remove the configuration, redirection, and execution variable families at minimum.</instead>
  </avoid>
  <avoid name="denylist_only_sanitization">
    <description>Filtering a fixed list of dangerous variable names out of an inherited environment.</description>
    <instead>Allowlist what the operation needs, so variables introduced by later Git versions are dropped rather than honored.</instead>
  </avoid>
  <avoid name="count_zero_without_removing_parameters">
    <description>Setting GIT_CONFIG_COUNT=0 and considering environment-sourced configuration handled.</description>
    <instead>Remove GIT_CONFIG_PARAMETERS as well; it outranks the count-based family.</instead>
  </avoid>
  <avoid name="path_based_repository_detection">
    <description>Deciding a directory is a repository by checking for a .git subdirectory.</description>
    <instead>Run Git and check the exit status; .git is a file in worktrees and submodules and absent in bare repositories.</instead>
  </avoid>
  <avoid name="dedupe_by_git_dir_or_working_path">
    <description>Treating each worktree, or each working directory, as a separate repository.</description>
    <instead>Deduplicate by absolute --git-common-dir; worktrees share one object store and one maintenance lock.</instead>
  </avoid>
  <avoid name="parsing_diff_without_no_ext_diff">
    <description>Grepping git diff output in a script while a machine-global external diff tool is configured.</description>
    <instead>Pass --no-ext-diff. Otherwise the check reads a format it does not understand and reports clean.</instead>
  </avoid>
  <avoid name="inheriting_output_format_config">
    <description>Parsing output whose color, path prefixes, quoting, or rename detection come from whatever the user configured.</description>
    <instead>Pin each of them on the invocation, or read plumbing output instead.</instead>
  </avoid>
  <avoid name="incremental_repack_without_a_pack">
    <description>Invoking incremental-repack on a repository whose objects are all loose.</description>
    <instead>Run loose-objects first and confirm a .pack exists before proceeding.</instead>
  </avoid>
  <avoid name="shared_tree_mutation_for_isolation">
    <description>Reaching for git stash, git checkout of another branch, git reset --hard, or git clean to make room for a task. These mutate a working tree that another session may be using, and the damage is to uncommitted work that no ref points at.</description>
    <instead>Create a worktree, work there, and mirror the result back as file content. See core-patterns#parallel_project_isolation.</instead>
  </avoid>
  <avoid name="force_removing_a_dirty_worktree">
    <description>Overriding git worktree remove's refusal, or deleting a worktree before confirming its state arrived elsewhere.</description>
    <instead>Satisfy the removal preconditions first: no unmerged paths, an empty diff against the target branch, and the branch ref retained until the state is committed.</instead>
  </avoid>
  <avoid name="installing_scheduled_maintenance_unasked">
    <description>Running git maintenance register or start as part of an automated flow.</description>
    <instead>These write global configuration and install a background schedule on the machine. Confirm before making that change for someone.</instead>
  </avoid>
</anti_patterns>

<error_escalation>
  <examples>
    <example severity="low">A repository skipped because it is empty or shallow, reported as such</example>
    <example severity="medium">Maintenance run redundantly across worktrees of one repository through --git-dir deduplication</example>
    <example severity="high">A diff-parsing check reporting clean because an external diff driver replaced the format</example>
    <example severity="critical">A Git subprocess spawned with an inherited environment, making config injection reachable as command execution</example>
  </examples>
</error_escalation>

<constraints>
  <must>Sanitize or explicitly construct the environment of every programmatically invoked Git subprocess</must>
  <must>Resolve repository identity through the absolute common directory</must>
  <must>Disable external diff drivers before parsing diff output</must>
  <must>Check task preconditions before invoking maintenance tasks</must>
  <avoid>Working-tree or shared-state mutation as a substitute for worktree isolation</avoid>
  <avoid>Denylist-only environment filtering</avoid>
  <avoid>Parsing porcelain output whose format is inherited from user configuration</avoid>
</constraints>

<related_skills>
  <skill name="core-patterns">Owns parallel_project_isolation: the prohibited shared-tree operations and the worktree requirement this skill supplies mechanics for</skill>
  <skill name="investigation-patterns">Evidence-gathering discipline for history inspection, where an unpinned output format silently invalidates the evidence</skill>
  <skill name="quality-tools">The layer that runs checks; diff-parsing checks in particular depend on the format-pinning rules here</skill>
  <skill name="testing-patterns">Asserting on process exit status, the same class of failure as a check that reads nothing and passes</skill>
  <skill name="execution-workflow">Owns branch_isolation_procedure: when to cut a feature branch versus isolate in a worktree, and the rule that a pull request targets only the default branch</skill>
</related_skills>

<related_agents>
  <agent name="devops">CI runners and automation that invoke Git with environments assembled from job configuration</agent>
  <agent name="security">Review of config-injection exposure in code that shells out to Git</agent>
  <agent name="explore">Locating every site in a codebase where Git is invoked as a subprocess</agent>
</related_agents>
