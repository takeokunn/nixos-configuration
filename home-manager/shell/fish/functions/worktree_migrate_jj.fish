# Removes the legacy git worktrees of every bare repo, from before worktrees
# became jj workspaces. Dry-run by default: prints `<class>\t<path>` per
# worktree and a count per class. With --apply, removes only `removable` ones
# (never --force) and prunes repos that have `prunable` entries.
#
# Classes, first match wins:
#   prunable      directory missing or git marks it prunable
#   locked        `git worktree lock`ed
#   in-use        some process has its cwd in the worktree
#   dirty         `git status --porcelain` is non-empty or fails, untracked
#                 files included; an indeterminate state is never removable
#   ignored       holds an ignored file that `git worktree remove` would
#                 delete, such as a copied .env. Not counted: .claude,
#                 .serena and .direnv, which our own tooling creates as shared
#                 links or regenerable caches
#   unreferenced  HEAD is contained in no branch, remote branch or tag
#   removable     none of the above
#
# `unreferenced` exists because git-maintenance's aggressive gc runs with
# reflogExpire=now: commits reachable only from a detached worktree HEAD would
# be deleted once that worktree is removed. refs/jj/keep is deliberately not
# counted, since `jj util gc` drops it.
#
# Before --apply removes a worktree it re-checks in-use (a fresh lsof scan per
# repo) and the unreferenced test, and skips the worktree if either changed.
#
# Deleting the legacy git support, once this reports no git worktrees:
#   1. grep -rn LEGACY-GIT in this directory and delete each marked block
#   2. delete this file last
function worktree_migrate_jj
    set -l apply false
    for arg in $argv
        switch $arg
            case --apply
                set apply true
            case '*'
                echo "usage: worktree_migrate_jj [--apply]" >&2
                return 2
        end
    end

    # One scan before any classification, failing closed: without it an
    # in-use worktree could be removed under a running process.
    set -l cwds (__worktree_migrate_jj_cwds)
    or begin
        __worktree_migrate_jj_cwds_error
        return 1
    end

    set -l classes prunable locked in-use dirty ignored unreferenced removable
    # Declared function-local up front, so the indirect `set` below updates
    # them. Keyed by class with the hyphen mapped to _, which a variable name
    # cannot contain.
    set -l count_prunable 0
    set -l count_locked 0
    set -l count_in_use 0
    set -l count_dirty 0
    set -l count_ignored 0
    set -l count_unreferenced 0
    set -l count_removable 0
    set -l failed 0

    for repo in (ghq list --full-path)
        __fzf_ghq_bare_p $repo; or continue

        # No .git entry also describes a plain directory nested in another
        # repository, where `git -C` would act on the enclosing repo.
        if test (git -C $repo rev-parse --is-bare-repository 2>/dev/null) != true
            echo "worktree_migrate_jj: '$repo' is not a bare repository; skipping" >&2
            continue
        end

        # Compared against the git-common-dir, as __fzf_ghq_worktree_paths
        # does: ghq's path and git's porcelain can differ after symlink
        # resolution.
        set -l repo_git_dir (git -C $repo rev-parse --path-format=absolute --git-common-dir 2>/dev/null)

        set -l paths
        set -l locked_flags
        set -l prunable_flags
        for line in (git -C $repo worktree list --porcelain 2>/dev/null)
            if string match -q 'worktree *' -- $line
                set -a paths (string replace -r '^worktree ' '' -- $line)
                set -a locked_flags 0
                set -a prunable_flags 0
            else if test (count $paths) -gt 0
                string match -rq '^locked( |$)' -- $line; and set locked_flags[-1] 1
                string match -rq '^prunable( |$)' -- $line; and set prunable_flags[-1] 1
            end
        end

        set -l listed 0
        set -l removed 0
        set -l pruned 0
        set -l has_prunable false
        set -l removable_paths
        for i in (seq (count $paths))
            set -l wt $paths[$i]
            test "$wt" != "$repo_git_dir"; or continue
            set listed (math $listed + 1)

            set -l class
            if test $prunable_flags[$i] = 1 -o ! -d "$wt"
                set class prunable
                set has_prunable true
                set pruned (math $pruned + 1)
            else if test $locked_flags[$i] = 1
                set class locked
            else if __worktree_migrate_jj_in_use $wt $cwds
                set class in-use
            else
                set -l status_lines (git -C $wt status --porcelain --ignored 2>/dev/null)
                set -l git_status $status
                # Ignored entries start with "!!"; any other line is a change.
                # Guarded: `string match` with no operands would read stdin.
                set -l changes
                set -l extra_ignored
                if test (count $status_lines) -gt 0
                    set changes (string match -rv '^!! ' -- $status_lines)
                    set extra_ignored (string match -r '^!! .*' -- $status_lines | string match -rv '^!! (\.claude|\.serena|\.direnv)/?$')
                end
                if test $git_status -ne 0 -o (count $changes) -gt 0
                    set class dirty
                else if test (count $extra_ignored) -gt 0
                    set class ignored
                else if __worktree_migrate_jj_referenced $repo $wt
                    set class removable
                    set -a removable_paths $wt
                else
                    set class unreferenced
                end
            end

            set -l count_var count_(string replace -- - _ $class)
            set $count_var (math $$count_var + 1)
            printf '%s\t%s\n' $class $wt
        end

        if test $apply = true
            # Fresh scan: a process may have entered a worktree since the
            # first one.
            if test (count $removable_paths) -gt 0
                set -l fresh_cwds (__worktree_migrate_jj_cwds)
                or begin
                    __worktree_migrate_jj_cwds_error
                    return 1
                end
                for wt in $removable_paths
                    if __worktree_migrate_jj_in_use $wt $fresh_cwds
                        echo "worktree_migrate_jj: skipping $wt: now in use" >&2
                    else if not __worktree_migrate_jj_referenced $repo $wt
                        echo "worktree_migrate_jj: skipping $wt: HEAD is no longer reachable from a ref" >&2
                    else
                        set -l remove_output (git -C $repo worktree remove $wt 2>&1)
                        if test $status -eq 0
                            set removed (math $removed + 1)
                        else
                            echo "worktree_migrate_jj: failed to remove $wt: $remove_output" >&2
                            set failed (math $failed + 1)
                        end
                    end
                end
            end

            if test $has_prunable = true
                set -l prune_output (git -C $repo worktree prune 2>&1)
                if test $status -ne 0
                    echo "worktree_migrate_jj: failed to prune $repo: $prune_output" >&2
                    set failed (math $failed + 1)
                    set pruned 0
                end
            else
                set pruned 0
            end
            echo "$repo: removed $removed, remaining "(math $listed - $removed - $pruned)
        end
    end

    for class in $classes
        set -l count_var count_(string replace -- - _ $class)
        echo "$class: $$count_var"
    end

    test $failed -eq 0
end

# Prints the working directory of every process, one per line. Fails closed
# when lsof is missing, fails, or reports nothing. The reason is not printed
# here: stderr of a command substitution bypasses the caller's redirections.
function __worktree_migrate_jj_cwds
    command -q lsof; or return 1
    set -l lsof_output (lsof -a -d cwd -Fn 2>/dev/null)
    test (count $lsof_output) -gt 0; or return 1
    set -l cwds (string replace -rf '^n' '' -- $lsof_output)
    test (count $cwds) -gt 0; or return 1
    printf '%s\n' $cwds
end

function __worktree_migrate_jj_cwds_error
    if command -q lsof
        echo "worktree_migrate_jj: lsof failed or reported no working directories; cannot detect in-use worktrees" >&2
    else
        echo "worktree_migrate_jj: lsof not found; cannot detect in-use worktrees" >&2
    end
end

# Succeeds when a cwd in $argv[2..] is the worktree $argv[1] or below it.
function __worktree_migrate_jj_in_use --argument-names wt
    set -l cwds $argv[2..-1]
    # lsof prints resolved paths, so compare against the resolved worktree
    # path. The trailing slash keeps w1 from matching w10.
    set -l real (path resolve -- $wt)
    set -l prefix "$real/"
    contains -- $real $cwds; and return 0
    test (count $cwds) -gt 0; or return 1
    set -l cwd_heads (string sub -l (string length -- $prefix) -- $cwds)
    contains -- $prefix $cwd_heads
end

# Succeeds when HEAD of worktree $argv[2] is contained in a branch, remote
# branch or tag of repo $argv[1]. A failing git is "not referenced".
function __worktree_migrate_jj_referenced --argument-names repo wt
    set -l head (git -C $wt rev-parse HEAD 2>/dev/null)
    or return 1
    set -l containing (git -C $repo for-each-ref --contains $head refs/heads refs/remotes refs/tags 2>/dev/null)
    or return 1
    test (count $containing) -gt 0
end
