# Removes the jj workspace at $PWD, or a legacy git worktree there, then cds
# to the bare repo root. No picker -- always acts on the current worktree.
#
# A bare repo's `git rev-parse --show-toplevel` prints nothing, so this
# naturally refuses to ever target the bare repo itself.
#
# `-f`/`--force` overrides jj's refusal for a non-empty working-copy commit.
# For a legacy git worktree, args pass through to `git worktree remove`, so it
# overrides git's own uncommitted-changes refusal.
function worktree_rm
    # Checked before git: from inside a jj workspace git finds the bare repo,
    # which has no toplevel.
    set -l jj_root (jj workspace root 2>/dev/null)
    if test -n "$jj_root"; and __fzf_ghq_jj_workspace_p $jj_root
        set -l force
        contains -- -f $argv; or contains -- --force $argv; and set force force
        set -l repo_root (jj workspace root --name default 2>/dev/null)
        if test -z "$repo_root"
            echo "worktree_rm: cannot find the default workspace" >&2
            return 1
        end

        set -l remove_output (__fzf_ghq_remove_jj_workspace $jj_root $force)
        if test $status -ne 0
            echo "worktree_rm: $remove_output" >&2
            return 1
        end

        echo "worktree_rm: removed jj workspace $jj_root" >&2
        cd $repo_root
        return
    end

    # LEGACY-GIT: delete once worktree_migrate_jj reports no git worktrees
    set -l target_path (git rev-parse --show-toplevel 2>/dev/null)
    if test -z "$target_path"
        echo "worktree_rm: not inside a git worktree" >&2
        return 1
    end

    set -l git_common_dir (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
    set -l bare_root (string replace -r '/\.git$' '' -- $git_common_dir)

    set -l remove_output (git -C $target_path worktree remove $argv $target_path 2>&1)
    if test $status -ne 0
        echo "worktree_rm: $remove_output" >&2
        return 1
    end

    echo "worktree_rm: removed worktree $target_path" >&2
    cd $bare_root
end
