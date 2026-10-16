# Removes the jj workspace at $PWD, then cds to the bare repo root. No picker --
# always acts on the current workspace. The default workspace (the bare repo
# root itself) is refused.
#
# `-f`/`--force` overrides jj's refusal for a non-empty working-copy commit.
function worktree_rm
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

    echo "worktree_rm: not inside a jj workspace" >&2
    return 1
end
