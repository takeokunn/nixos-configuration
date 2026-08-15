# Removes the worktree at $PWD, then cds to the bare repo root. No picker --
# always acts on the current worktree.
#
# A bare repo's `git rev-parse --show-toplevel` prints nothing, so this
# naturally refuses to ever target the bare repo itself.
#
# Args pass through to `git worktree remove`, so `-f`/`--force` overrides
# git's own uncommitted-changes refusal.
function worktree_rm
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
