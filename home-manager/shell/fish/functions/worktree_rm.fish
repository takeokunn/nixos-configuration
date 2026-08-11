# Removes the worktree the shell is currently inside, then cds to the bare
# repo root. Unlike worktree_new there is no picker: this only ever acts on
# $PWD's own worktree, matching worktree_new's own "operate on current
# context" precedent rather than accepting an arbitrary target path.
#
# `git rev-parse --show-toplevel` fails with no output when run from the bare
# repo root itself, since a bare repo has no working tree to report — that
# emptiness is exactly what refuses to ever target the bare repo here, so no
# separate bare-repo check is needed.
#
# Any arguments pass straight through to `git worktree remove`, so
# `worktree_rm --force` / `worktree_rm -f` overrides git's own refusal to
# remove a worktree with uncommitted changes. This function does not
# reimplement that safety check itself, the same deference to git's own
# plumbing used in __fzf_ghq_new_worktree's `worktree add --detach` call.
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
