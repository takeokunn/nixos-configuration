# Private helper: the jj counterpart of `git worktree remove` for the
# workspace at $argv[1]. Refuses when the workspace has a non-empty
# working-copy commit unless $argv[2] is "force"; the snapshot puts untracked
# files there too. Prints any failure reason to stdout, as callers capture it.
function __fzf_ghq_remove_jj_workspace
    set -l path $argv[1]
    set -l force $argv[2]

    # No --ignore-working-copy: the snapshot is what makes unsaved edits count.
    set -l is_empty (jj -R $path log --no-graph -r @ -T empty 2>&1)
    if test $status -ne 0
        echo $is_empty
        return 1
    end
    if test "$is_empty" != true -a "$force" != force
        echo "'$path' has changes in its working-copy commit; use --force to remove it anyway"
        return 1
    end

    set -l forget_output (jj -R $path workspace forget 2>&1)
    if test $status -ne 0
        echo $forget_output
        return 1
    end
    # forget leaves the directory behind. Its working-copy commit stays in
    # jj's history; only ignored files are lost with the directory.
    rm -rf -- $path
end
