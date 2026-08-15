function worktree_new
    set -l target (__fzf_ghq_new_worktree $argv)
    if test $status -ne 0 -o -z "$target"
        return 1
    end

    cd $target
end
