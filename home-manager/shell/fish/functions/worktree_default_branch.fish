function worktree_default_branch
    set -l target (__fzf_ghq_new_worktree "" "" branch)
    if test $status -ne 0 -o -z "$target"
        return 1
    end

    cd $target
end
