# Always resolves the base ref to the repo's own default branch, named via
# the "branch" naming mode. Takes no arguments.
function worktree_default_branch
    set -l target (__fzf_ghq_new_worktree "" "" branch)
    if test $status -ne 0 -o -z "$target"
        return 1
    end

    cd $target
end
