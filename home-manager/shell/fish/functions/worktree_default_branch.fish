# Sibling of worktree_new: always resolves the base ref to the repo's own
# default branch and names the resulting directory
# <timestamp>-<default-branch> instead of <timestamp>-<short-sha>, via the
# helper's "branch" naming mode. Takes no arguments at all -- unlike
# worktree_new, which passes $argv through for an optional base ref -- since
# "the repo's default branch" is the only base ref this ever means.
function worktree_default_branch
    set -l target (__fzf_ghq_new_worktree "" "" branch)
    if test $status -ne 0 -o -z "$target"
        return 1
    end

    cd $target
end
