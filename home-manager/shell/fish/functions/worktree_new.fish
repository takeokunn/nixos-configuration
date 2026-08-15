# No fzf picker, no tmux integration. With no arguments, targets the repo
# containing $PWD. $argv forwards straight through, including the optional
# naming-mode argument.
function worktree_new
    set -l target (__fzf_ghq_new_worktree $argv)
    if test $status -ne 0 -o -z "$target"
        return 1
    end

    cd $target
end
