# Standalone entry point around __fzf_ghq_new_worktree: no fzf repo picker and
# no tmux session integration, unlike fzf_ghq. Called with no arguments, it
# targets the repo containing $PWD via the helper's own fallback resolution.
# $argv is forwarded verbatim, including the helper's optional 3rd
# <naming-mode> argument -- callers wanting the repo's default branch name
# instead of a plain base ref should use worktree_default_branch instead of
# passing `branch` here by hand.
function worktree_new
    set -l target (__fzf_ghq_new_worktree $argv)
    if test $status -ne 0 -o -z "$target"
        return 1
    end

    cd $target
end
