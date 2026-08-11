# Standalone entry point around __fzf_ghq_new_worktree: no fzf repo picker and
# no tmux session integration, unlike fzf_ghq. Called with no arguments, it
# targets the repo containing $PWD via the helper's own fallback resolution.
function worktree_new
    set -l target (__fzf_ghq_new_worktree $argv)
    if test $status -ne 0 -o -z "$target"
        return 1
    end

    cd $target
end
