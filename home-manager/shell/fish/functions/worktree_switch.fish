# Lists the current repo's worktrees via fzf and cds to the pick. Never
# creates a worktree -- reports and returns if none exist.
function worktree_switch
    # Also resolves from inside a legacy git worktree below the bare root.
    set -l repo_path (jj workspace root --name default 2>/dev/null)
    if test -z "$repo_path"
        echo "worktree_switch: not inside a jj repository" >&2
        return 1
    end

    set -l worktree_paths (__fzf_ghq_worktree_paths $repo_path)

    if test (count $worktree_paths) -eq 0
        echo "worktree_switch: no worktrees found for this repository" >&2
        return 1
    end

    set -l target (printf '%s\n' $worktree_paths | fzf --preview "eza --tree --level=2 --color=always {} 2>/dev/null" --prompt "worktree> ")
    if test -z "$target"
        return
    end

    cd $target
end
