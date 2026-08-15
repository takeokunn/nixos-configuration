# Lists the current repo's worktrees via fzf and cds to the pick. Never
# creates a worktree -- reports and returns if none exist.
function worktree_switch
    set -l git_common_dir (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
    if test -z "$git_common_dir"
        echo "worktree_switch: not inside a git repository" >&2
        return 1
    end
    set -l repo_path (string replace -r '/\.git$' '' -- $git_common_dir)

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
