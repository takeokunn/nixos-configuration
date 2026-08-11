# Sibling of fzf_ghq: reuses its worktree-enumeration and fzf-picker logic
# (Stage 2 of fzf_ghq.fish) but scoped to only the repo containing $PWD, with
# no cross-repo `ghq list` stage and no tmux session handling -- this is a
# plain `cd`. Unlike worktree_new, this never creates a worktree: with zero
# worktrees found it reports and returns rather than falling back to create.
function worktree_switch
    set -l git_common_dir (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
    if test -z "$git_common_dir"
        echo "worktree_switch: not inside a git repository" >&2
        return 1
    end
    set -l repo_path (string replace -r '/\.git$' '' -- $git_common_dir)

    # Exclude the bare repo's own entry by comparing against git's own
    # canonical path for it, not $repo_path verbatim -- mirrors the same
    # symlink-resolution caveat documented in fzf_ghq.fish.
    set -l repo_git_dir (git -C $repo_path rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
    set -l worktree_paths
    for line in (git -C $repo_path worktree list --porcelain)
        if string match -q 'worktree *' -- $line
            set -l wt_path (string replace -r '^worktree ' '' -- $line)
            if test "$wt_path" != "$repo_git_dir"
                set -a worktree_paths $wt_path
            end
        end
    end

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
