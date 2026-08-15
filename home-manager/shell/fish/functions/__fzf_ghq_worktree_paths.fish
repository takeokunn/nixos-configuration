function __fzf_ghq_worktree_paths
    set -l repo_path $argv[1]

    set -l repo_git_dir (git -C $repo_path rev-parse --path-format=absolute --git-common-dir 2>/dev/null)

    for line in (git -C $repo_path worktree list --porcelain)
        if string match -q 'worktree *' -- $line
            set -l wt_path (string replace -r '^worktree ' '' -- $line)
            if test "$wt_path" != "$repo_git_dir"
                echo $wt_path
            end
        end
    end
end
