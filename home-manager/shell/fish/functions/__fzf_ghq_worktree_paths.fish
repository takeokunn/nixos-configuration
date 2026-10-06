# Private helper: list a repo's linked worktree and jj workspace paths, one
# per line. Named
# __fzf_ghq_* rather than __ghq_* to avoid colliding with the vendored
# fish-ghq NUR plugin's own __ghq_* namespace.
#
# Excludes the repo's own git-common-dir entry: a bare repo's porcelain
# listing includes its own bare directory as the first "worktree", which is
# never a real linked worktree. Compares against `git rev-parse
# --git-common-dir`, not the input path verbatim -- `ghq list --full-path`
# and git's porcelain output can disagree after symlink resolution (e.g.
# /tmp vs /private/tmp), which would otherwise leak the bare repo itself.
function __fzf_ghq_worktree_paths
    set -l repo_path $argv[1]

    # LEGACY-GIT: delete once worktree_migrate_jj reports no git worktrees
    set -l repo_git_dir (git -C $repo_path rev-parse --path-format=absolute --git-common-dir 2>/dev/null)

    for line in (git -C $repo_path worktree list --porcelain)
        if string match -q 'worktree *' -- $line
            set -l wt_path (string replace -r '^worktree ' '' -- $line)
            if test "$wt_path" != "$repo_git_dir"
                echo $wt_path
            end
        end
    end

    # jj workspaces are invisible to `git worktree list`. "default" is the
    # workspace holding jj's store: the repo root itself.
    if test -d "$repo_path/.jj"
        jj -R $repo_path --ignore-working-copy workspace list -T 'if(name != "default", root ++ "\n")' 2>/dev/null
    end
end
