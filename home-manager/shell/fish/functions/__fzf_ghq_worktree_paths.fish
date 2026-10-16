# Private helper: list a repo's jj workspace paths, one per line. Named
# __fzf_ghq_* rather than __ghq_* to avoid colliding with the vendored
# fish-ghq NUR plugin's own __ghq_* namespace.
function __fzf_ghq_worktree_paths
    set -l repo_path $argv[1]

    # "default" is the workspace holding jj's store: the repo root itself.
    if test -d "$repo_path/.jj"
        jj -R $repo_path --ignore-working-copy workspace list -T 'if(name != "default", root ++ "\n")' 2>/dev/null
    end
end
