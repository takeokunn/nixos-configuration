# Private helper: resolve a repo's default ref to a jj revset via a fallback
# chain (main@origin, master@origin, trunk@origin, then the bare repo's HEAD
# branch). Named __fzf_ghq_* rather than __ghq_* to avoid colliding with the
# vendored fish-ghq NUR plugin's own __ghq_* namespace.
#
# A `ghq get --bare` clone has no remote bookmarks until something fetches, so
# only the HEAD fallback resolves there. No fetch and no error message here;
# callers own both.
function __fzf_ghq_resolve_default_ref
    set -l repo_path $argv[1]

    for candidate in main@origin master@origin trunk@origin
        set -l found (jj -R $repo_path --ignore-working-copy log --no-graph -r "present($candidate)" -T commit_id 2>/dev/null)
        if test -n "$found"
            echo $candidate
            return 0
        end
    end

    set -l head_ref (string replace -rf '^ref: refs/heads/(.+)$' '$1' <"$repo_path/HEAD" 2>/dev/null)
    if test -n "$head_ref"
        set -l found (jj -R $repo_path --ignore-working-copy log --no-graph -r "present($head_ref)" -T commit_id 2>/dev/null)
        if test -n "$found"
            echo $head_ref
            return 0
        end
    end

    return 1
end
