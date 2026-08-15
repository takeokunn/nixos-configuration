# Private helper: resolve a repo's default ref via a 3-step fallback chain
# (origin/HEAD, origin/main, local HEAD). Named __fzf_ghq_* rather than
# __ghq_* to avoid colliding with the vendored fish-ghq NUR plugin's own
# __ghq_* namespace.
#
# A `ghq get --bare` clone has no refs/remotes/*, so only the local-HEAD
# fallback resolves there. No fetch and no error message here -- callers own
# both.
function __fzf_ghq_resolve_default_ref
    set -l repo_path $argv[1]

    set -l default_ref (git -C $repo_path symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | string replace 'refs/remotes/' '')
    if test -z "$default_ref"
        if git -C $repo_path rev-parse --verify --quiet origin/main >/dev/null 2>&1
            set default_ref origin/main
        end
    end
    if test -z "$default_ref"
        set default_ref (git -C $repo_path symbolic-ref --short HEAD 2>/dev/null)
    end

    if test -z "$default_ref"
        return 1
    end

    echo $default_ref
end
