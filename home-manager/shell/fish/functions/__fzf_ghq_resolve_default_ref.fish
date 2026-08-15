# Private helper: resolve a repo's default ref name via a fixed 3-step
# fallback chain. Not named ghq_* so it stays out of the command namespace
# and off the ghq_* glob used elsewhere to manage those functions -- and
# specifically not __ghq_* either, which collides with the vendored
# fish-ghq NUR plugin's own private namespace (e.g. __ghq_repository_search,
# pulled into the same $fish_function_path by
# home-manager/shell/fish/default.nix). __fzf_ghq_* mirrors the existing
# __fzf_ghq_new_worktree.fish precedent, which inserts the same fzf_ buffer
# for the same reason.
#
#   __fzf_ghq_resolve_default_ref <repo-path>
#
# <repo-path> is a repo's absolute path. Resolves, in order:
#
#   1. refs/remotes/origin/HEAD, with the "refs/remotes/" prefix stripped
#   2. the literal "origin/main", if it verifies (rev-parse succeeds)
#   3. the local HEAD's own symbolic-ref (the repo's checked-out branch)
#
# and prints the first one that resolves to stdout. Prints nothing and
# returns 1 if all three fail to resolve anything -- this mirrors a
# repository produced by `ghq get --bare`, which carries no refs/remotes/*
# at all and no remote.origin.fetch refspec, so neither of steps 1-2
# resolves there; only step 3 (the bare repo's own HEAD symref) does.
#
# This helper deliberately does NOT fetch anything before resolving: both
# existing callers (__fzf_ghq_new_worktree.fish and worktree_clean.fish) do
# their own fetch first, at different points in their own control flow and
# with different refspecs, and that must stay each caller's own
# responsibility. It also does NOT print any error/warning message on
# failure, since the two callers handle an empty result differently -- one
# hard-errors, the other warns and continues -- and baking a message in here
# would either duplicate or fight whatever the caller already does.
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
