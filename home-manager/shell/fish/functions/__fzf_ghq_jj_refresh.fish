# Private helper: bring the jj store of $argv[1] up to date with its remote.
# $argv[2] prefixes messages. A failed fetch is only a warning, callers continue
# with local state. A failed import returns 1: the store would not see refs git
# wrote, so the caller must not act on it.
#
# A bare clone has no persisted remote.origin.fetch, so an explicit
# --branch pattern is needed or the fetch gets nothing. Nothing here may depend
# on the user's jj config. The final import picks up refs written by git
# itself, such as a maintenance fetch.
function __fzf_ghq_jj_refresh
    set -l repo $argv[1]
    set -l prefix $argv[2]

    if jj -R $repo --ignore-working-copy git remote list 2>/dev/null | string match -rq '^origin\s'
        set -l fetch_output (jj -R $repo git fetch --remote origin --branch 'glob:*' 2>&1)
        if test $status -ne 0
            echo "$prefix: fetch of origin failed ($fetch_output); using existing local refs" >&2
        end
    end

    set -l import_output (jj -R $repo git import 2>&1)
    if test $status -ne 0
        echo "$prefix: jj git import failed ($import_output)" >&2
        return 1
    end
end
