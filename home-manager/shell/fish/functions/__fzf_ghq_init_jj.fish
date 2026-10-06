# Private helper: give the bare repo $argv[1] a jj store at its root. A no-op
# when .jj already exists. Prints the failure reason to stderr and returns 1,
# leaving neither .jj nor the scratch directory behind.
#
# jj cannot initialise a workspace at the bare root directly, so it is built
# in a scratch directory inside the repo and its .jj is moved over.
function __fzf_ghq_init_jj
    set -l repo $argv[1]

    if test -z "$repo"
        echo "fzf_ghq: no repository given" >&2
        return 1
    end

    test -e "$repo/.jj"; and return 0

    if not __fzf_ghq_bare_p $repo
        echo "fzf_ghq: '$repo' is not a bare repository" >&2
        return 1
    end

    set -l tmp (mktemp -d "$repo/.jj-init.XXXXXX")
    if test $status -ne 0 -o -z "$tmp"
        echo "fzf_ghq: cannot create a scratch directory in '$repo'" >&2
        return 1
    end

    set -l reason
    # Set once mv succeeded: only then is "$repo/.jj" ours to remove.
    set -l moved false
    set -l out (jj git init --git-repo $repo $tmp 2>&1)
    or set reason "jj git init failed: $out"

    if test -z "$reason"
        set out (jj -R $tmp sparse set --clear 2>&1)
        or set reason "jj sparse set failed: $out"
    end

    # The sparse set is empty, so the root workspace tracks no files and the
    # scratch directory's own .jj-init.* name never reaches a commit.
    # mv would move into an existing .jj instead of failing, and a rollback
    # must never delete a store this call did not create.
    if test -z "$reason"
        if test -e "$repo/.jj"
            set reason "concurrent jj init in '$repo'"
        else
            set out (mv $tmp/.jj $repo/.jj 2>&1)
            and set moved true
            or set reason "cannot move .jj into '$repo': $out"
        end
    end

    # jj records an absolute git_target, which dangles once .jj has moved.
    # Relative to .jj/repo/store: store -> repo -> .jj -> bare root.
    if test -z "$reason"
        printf %s ../../.. >"$repo/.jj/repo/store/git_target"
        or set reason "cannot rewrite git_target"
    end

    if test -z "$reason"
        set out (jj -R $repo --ignore-working-copy workspace list 2>&1)
        or set reason "jj store at '$repo' is unusable: $out"
    end

    if test -n "$reason"
        rm -rf -- $tmp
        test $moved = true; and rm -rf -- "$repo/.jj"
        echo "fzf_ghq: $reason" >&2
        return 1
    end

    rm -rf -- $tmp
end
