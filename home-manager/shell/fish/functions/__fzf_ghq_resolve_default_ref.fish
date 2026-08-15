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
