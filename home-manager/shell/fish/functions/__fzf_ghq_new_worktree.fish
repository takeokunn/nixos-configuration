# Private helper: create a new worktree under a bare repo's .worktrees/.
# Not named ghq_* so it stays out of the command namespace and off the
# ghq_* glob used elsewhere to manage those functions.
#
#   __fzf_ghq_new_worktree [<repo-path>] [<base-ref>] [<naming-mode>]
#
# <naming-mode>, when it is exactly the literal string `branch`, names the
# created directory <timestamp>-<sanitized-default-branch> (leading
# `origin/` stripped, remaining `/` replaced with `-`) instead of the
# default <timestamp>-<short-sha>. Any other value, including empty/absent,
# keeps the default sha-based naming.
#
# Prints the created absolute path to stdout as the ONLY stdout output, so
# callers — fzf_ghq (fzf_ghq.fish) and the public wrapper worktree_new
# (worktree_new.fish) — can consume it directly via command substitution;
# all human-facing messages go to stderr.
function __fzf_ghq_new_worktree
    set -l repo_path $argv[1]
    set -l base_ref $argv[2]
    set -l naming_mode $argv[3]

    if test -z "$repo_path"
        set -l git_common_dir (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
        if test -z "$git_common_dir"
            echo "fzf_ghq: not inside a git repository" >&2
            return 1
        end
        set repo_path (string replace -r '/\.git$' '' -- $git_common_dir)
    end

    if not git -C $repo_path rev-parse --git-dir >/dev/null 2>&1
        echo "fzf_ghq: '$repo_path' is not a git repository" >&2
        return 1
    end

    # Serena's activate_project tool permanently and unboundedly appends the
    # activated project's absolute path to ~/.serena/serena_config.yml's
    # `projects:` list, and no git hook fires on `git worktree remove` to undo
    # that -- so every worktree this function has ever created stays
    # registered forever, even after its directory is gone. There is no
    # deletion-time hook to attach a fix to, so prune on the next worktree
    # creation instead: garbage-collect dead entries here, before `git
    # worktree add` below, so this runs regardless of whether that add
    # succeeds. The guard mirrors the one in
    # home-manager/ai-tools/serena/default.nix so a machine that has never
    # run Serena does not fail here.
    set -l serena_config "$HOME/.serena/serena_config.yml"
    if test -f "$serena_config"
        # A failed read (transient I/O error, malformed YAML, a schema change)
        # must not be treated as "zero projects": that would fall through to
        # the empty-list branch below and silently wipe the real list. Capture
        # the read's own exit status before anything else can overwrite
        # $status, and skip the prune entirely -- no write at all -- unless
        # the read cleanly succeeded.
        set -l yq_read_output (yq-go eval '.projects[]' "$serena_config" 2>/dev/null)
        set -l yq_read_status $status

        if test $yq_read_status -ne 0
            echo "fzf_ghq: could not read '$serena_config' (yq-go exited $yq_read_status); skipping Serena project prune" >&2
        else
            set -l kept_projects
            for project in $yq_read_output
                test -d "$project"; and set -a kept_projects $project
            end

            # Rebuild the whole `projects:` list in one write rather than
            # deleting entries one at a time, and pass it as a single yq
            # expression string (the same pattern default.nix uses for
            # `ignored_paths`) rather than interpolating each path as its own
            # shell argument. Escape backslashes before quotes so an escaped
            # quote's own backslash is not re-escaped.
            if test (count $kept_projects) -eq 0
                yq-go eval -i '.projects = []' "$serena_config"
            else
                set -l quoted_projects
                for project in $kept_projects
                    set -a quoted_projects (string replace -a '"' '\\"' -- (string replace -a '\\' '\\\\' -- $project))
                end
                set -l joined_projects (string join '", "' -- $quoted_projects)
                set -l prune_expr ".projects = [\"$joined_projects\"]"
                yq-go eval -i "$prune_expr" "$serena_config"
            end
        end
    end

    # A repository produced by `ghq get --bare` carries no refs/remotes/* at
    # all and no remote.origin.fetch refspec, so neither refs/remotes/origin/HEAD
    # nor origin/main resolves there — the only ref present is refs/heads/main,
    # which the repository's own HEAD symref points at. The first two candidates
    # only become available once something has written the refspec and fetched.
    if test -z "$base_ref"
        # An explicit refspec on this one call, rather than a persisted
        # remote.origin.fetch config, keeps a `ghq get --bare` clone from
        # permanently gaining refs/remotes/* it never asked for. A failed
        # fetch is not fatal here -- the three-step chain below still has
        # local refs to fall back to.
        if git -C $repo_path remote get-url origin >/dev/null 2>&1
            set -l fetch_output (git -C $repo_path fetch --prune origin '+refs/heads/*:refs/remotes/origin/*' 2>&1)
            if test $status -ne 0
                echo "fzf_ghq: fetch of origin/main failed ($fetch_output); falling back to local refs" >&2
            end
        end
    end
    if test -z "$base_ref"
        set base_ref (__fzf_ghq_resolve_default_ref $repo_path)
    end
    if test -z "$base_ref"
        echo "fzf_ghq: cannot resolve a base ref in '$repo_path'" >&2
        return 1
    end

    set -l base_sha (git -C $repo_path rev-parse --short $base_ref 2>/dev/null)
    if test -z "$base_sha"
        echo "fzf_ghq: cannot resolve base ref '$base_ref' in '$repo_path'" >&2
        return 1
    end

    set -l timestamp (date +%Y%m%dT%H%M%S)
    set -l worktrees_dir "$repo_path/.worktrees"
    set -l base_name
    if test "$naming_mode" = branch
        set -l sanitized_branch_name (string replace -a '/' '-' -- (string replace -r '^origin/' '' -- $base_ref))
        set base_name "$timestamp-$sanitized_branch_name"
    else
        set base_name "$timestamp-$base_sha"
    end
    set -l target_name $base_name
    set -l suffix 2
    while test -e "$worktrees_dir/$target_name"
        set target_name "$base_name-$suffix"
        set suffix (math $suffix + 1)
    end
    set -l target_path "$worktrees_dir/$target_name"

    # Detach at the resolved commit rather than checking out the branch name.
    # With the HEAD fallback above, base_ref is frequently the bare repo's only
    # branch, and `git worktree add <path> <branch>` fails outright once any
    # sibling worktree already holds it. The directory name records the commit
    # anyway, so the branch name carries nothing extra here.
    set -l worktree_add_output (git -C $repo_path worktree add --detach $target_path $base_sha 2>&1)
    if test $status -ne 0
        echo "fzf_ghq: failed to create worktree: $worktree_add_output" >&2
        return 1
    end

    # A fresh worktree carries no accumulated project knowledge. Serena memories
    # and .claude settings describe the repository rather than any one checkout,
    # so point them at a single copy held beside the bare repo. Linking rather
    # than copying means a memory written in one worktree is visible from the
    # others, and that removing a worktree does not take it away. Serena's cache
    # is deliberately left unlinked: it indexes paths, so sharing it across
    # concurrent worktrees would have them overwrite each other's index.
    #
    # Link only where the checkout does not already provide the path. A
    # repository that tracks .claude has it checked out as a real directory, and
    # `ln -sfn <src> <existing-dir>` silently creates the link *inside* it —
    # yielding .claude/.claude rather than failing. Where the path is tracked the
    # checkout is authoritative, so the shared copy must not shadow it.
    set -l state_dir "$repo_path/.state"

    # The loop below only links a $pair it finds under $state_dir (`test -d
    # "$src"; or continue`), so on a machine where .state/ has never been
    # populated every pair would silently skip instead of linking. Create
    # both targets up front so the first worktree ever created for a repo
    # bootstraps them automatically. mkdir -p is idempotent, so this is a
    # no-op once .state/ is already populated, and it only ever touches
    # $state_dir -- never $target_path -- so it cannot trip the
    # checkout-shadowing guard in the loop below.
    mkdir -p "$state_dir/.serena/memories" "$state_dir/.claude"

    for pair in ".serena/memories" ".claude"
        set -l src "$state_dir/$pair"
        set -l dst "$target_path/$pair"
        test -d "$src"; or continue
        if test -e "$dst"; or test -L "$dst"
            echo "fzf_ghq: $pair is provided by the checkout; leaving it unlinked" >&2
            continue
        end
        mkdir -p (dirname "$dst")
        ln -sfn "$src" "$dst"
    end

    # .envrc and .env are copied rather than symlinked: direnv and dotenv
    # tooling expect a real file, and a per-worktree copy lets one worktree
    # edit its own environment without affecting siblings. Same
    # checkout-shadowing guard as the symlink loop above -- a repository that
    # tracks its own .envrc/.env must not have it overwritten by the shared
    # copy.
    for name in .envrc .env
        set -l src "$state_dir/$name"
        set -l dst "$target_path/$name"
        if test -e "$dst"; or test -L "$dst"
            echo "fzf_ghq: $name is provided by the checkout; leaving it un-copied" >&2
            continue
        end
        test -f "$src"; or continue
        cp "$src" "$dst"
        if test "$name" = .envrc
            if command -v direnv >/dev/null 2>&1
                set -l direnv_allow_output (direnv allow "$target_path" 2>&1)
                if test $status -ne 0
                    echo "fzf_ghq: direnv allow failed ($direnv_allow_output)" >&2
                end
            end
        end
    end

    echo "fzf_ghq: created worktree $target_path" >&2
    echo $target_path
end
