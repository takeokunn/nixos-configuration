# Private helper: succeed when $argv[1] is a secondary jj workspace, created
# by `jj workspace add`. Its .jj/repo is a file pointing at the store; the
# default workspace holding the store, such as a bare repo root, has a
# directory there instead.
function __fzf_ghq_jj_workspace_p
    test -f "$argv[1]/.jj/repo"
end
