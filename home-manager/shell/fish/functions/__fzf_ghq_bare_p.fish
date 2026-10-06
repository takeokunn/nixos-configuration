# Private helper: succeed when $argv[1] is a bare repository, i.e. it has no
# `.git` entry. A linked worktree has a `.git` file and a normal clone has a
# `.git` directory; -L also catches a dangling symlink.
function __fzf_ghq_bare_p
    not test -e "$argv[1]/.git"; and not test -L "$argv[1]/.git"
end
