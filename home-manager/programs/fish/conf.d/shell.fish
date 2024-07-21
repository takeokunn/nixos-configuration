if type -q eza
    alias ls 'eza'
end

if type -q bat
    set -x MANPAGER "sh -c 'col -bx | bat -l man -p'"
    alias cat 'bat'
end

if type -q dust
    alias du 'dust'
end

if type -q aws
    set -x AWS_SDK_LOAD_CONFIG 1
end

if type -q direnv
    set -gx DIRENV_LOG_FORMAT ""
    eval (direnv hook fish)
end

if type -q gh
    eval (gh completion -s fish| source)
end

if test -d /etc/profiles/per-user/(whoami)/bin
    fish_add_path /etc/profiles/per-user/(whoami)/bin
end
