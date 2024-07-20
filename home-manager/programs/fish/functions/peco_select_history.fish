function peco_select_history
    set -l query (commandline)

    if test -n $query
        set peco_flags --query "$query"
    end

    history | peco $peco_flags | read recent
    if [ $recent ]
        commandline $recent
    end
end
