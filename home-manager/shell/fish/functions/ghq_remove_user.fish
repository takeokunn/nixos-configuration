function ghq_remove_user
    ghq list | grep "github.com" | cut -d / -f 2 | sort | uniq | peco | read recent
    ghq list --full-path | grep -E "/"(echo $recent)"/" | xargs -L1 rm -fr
end
