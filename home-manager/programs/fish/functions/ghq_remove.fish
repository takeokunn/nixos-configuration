function ghq_remove
    ghq list --full-path | peco | xargs -L1 rm -fr
end
