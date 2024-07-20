function ghq_users
    ghq list | grep "github.com" | cut -d / -f 2 | sort | uniq
end
