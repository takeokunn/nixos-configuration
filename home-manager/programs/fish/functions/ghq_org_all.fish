function ghq_org_all
    curl "takeokunn" "https://api.github.com/orgs/"(echo $argv)"/repos" | jq -r ".[].clone_url" | xargs -L1 ghq get
end
