function peco_redmine_my_issue
    set -l base_url $argv[2]
    set -l issue (
        curl -s $base_url"/issues.json?limit=100&assigned_to_id=me" \
            -H "X-Redmine-API-Key: "$argv[1] \
            | jq -r '.issues[] | "[" + (.id|tostring) + "]" + .subject' \
            | peco
    )
    set -l issue_id (string match -r '\d+' $issue)

    echo $base_url"/issues/"$issue_id
    echo $base_url"/issues/"$issue_id | pbcopy
    open $base_url"/issues/"$issue_id
end
