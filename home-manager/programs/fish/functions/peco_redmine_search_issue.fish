function peco_redmine_search_issue
    set -l base_url $argv[2]
    set -l project (
        curl -s $base_url"/projects.json?limit=100" \
            -H "X-Redmine-API-Key: "$argv[1] \
            | jq -r '.projects[] | "[" + (.id|tostring) + "]" + " " + .name + " (" + .identifier + ")"' \
            | peco
    )
    set -l project_id (string match -r '\d+' $project)

    set -l issue (
        curl -s $base_url"/issues.json?limit=100&project_id="$project_id \
            -H "X-Redmine-API-Key: "$argv[1] \
            | jq -r '.issues[] | "[" + (.id|tostring) + "]" + .subject' \
            | peco
    )
    set -l issue_id (string match -r '\d+' $issue)

    echo $base_url"/issues/"$issue_id
    echo $base_url"/issues/"$issue_id | pbcopy
    open $base_url"/issues/"$issue_id
end
