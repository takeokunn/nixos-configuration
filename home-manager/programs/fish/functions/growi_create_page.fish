function growi_create_page
    set -l access_token $argv[1]
    set -l base_url $argv[2]
    set -l base_path $argv[3]
    set -l slug (read -p "echo 'slug > '")
    set -l path $base_path$slug

    curl -s $base_url"/_api/v3/pages?access_token="$access_token \
        -X POST \
        -d "{\"path\": \""$path"\",\"body\": \"# "$slug"\"}" \
        -H "Content-Type: application/json"

    echo $base_url$path
    echo $base_url$path | pbcopy
    open $base_url$path
end
