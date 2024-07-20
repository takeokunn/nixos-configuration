function peco_growi
    set -l base_url $argv[3]
    set -l path (echo $argv[2] | sort | uniq | peco)
    set -l my_path (
        curl -s $base_url"/_api/pages.list?path=$path&access_token="$argv[1]"&limit=1000" \
            -H "Content-Type: application/json" \
            | jq -r ".pages[].path" \
            | peco
    )
    echo $base_url$my_path
    echo $base_url$my_path | pbcopy
    open $base_url$my_path
end
