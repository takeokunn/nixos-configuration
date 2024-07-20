function skylark
    set -l brand_id
    switch $argv
        case "gusto"
            set brand_id 1
        case "bamiyan"
            set brand_id 2
        case "jonathan"
            set brand_id 3
        case "yumean"
            set brand_id 4
        case "steak_gusto"
            set brand_id 5
        case "grazie_gardens"
            set brand_id 6
        case "aiya"
            set brand_id 7
        case "karayoshi"
            set brand_id 13
        case "tonkara_tei"
            set brand_id 14
        case '*'
            echo "invalid argument: please input [gusto, bamiyan, jonathan yumean steak_gusto grazie_gardens aiya karayoshi tonkara_tei]"
            return
    end

    curl -s "https://app-data.skylark-app.net/coupon/normals.json" \
        | jq ".[]" \
        | jq "{ brand_id: .brand_id, name: .name, before_price: .before_price, after_price: .after_price, display_number: .display_number }" \
        | jq "select(.brand_id == \"$brand_id\")" \
        | jq "\"[\" + .display_number + \"] \" + .name + \": \" + .before_price + \" → \" + .after_price" \
        | uniq \
        | tr -d '"'
end
