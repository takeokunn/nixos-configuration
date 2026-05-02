function myip
    set -l ip_address (curl -s https://checkip.amazonaws.com)"/32"
    echo $ip_address | pbcopy
    echo $ip_address
end
