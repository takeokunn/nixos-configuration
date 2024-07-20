function calculate_oidc_thumbprint --description  "e.g. `calculate_oidc_thumbprint token.actions.githubusercontent.com`"
    set -u url "https:"//$argv[1]"/.well-known/openid-configuration"
    set -u jwks_uri (curl -s $url | jq -r '.jwks_uri | split("/")[2]')

    set -u tmpfile (mktemp)
    echo | openssl s_client -servername $jwks_uri -showcerts -connect $jwks_uri:443 2> /dev/null | sed -n -e '/BEGIN/h' -e '/BEGIN/,/END/H' -e '$x' -e '$p' | tail +2 > $tmpfile

    set -u thumbprint (openssl x509 -in $tmpfile -fingerprint -noout | sed -e "s/.*=//" -e "s/://g" | tr "ABCDEF" "abcdef")
    echo $thumbprint

    rm $tmpfile
end
