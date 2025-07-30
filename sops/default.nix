# { homeDirectory }:
# {
#   sops = {
#     defaultSopsFile = ./password.yaml;
#     gnupg = {
#       home = "${homeDirectory}/.gnupg";
#     };
#     secrets = {
#       home-wifi-psk = { };
#       brave-api-token = { };
#     };
#   };
# }
