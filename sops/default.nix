{ username }:
{
  sops = {
    defaultSopsFile = ./password.yaml;
    gnupg = {
      home = "/Users/${username}/.gnupg";
    };
    secrets = {
      home-wifi-psk = { };
      brave-api-token = { };
    };
  };
}
