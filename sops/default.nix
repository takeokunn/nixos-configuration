{ username }:
{
  sops = {
    defaultSopsFile = ./password.yaml;
    defaultSopsFormat = "yaml";
    gnupg = {
      sshKeyPaths = [
        "/Users/${username}/.ssh/id_ed25519.pub"
      ];
    };
    secrets = {
      home-wifi-psk = { };
      brave-api-token = { };
    };
  };
}
