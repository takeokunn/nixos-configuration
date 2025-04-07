{ username }:
{
  sops = {
    defaultSopsFile = ./password.yaml;
    age.sshKeyPaths = [
      "/Users/${username}/.ssh/id_ed25519"
    ];
    secrets = {
      home-wifi-psk = { };
      brave-api-token = { };
    };
  };
}
