{
  sops = {
    defaultSopsFile = ../../secrets/password.yaml;
    age.sshKeyPaths = [ "/home/take/.ssh/id_ed25519" ];

    secrets.home-wifi-psk = { };
  };
}
