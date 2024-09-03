{
  sops = {
    defaultSopsFile = ../../secrets/network.yaml;
    age.sshKeyPaths = [ "/home/take/.ssh/id_ed25519" ];
    secrets."home-wifi" = { };
  };
}
