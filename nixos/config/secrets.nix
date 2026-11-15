{ ... }:
{
  sops.defaultSopsFile = ../../secrets/x13gen2.yaml;
  # The persisted path, because sops runs before impermanence bind-mounts /etc/ssh.
  sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
  sops.gnupg.sshKeyPaths = [ ];

  sops.secrets.networkmanager-env.restartUnits = [ "NetworkManager-ensure-profiles.service" ];
  sops.secrets.take-hashed-password.neededForUsers = true;

  # sops-nix orders this unit before systemd-sysusers only; userborn reads the hash, so it must run first too.
  systemd.services.sops-install-secrets-for-users = {
    wantedBy = [ "userborn.service" ];
    before = [ "userborn.service" ];
  };
}
