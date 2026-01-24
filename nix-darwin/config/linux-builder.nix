{ lib, ... }:
{
  nix.linux-builder = {
    enable = true;
    ephemeral = true;
    systems = [
      "aarch64-linux"
      "x86_64-linux"
    ];
    config = {
      virtualisation = {
        cores = 8;
        memorySize = lib.mkForce (1024 * 64);
        diskSize = lib.mkForce (1024 * 500);
      };
      security.sudo.wheelNeedsPassword = false;
      users.users.builder.extraGroups = [ "wheel" ];
      nix.settings = {
        experimental-features = [
          "nix-command"
          "flakes"
        ];
      };
    };
  };
}
