{ lib, ... }:
{
  nix.linux-builder = {
    enable = true;
    ephemeral = false;
    systems = [ "aarch64-linux" ];
    config = {
      virtualisation = {
        cores = 6;
        memorySize = lib.mkForce (1024 * 16);
        diskSize = lib.mkForce (1024 * 200);
      };
    };
  };
}
