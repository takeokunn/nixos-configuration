{ lib, ... }:
{
  nix.linux-builder = {
    enable = true;
    systems = [
      "aarch64-linux"
      "x86_64-linux"
    ];
    config = {
      boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
      virtualisation = {
        cores = 8;
        memorySize = lib.mkForce (1024 * 64);
        diskSize = lib.mkForce (1024 * 500);
      };
      nix.settings = {
        experimental-features = [
          "nix-command"
          "flakes"
        ];
      };
      security.sudo.extraRules = [
        {
          users = [ "builder" ];
          commands = [
            {
              command = "ALL";
              options = [ "NOPASSWD" ];
            }
          ];
        }
      ];
    };
  };
}
