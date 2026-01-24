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
      nix.settings = {
        experimental-features = [
          "nix-command"
          "flakes"
        ];
      };
    };
  };
}
