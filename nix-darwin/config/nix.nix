{ pkgs, username, ... }:
let
  caches = import ../../shared/nix-caches.nix;
in
{
  # Pin the host nix-daemon to this flake's nixpkgs nix, matching the
  # darwin-vz-nix guest's Nix version for a consistent distributed-build protocol.
  nix.package = pkgs.nix;

  nix.optimise.automatic = true;
  nix.optimise.interval = [
    {
      Hour = 3;
      Minute = 0;
    }
  ];

  nix.gc.automatic = true;
  nix.gc.interval = [
    {
      Hour = 3;
      Minute = 30;
    }
  ];
  nix.gc.options = "--delete-older-than 7d";

  nix.settings = {
    trusted-users = [
      "root"
      username
    ];
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    cores = 8;
    max-jobs = 4;
    http-connections = 50;
    download-buffer-size = 268435456;
    always-allow-substitutes = true;
    warn-dirty = false;
    extra-substituters = caches.substituters ++ [ "https://takeokunn-darwin-vz-nix.cachix.org" ];
    extra-trusted-public-keys = caches.trusted-public-keys ++ [
      "takeokunn-darwin-vz-nix.cachix.org-1:eqv/henpzh2D5PjoV9YNCGGQJ4jUHW4oeFoqcooQX3g="
    ];
  };
}
