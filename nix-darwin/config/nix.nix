{ pkgs, ... }:
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
      "take"
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
    extra-substituters = [
      "https://devenv.cachix.org"
      "https://nix-community.cachix.org"
      "https://takeokunn-darwin-vz-nix.cachix.org"
      "https://takeokunn-nixos-configuration.cachix.org"
      "https://cache.numtide.com"
      "https://cclens.cachix.org"
    ];
    extra-trusted-public-keys = [
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "takeokunn-darwin-vz-nix.cachix.org-1:eqv/henpzh2D5PjoV9YNCGGQJ4jUHW4oeFoqcooQX3g="
      "takeokunn-nixos-configuration.cachix.org-1:TzNrelquzPLqhPJnUFUp/RC/eHrtEegOjVvGEw+jsdc="
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
      "cclens.cachix.org-1:0QUNU6PuVyf+yXOvg3n1rd3FksBoB3s3/Jty50iKRNQ="
    ];
  };
}
