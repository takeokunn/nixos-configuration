{ pkgs, ... }:
{
  # Match the guest's Nix version (nixpkgs-linux, also unstable) so the
  # ssh-ng distributed-build protocol between this host's nix-daemon and
  # the darwin-vz-nix builder doesn't hit a client/server mismatch.
  # nix-darwin's own default nix.package lags noticeably behind nixpkgs
  # (2.30.2 vs. this flake's pinned 2.34.7), and that gap made every
  # remote build fail immediately with "interrupted by the user" on the
  # guest's nix-daemon.
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
    extra-substituters = [
      "https://devenv.cachix.org"
      "https://nix-community.cachix.org"
      "https://takeokunn-darwin-vz-nix.cachix.org"
      "https://numtide.cachix.org"
    ];
    extra-trusted-public-keys = [
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "takeokunn-darwin-vz-nix.cachix.org-1:/JRjcn9UMUbE0DRyJUg7g+gq/e7QSUXxvz+FZprHIH4="
      "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
    ];
  };
}
