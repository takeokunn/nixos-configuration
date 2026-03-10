{ pkgs, nurPkgs }:
with pkgs;
[
  # for terminal tools
  nurPkgs.devenv
  dasel
  unixtools.watch

  # for git
  ghq

  # for nix
  nix-output-monitor
]
