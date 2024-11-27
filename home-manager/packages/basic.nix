{ pkgs }:
with pkgs;
[
  # for terminal tools
  devenv
  unixtools.watch

  # for git
  gh
  ghq

  # for nix
  nix-output-monitor
]
