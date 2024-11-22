{ pkgs }:
with pkgs;
[
  # for terminal tools
  devenv
  unixtools.watch

  # for git
  ghq

  # for nix
  nix-output-monitor
]
