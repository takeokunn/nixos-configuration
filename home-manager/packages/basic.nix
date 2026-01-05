{ pkgs }:
with pkgs;
[
  # for terminal tools
  devenv
  dasel
  unixtools.watch

  # for git
  ghq

  # for nix
  nix-output-monitor
]
