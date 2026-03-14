{ pkgs, nurPkgs, devenvPkgs ? null }:
with pkgs;
lib.optional (devenvPkgs != null) devenvPkgs.devenv
++ [
  dasel
  unixtools.watch

  # for git
  ghq

  # for nix
  nix-output-monitor
]
