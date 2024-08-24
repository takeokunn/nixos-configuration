{ pkgs }:
let
  ecspresso = pkgs.callPackage ../nixpkgs/ecspresso { };
  isucrud = pkgs.callPackage ../nixpkgs/isucrud { };
in with pkgs; [
  # for infra/isucon
  ecspresso
  isucrud

  # for network tools
  speedtest-cli

  # for DB
  mysql

  # for ai
  ollama

  # for misc
  discord
  drawio
  slack
]
