{ pkgs }:
let
  ecspresso = pkgs.callPackage ./nixpkgs/ecspresso { };
  isucrud = pkgs.callPackage ./nixpkgs/isucrud { };
in with pkgs; [
  # for infra
  ecspresso

  # for isucrud
  isucrud

  # for network tools
  speedtest-cli

  # for DB
  mysql

  # for ai
  ollama

  # for web service
  discord
  drawio
  slack
]
