{ pkgs }:
let
  # system = pkgs.system;
  # mitamae = pkgs.callPackage ./nixpkgs/mitamae { inherit system; };
  ecspresso = pkgs.callPackage ./nixpkgs/ecspresso { };
  isucrud = pkgs.callPackage ./nixpkgs/isucrud { };
in with pkgs; [
  # for cli
  tokei

  # for infra/isucon
  # mitamae
  ecspresso
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
