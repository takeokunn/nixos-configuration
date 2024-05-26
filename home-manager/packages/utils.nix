{ pkgs }:
let
  ecspresso = pkgs.callPackage ../nixpkgs/ecspresso { };
  isucrud = pkgs.callPackage ../nixpkgs/isucrud { };
  mitamae = pkgs.callPackage ../pkgs/mitamae { };
in with pkgs; [
  # for util tools
  awscli
  ecspresso
  ffmpeg
  gibo
  graphviz
  iftop
  imagemagick
  isucrud
  mitamae
  ncurses
  neofetch
  offlineimap
  speedtest-cli
  tcpdump

  # for editor
  cmigemo
  editorconfig-core-c
  nano
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))

  # for shell
  zx

  # for DB
  redis
  mysql
  sqlite
  sqldef

  # for jokes
  asciiquarium
  cmatrix
  sl
  silicon
  genact

  # for ai
  ollama
]
