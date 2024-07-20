{ pkgs }:
let
  git = import ./git;
  bottom = import ./bottom;
  vim = import ./vim;
  peco = import ./peco { inherit pkgs; };
  wget = import ./wget { inherit pkgs; };
  tig = import ./tig { inherit pkgs; };
  editorconfig = import ./editorconfig { inherit pkgs; };
  tmux = import ./tmux { inherit pkgs; };
in [ git bottom vim peco wget tig editorconfig tmux ]
