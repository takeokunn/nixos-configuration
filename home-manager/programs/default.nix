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
  direnv = import ./direnv;
  eza = import ./eza;
  password-store = import ./password-store { inherit pkgs; };
  bat = import ./bat;
  ripgrep = import ./ripgrep;
  readline = import ./readline;
  gnupg = import ./gnupg;
in [
  git
  bottom
  vim
  peco
  wget
  tig
  editorconfig
  tmux
  direnv
  eza
  password-store
  bat
  ripgrep
  readline
  gnupg
]
