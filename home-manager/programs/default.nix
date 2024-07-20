{ pkgs }:
let
  bat = import ./bat;
  bottom = import ./bottom;
  direnv = import ./direnv;
  editorconfig = import ./editorconfig { inherit pkgs; };
  eza = import ./eza;
  fish = import ./fish { inherit pkgs; };
  git = import ./git;
  gnupg = import ./gnupg;
  man = import ./man;
  misc = import ./misc;
  password-store = import ./password-store { inherit pkgs; };
  peco = import ./peco { inherit pkgs; };
  readline = import ./readline;
  ripgrep = import ./ripgrep;
  tig = import ./tig { inherit pkgs; };
  tmux = import ./tmux { inherit pkgs; };
  vim = import ./vim;
  wget = import ./wget { inherit pkgs; };
in [
  bat
  bottom
  direnv
  editorconfig
  eza
  fish
  git
  gnupg
  man
  misc
  password-store
  peco
  readline
  ripgrep
  tig
  tmux
  vim
  wget
]
