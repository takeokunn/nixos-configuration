{ pkgs }:
let
  bat = import ./bat { inherit pkgs; };
  bottom = import ./bottom;
  direnv = import ./direnv;
  editorconfig = import ./editorconfig { inherit pkgs; };
  eza = import ./eza;
  fish = import ./fish { inherit pkgs; };
  gnupg = import ./gnupg;
  misc = import ./misc;
  neovim = import ./neovim;
  password-store = import ./password-store { inherit pkgs; };
  peco = import ./peco { inherit pkgs; };
  readline = import ./readline;
  ripgrep = import ./ripgrep;
  tig = import ./tig { inherit pkgs; };
  tmux = import ./tmux { inherit pkgs; };
  vim = import ./vim;
  wget = import ./wget { inherit pkgs; };
  sway = import ./sway { inherit pkgs; };
in [
  bat
  bottom
  direnv
  editorconfig
  eza
  fish
  gnupg
  misc
  neovim
  password-store
  peco
  readline
  ripgrep
  tig
  tmux
  vim
  wget
  sway
]
