{ pkgs }:
let
  bat = import ./bat { inherit pkgs; };
  bottom = import ./bottom;
  direnv = import ./direnv;
  editorconfig = import ./editorconfig { inherit pkgs; };
  eza = import ./eza;
  fish = import ./fish { inherit pkgs; };
  git = import ./git;
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
  wezterm = import ./wezterm;
  wget = import ./wget { inherit pkgs; };
  awscli = import ./awscli;
in [
  awscli
  bat
  bottom
  direnv
  editorconfig
  eza
  fish
  git
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
  wezterm
  wget
]
