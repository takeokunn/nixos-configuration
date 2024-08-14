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
  wget = import ./wget { inherit pkgs; };
in [
  awscli
  bat
  bottom
  direnv
  editorconfig
  emacs
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
  roswell
  tig
  tmux
  vim
  wezterm
  wget
]
