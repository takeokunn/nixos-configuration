{ pkgs, sources }:
let
  bat = import ./bat { inherit pkgs sources; };
  bottom = import ./bottom;
  direnv = import ./direnv;
  dust = import ./dust;
  editorconfig = import ./editorconfig { inherit pkgs; };
  eza = import ./eza;
  fish = import ./fish { inherit pkgs sources; };
  gnupg = import ./gnupg;
  man = import ./man;
  misc = import ./misc;
  neovim = import ./neovim { inherit pkgs sources; };
  password-store = import ./password-store { inherit pkgs; };
  peco = import ./peco { inherit pkgs; };
  readline = import ./readline;
  ripgrep = import ./ripgrep;
  sops = import ./sops;
  tig = import ./tig { inherit pkgs sources; };
  tmux = import ./tmux { inherit pkgs; };
  vim = import ./vim { inherit pkgs; };
  wget = import ./wget { inherit pkgs; };
in
[
  bat
  bottom
  direnv
  dust
  editorconfig
  eza
  fish
  gnupg
  man
  misc
  password-store
  neovim
  peco
  readline
  ripgrep
  sops
  tig
  tmux
  vim
  wget
]
