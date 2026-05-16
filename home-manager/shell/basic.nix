{ pkgs, nurPkgs }:
let
  bat = import ./bat { inherit nurPkgs; };
  bottom = import ./bottom;
  direnv = import ./direnv;
  dust = import ./dust;
  eza = import ./eza;
  fd = import ./fd;
  fish = import ./fish { inherit pkgs nurPkgs; };
  fzf = import ./fzf;
  jq = import ./jq;
  man = import ./man;
  readline = import ./readline;
  ripgrep = import ./ripgrep;
  tmux = import ./tmux { inherit pkgs nurPkgs; };
  wget = import ./wget { };
  zoxide = import ./zoxide;
in
[
  ./modules/wget
  ./modules/dust
  ./modules/zellij
]
++ [
  bat
  bottom
  direnv
  dust
  eza
  fd
  fish
  fzf
  jq
  man
  readline
  ripgrep
  tmux
  wget
  zoxide
  { home.packages = with pkgs; [ dasel unixtools.watch nix-output-monitor ]; }
]
