{
  pkgs,
  nurPkgs,
}:
let
  bat = import ./bat { inherit nurPkgs; };
  bottom = import ./bottom;
  cargo = import ./cargo { inherit pkgs; };
  direnv = import ./direnv;
  dust = import ./dust;
  editorconfig = import ./editorconfig { inherit pkgs; };
  eza = import ./eza;
  fd = import ./fd;
  fish = import ./fish { inherit pkgs nurPkgs; };
  gnupg = import ./gnupg;
  man = import ./man;
  nixvim = import ./nixvim { inherit pkgs nurPkgs; };
  password-store = import ./password-store { inherit pkgs; };
  fzf = import ./fzf;
  readline = import ./readline;
  ripgrep = import ./ripgrep;
  jq = import ./jq;
  tig = import ./tig { inherit nurPkgs; };
  tmux = import ./tmux { inherit pkgs; };
  vim = import ./vim { inherit pkgs; };
  wget = import ./wget { };
  zoxide = import ./zoxide;
in
[
  bat
  bottom
  cargo
  direnv
  dust
  editorconfig
  eza
  fd
  fish
  gnupg
  man
  password-store
  nixvim
  fzf
  readline
  ripgrep
  jq
  tig
  tmux
  vim
  wget
  zoxide
]
