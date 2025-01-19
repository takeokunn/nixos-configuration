{
  pkgs,
  sources,
  org-babel,
  emacsPkg,
}:
let
  bat = import ./bat { inherit pkgs sources; };
  bottom = import ./bottom;
  direnv = import ./direnv;
  dust = import ./dust;
  editorconfig = import ./editorconfig { inherit pkgs; };
  emacs = import ./emacs { inherit pkgs emacsPkg org-babel; };
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
  jq = import ./jq;
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
  emacs
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
  jq
  sops
  tig
  tmux
  vim
  wget
]
