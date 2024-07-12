{ pkgs }:
with pkgs; [
  # for terminal tools
  bat
  bottom
  devbox
  direnv
  du-dust
  extract_url
  eza
  fd
  gnupg
  lnav
  nkf
  offlineimap
  openssl
  peco
  pv
  ripgrep
  rlwrap
  tmux
  tree
  unixtools.procps
  unixtools.watch
  wget

  # for query
  csvq
  jq
  yq

  # for git
  ghq
  git
  tig

  # for password tools
  pwgen
  (pass.withExtensions (extensions: with extensions; [ pass-otp ]))

  # for editor
  editorconfig-core-c
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))
  neovim

  # for shell
  fish
]
