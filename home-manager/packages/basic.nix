{ pkgs }:
with pkgs; [
  # for terminal tools
  devbox
  du-dust
  lnav
  nkf
  offlineimap
  openssl
  pv
  rlwrap
  tree
  unixtools.procps
  unixtools.watch

  # for query
  csvq
  jq
  yq

  # for git
  ghq

  # for password tools
  pwgen

  # for editor
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))
  neovim

  # for shell
  fish
]
