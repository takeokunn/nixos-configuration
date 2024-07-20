{ pkgs }:
with pkgs; [
  # for terminal tools
  bat
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
  pv
  ripgrep
  rlwrap
  tmux
  tree
  unixtools.procps
  unixtools.watch

  # for query
  csvq
  jq
  yq

  # for git
  ghq
  tig

  # for password tools
  pwgen
  (pass.withExtensions (extensions: with extensions; [ pass-otp ]))

  # for editor
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))
  neovim

  # for shell
  fish
]
