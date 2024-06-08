{ pkgs }:
with pkgs; [
  # for essential tools
  bat
  devbox
  direnv
  du-dust
  extract_url
  eza
  fd
  gnupg
  nkf
  openssl
  peco
  pv
  ripgrep
  rlwrap
  tmux
  tokei
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

  # for util tools
  awscli
  exiftool
  ffmpeg
  offlineimap
  silicon

  # for network tools
  bottom
  speedtest-cli
  tcpdump

  # for password tools
  pwgen
  (pass.withExtensions (extensions: with extensions; [ pass-otp ]))

  # for editor
  cmigemo
  editorconfig-core-c
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))
  neovim

  # for shell
  fish

  # for DB
  mysql

  # for jokes
  asciiquarium
  cmatrix
  fastfetch
  genact
  sl

  # for ai
  ollama
]
