{ pkgs }:
let
  awscli = pkgs.callPackage ./pkgs/awscli { };
  ecspresso = pkgs.callPackage ./pkgs/ecspresso { };
  isucrud = pkgs.callPackage ./pkgs/isucrud { };
  mitamae = pkgs.callPackage ./pkgs/mitamae { };
  # tmux-sixel = pkgs.callPackage ./pkgs/tmux-sixel { };
in with pkgs; [
  # for lanaguage
  nodejs
  php83

  # for language specific
  gopls
  gotools
  hadolint
  nixfmt-classic
  roswell
  shellcheck

  # for language server
  haskell-language-server
  jsonnet-language-server
  nil
  nodePackages_latest.bash-language-server
  typescript
  nodePackages_latest.typescript-language-server
  nodePackages_latest.vim-language-server
  phpactor
  rubyPackages.solargraph
  terraform-ls
  yaml-language-server

  # for gnupg
  pinentry_mac
  pinentry-emacs
  gnupg

  # for essential tools
  bat
  csvq
  eza
  fd
  fzf
  gh
  ghq
  git
  htop
  jq
  nkf
  peco
  pv
  ripgrep
  tig
  rlwrap
  tmux # or tmux-sixel
  tree
  wget
  yq

  # for essential tools
  devbox
  direnv
  du-dust
  exiftool
  extract_url
  openssl
  terminal-notifier
  tokei
  unixtools.procps
  unixtools.watch

  # for util tools
  actionlint
  android-tools
  awscli
  ffmpeg
  graphviz
  iftop
  imagemagick
  isucrud
  mitamae
  ncurses
  neofetch
  offlineimap
  speedtest-cli
  tcpdump

  # for pass
  pwgen
  (pass.withExtensions (extensions: with extensions; [ pass-otp ]))

  # for editor
  cmigemo
  emacs-git
  editorconfig-core-c
  nano
  neovim
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))

  # for shell
  fish
  zx

  # for DB
  redis
  mysql
  sqlite
  sqldef
  tbls

  # for jokes
  asciiquarium
  cmatrix
  gibo
  sl
  silicon
  genact

  # for ai
  ollama

  # for emacs
  mu
  emacsPackages.mu4e
]
