{ pkgs }:
let
  awscli = pkgs.callPackage ./pkgs/awscli { };
  ecspresso = pkgs.callPackage ./pkgs/ecspresso { };
  isucrud = pkgs.callPackage ./pkgs/isucrud { };
  mitamae = pkgs.callPackage ./pkgs/mitamae { };
  tmux-sixel = pkgs.callPackage ./pkgs/tmux-sixel { };
in with pkgs; [
  # for lanaguage
  nodejs
  php83
  typescript

  # for language specific
  gopls
  gotools
  hadolint
  roswell
  shellcheck

  # for nix
  nixfmt-classic
  nixpkgs-fmt
  niv
  nix-prefetch
  nix-prefetch-git
  nix-prefetch-github

  # for language server
  haskell-language-server
  jsonnet-language-server
  nil
  nodePackages_latest.bash-language-server
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
  gitflow
  htop
  jq
  nkf
  peco
  pv
  ripgrep
  rlwrap
  tmux-sixel
  tree
  wget
  yq

  # for basic tools
  act
  actionlint
  android-tools
  cacert
  cmigemo
  devbox
  direnv
  du-dust
  exiftool
  extract_url
  ffmpeg
  graphviz
  iftop
  imagemagick
  ncurses
  neofetch
  offlineimap
  openssl
  mitamae
  isucrud
  pwgen
  silicon
  sqldef
  terminal-notifier
  tig
  tokei
  unixtools.procps
  unixtools.watch

  # for pass
  (pass.withExtensions (extensions: with extensions; [ pass-otp ]))

  # for cloud
  awscli
  ssm-session-manager-plugin

  # for network
  speedtest-cli
  tcpdump

  # for editor
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
  tbls

  # for jokes
  asciiquarium
  cmatrix
  gibo
  sl
  genact

  # for ai
  ollama

  # for emacs
  mu
  emacsPackages.mu4e
]
