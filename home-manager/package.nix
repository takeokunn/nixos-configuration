{ pkgs }:
let
  ecspresso = pkgs.callPackage ./pkgs/ecspresso { };
  isucrud = pkgs.callPackage ./pkgs/isucrud { };
  mitamae = pkgs.callPackage ./pkgs/mitamae { };
  tmux-sixel = pkgs.callPackage ./pkgs/tmux-sixel { };
in with pkgs; [
  # for lanaguage
  clojure
  deno
  erlang
  fsharp
  gauche
  ghc
  go
  go-jsonnet
  guile
  lua
  nodejs_21
  perl
  php83
  php83Extensions.redis
  php83Extensions.imagick
  php83Packages.composer
  ruby
  roswell
  terraform
  vlang
  zig

  # for language specific
  gopls
  gotools
  hadolint
  rustup
  rye
  shellcheck
  terraform-ls
  tflint
  tfsec
  tfupdate
  typescript
  yarn

  # for nix
  nixfmt-classic
  nixpkgs-fmt
  niv
  nix-prefetch
  nix-prefetch-git
  nix-prefetch-github

  # for language server
  phpactor
  haskell-language-server
  jsonnet-language-server
  nil
  nodePackages_latest.bash-language-server
  # nodePackages_latest.intelephense
  nodePackages_latest.typescript-language-server
  nodePackages_latest.vim-language-server
  rubyPackages.solargraph
  yaml-language-server

  # for gnupg
  pinentry_mac
  pinentry-emacs
  gnupg

  # for build tools
  autoconf
  automake
  binutils
  bison
  boost
  cmake
  coreutils
  foreman
  glib
  gnutls
  icu
  libcxx
  libcxxrt
  libgccjit
  libiconv
  libllvm
  libmng
  libpng
  librsvg
  libsixel
  libxml2
  libzip
  meson
  pkg-config
  sqldef
  stunnel
  texinfo

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
  SDL2
  act
  actionlint
  android-tools
  cacert
  cmigemo
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
  ecspresso
  ssm-session-manager-plugin

  # for network
  bettercap
  gping
  speedtest-cli
  tcpdump

  # for editor
  emacs-git
  editorconfig-core-c
  micro
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

  # for application
  discord
  raycast
  slack
]
