{ pkgs }:
let
  tmux-sixel = pkgs.callPackage ../nixpkgs/tmux-sixel { };
  ecspresso = pkgs.callPackage ../nixpkgs/ecspresso { };
  isucrud = pkgs.callPackage ../nixpkgs/isucrud { };
  mitamae = pkgs.callPackage ../nixpkgs/mitamae { };
in with pkgs; [
  # for language
  php83
  nodejs

  # for language specific
  gopls
  gotools
  hadolint
  nixfmt-classic
  roswell
  shellcheck

  # for language server
  ccls
  haskell-language-server
  jsonnet-language-server
  nil
  nodePackages_latest.bash-language-server
  typescript
  nodePackages_latest.typescript-language-server
  nodePackages_latest.vim-language-server
  phpactor
  nodePackages_latest.intelephense
  rubyPackages.solargraph
  terraform-ls
  yaml-language-server

  # for gnupg
  pinentry-emacs
  gnupg

  # for essential tools
  bat
  bottom
  csvq
  devbox
  direnv
  du-dust
  exiftool
  extract_url
  eza
  fd
  fzf
  gh
  ghq
  git
  gitflow
  jq
  nkf
  openssl
  peco
  pv
  ripgrep
  rlwrap
  tig
  tmux-sixel
  tokei
  tree
  unixtools.procps
  unixtools.watch
  wget
  yq

  # for util tools
  awscli
  ecspresso
  ffmpeg
  gibo
  graphviz
  hub
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
  editorconfig-core-c
  nano
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))
  neovim

  # for shell
  fish
  zx

  # for DB
  redis
  mysql
  sqlite
  sqldef

  # for jokes
  asciiquarium
  cmatrix
  sl
  silicon
  genact

  # for ai
  ollama
]
