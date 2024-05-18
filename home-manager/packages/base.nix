{ pkgs }:
let
  # ecspresso = pkgs.callPackage ../nixpkgs/ecspresso { };
  # isucrud = pkgs.callPackage ../nixpkgs/isucrud { };
  # mitamae = pkgs.callPackage ../pkgs/mitamae { };
  tmux-sixel = pkgs.callPackage ../nixpkgs/tmux-sixel { };
in with pkgs; [
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
  php83
  phpactor
  rubyPackages.solargraph
  terraform-ls
  yaml-language-server

  # for gnupg
  pinentry-emacs
  gnupg

  # for essential tools
  bat
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
  htop
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
  # ecspresso
  ffmpeg
  gibo
  graphviz
  iftop
  imagemagick
  # isucrud
  # mitamae
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

  # for jokes
  asciiquarium
  cmatrix
  sl
  silicon
  genact

  # for ai
  ollama

  # for emacs
  mu
  emacsPackages.mu4e
]
