{ pkgs }:
let tmux-sixel = pkgs.callPackage ../nixpkgs/tmux-sixel { };
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

  # for pass
  pwgen
  (pass.withExtensions (extensions: with extensions; [ pass-otp ]))

  # for editor
  neovim

  # for shell
  fish
]
