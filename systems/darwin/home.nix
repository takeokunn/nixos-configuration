{ pkgs, ... }:
{
  home.stateVersion = "23.11";
  home.packages = with pkgs; [
    # for lanaguage
    clojure
    # deno
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
    php83Packages.composer
    ruby
    roswell
    # terraform
    vlang
    zig

    # for language specific
    gopls
    # gotools
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
    nixfmt
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
    # nodePackages_latest.intelephense
    nodePackages_latest.typescript-language-server
    nodePackages_latest.vim-language-server
    rubyPackages.solargraph
    yaml-language-server

    # for gnupg
    pinentry
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
    tmux
    # personal.tmux-sixel
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
    hugo
    iftop
    imagemagick
    mu
    ncurses
    neofetch
    offlineimap
    openssl
    # personal.mitamae
    # personal.isucrud
    pwgen
    silicon
    sqldef
    terminal-notifier
    tig
    tokei
    unixtools.procps
    unixtools.watch

    # for pass
    bitwarden-cli
    (pass.withExtensions (extensions: with extensions; [ pass-otp ]))

    # for cloud
    # personal.ecspresso
    awscli
    ssm-session-manager-plugin

    # for network
    bettercap
    gping
    speedtest-cli
    tcpdump

    # for editor
    emacs
    editorconfig-core-c
    micro
    nano
    neovim
    tree-sitter
    (tree-sitter.withPlugins (p: builtins.attrValues p))

    # for shell
    fish

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

    # for font
    hackgen-font
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    noto-fonts-emoji-blob-bin
    noto-fonts-lgc-plus
    noto-fonts-monochrome-emoji

    # for application
    # discord
    gimp
    # raycast
    # slack
    # iterm2
  ];
}
