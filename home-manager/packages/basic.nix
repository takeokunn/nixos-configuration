{ pkgs }:
with pkgs; [
  sheldon

  # for terminal tools
  devbox
  devenv
  du-dust
  git
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
  sops
  pwgen

  # for jokes
  asciiquarium
  cmatrix
  fastfetch
  genact
  sl
]
