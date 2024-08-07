{ pkgs }:
with pkgs; [
  # for terminal tools
  devbox
  devenv
  du-dust
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
  pwgen
]
