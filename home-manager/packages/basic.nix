{ pkgs }:
with pkgs; [
  # for terminal tools
  dasel
  devbox
  devenv
  offlineimap
  openssl
  pv
  rlwrap
  unixtools.procps
  unixtools.watch

  # for git
  gh
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
