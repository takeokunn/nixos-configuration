{ pkgs }:
with pkgs; [
  # for terminal tools
  dasel
  devbox
  devenv
  offlineimap
  openssl
  unixtools.procps
  unixtools.watch

  # for git
  gh
  ghq

  # for password tools
  pwgen

  # for jokes
  asciiquarium
  cmatrix
  fastfetch
  genact
  sl
]
