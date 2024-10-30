{ pkgs }:
with pkgs; [
  # for terminal tools
  devenv
  offlineimap
  unixtools.watch

  # for git
  ghq
]
