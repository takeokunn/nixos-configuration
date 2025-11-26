{ pkgs }:
with pkgs;
[
  # for terminal tools
  devenv
  unixtools.watch

  # for git
  ghq

  # for nix
  nix-output-monitor
]
++ lib.optionals stdenv.isDarwin [
  # for macOS (former brews)
  pinentry_mac
  terminal-notifier
]
