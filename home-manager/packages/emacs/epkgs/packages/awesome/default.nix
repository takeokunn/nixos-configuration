{
  pkgs,
  epkgs,
  sources,
}:
let
  packages = pkgs.callPackage ./packages.nix { inherit sources epkgs; };
in
with epkgs;
[
  dumb-jump

  # Command
  amx

  # Cursor
  avy
  avy-zap
  expand-region
  multiple-cursors

  # Dictionary
  define-word

  # EWW
  eww-lnum

  # GC
  gcmh

  # GPG
  packages.pinentry-el

  # Help
  helpful

  # Joke
  hacker-typer
  power-mode
  packages.sudden-death
  redacted
  lorem-ipsum
  packages.zalgo-mode

  # Minor Modes
  command-log-mode

  # Shell
  exec-path-from-shell

  # Utility
  lte

  # Password
  password-store
  password-store-otp
  pass
  password-generator
  sops

  # pdf
  pdf-tools

  # Mouse
  inhibit-mouse
]
