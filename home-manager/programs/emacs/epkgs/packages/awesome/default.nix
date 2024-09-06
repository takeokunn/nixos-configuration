{ pkgs, epkgs, sources }:
let plugins = pkgs.callPackage ./plugins.nix { inherit sources epkgs; };
in with epkgs; [
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
  plugins.pinentry-el

  # Help
  helpful

  # Joke
  hacker-typer
  power-mode
  plugins.sudden-death
  redacted
  lorem-ipsum

  # Minor Modes
  command-log-mode

  # Shell
  exec-path-from-shell

  # Password
  password-store
  password-store-otp
  pass
]
