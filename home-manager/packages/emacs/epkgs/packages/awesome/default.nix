{
  pkgs,
  epkgs,
  nurPkgs,
}:
let
  pinentry-el = epkgs.pinentry;
  sudden-death = nurPkgs.emacs-sudden-death;
  zalgo-mode = nurPkgs.emacs-zalgo-mode;
  emacs-arto = nurPkgs.emacs-arto;
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
  pinentry-el

  # Help
  helpful

  # Joke
  hacker-typer
  power-mode
  sudden-death
  redacted
  lorem-ipsum
  zalgo-mode
  emacs-arto

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
