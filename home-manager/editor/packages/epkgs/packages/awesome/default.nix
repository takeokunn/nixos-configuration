{ epkgs, nurPkgs }:
let
  pinentry-el = epkgs.pinentry;
  sudden-death = nurPkgs.emacs-sudden-death;
  zalgo-mode = nurPkgs.emacs-zalgo-mode;
  emacs-arto = nurPkgs.emacs-arto;
  soft-narrow = nurPkgs.emacs-soft-narrow;
in
with epkgs;
[
  dumb-jump

  avy
  avy-zap
  expand-region
  multiple-cursors

  define-word

  eww-lnum

  gcmh

  pinentry-el

  helpful

  hacker-typer
  power-mode
  sudden-death
  redacted
  lorem-ipsum
  zalgo-mode
  emacs-arto

  command-log-mode
  soft-narrow

  exec-path-from-shell

  lte

  password-store
  password-store-otp
  pass
  password-generator
  sops

  pdf-tools

  inhibit-mouse
]
