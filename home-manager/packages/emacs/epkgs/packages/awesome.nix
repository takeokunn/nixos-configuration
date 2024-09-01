{ pkgs, epkgs }:
let
  flycheck-projectile =
    pkgs.callPackage ./nixpkgs/flycheck-projectile.nix { inherit epkgs; };
  pinentry-el = pkgs.callPackage ./nixpkgs/pinentry-el.nix { inherit epkgs; };
  sudden-death = pkgs.callPackage ./nixpkgs/sudden-death.nix { inherit epkgs; };
  mu4e-dashboard =
    pkgs.callPackage ./nixpkgs/mu4e-dashboard.nix { inherit epkgs; };
  explain-pause-mode =
    pkgs.callPackage ./nixpkgs/explain-pause-mode.nix { inherit epkgs; };
  view-lock-mode =
    pkgs.callPackage ./nixpkgs/view-lock-mode.nix { inherit epkgs; };
  consult-tramp =
    pkgs.callPackage ./nixpkgs/consult-tramp.nix { inherit epkgs; };
in with epkgs; [
  # Buffer
  auto-save-buffers-enhanced
  editorconfig
  persistent-scratch
  popwin

  # Check
  flycheck
  flycheck-elsa
  flycheck-projectile

  # Client
  md4rd

  # Color
  highlight-indent-guides
  hl-todo
  xterm-color

  # Command
  amx

  # Completion
  corfu
  cape
  prescient
  kind-icon

  # Cursor
  avy
  avy-zap
  expand-region
  multiple-cursors

  # Dictionary
  define-word

  # Dired
  dired-narrow
  dired-open
  dired-quick-sort
  diredfl

  # EWW
  eww-lnum

  # File
  open-junk-file
  vlf

  # Font
  font-lock-studio

  # GC
  gcmh

  # Git
  magit
  magit-file-icons
  magit-gptcommit
  forge
  git-gutter
  git-gutter-fringe
  git-timemachine
  gist
  blamer
  git-auto-commit-mode

  # Googling
  google-this
  google-translate

  # GPG
  pinentry-el

  # Help
  helpful

  # IME
  ddskk
  ddskk-posframe

  # Joke
  hacker-typer
  power-mode
  sudden-death
  redacted
  lorem-ipsum

  # Keyboard
  key-chord
  key-combo
  which-key
  dmacro
  god-mode

  # LSP
  lsp-mode
  lsp-sourcekit
  ccls
  consult-lsp
  lsp-docker
  dap-mode
  lsp-ui
  lsp-scheme
  lsp-haskell
  lsp-pyright
  # lsp-bridge

  # Mail
  mu4e
  mu4e-views
  mu4e-dashboard

  # Minor Modes
  command-log-mode

  # Narrowing
  fancy-narrow
  origami

  # Process
  proced-narrow

  # Project
  projectile
  consult-projectile

  # Refactor
  emr

  # Search
  migemo
  wgrep
  consult
  affe
  compile-multi
  vertico
  marginalia
  orderless

  # Shell
  exec-path-from-shell

  # Snippet
  yasnippet
  consult-yasnippet

  # Statistics
  esup
  explain-pause-mode
  disk-usage
  keyfreq
  uptimes

  # Syntax
  syntax-subword

  # System
  symon

  # Theme
  nerd-icons
  nerd-icons-dired
  nerd-icons-completion
  dashboard
  dimmer
  doom-themes
  doom-modeline
  idle-highlight-mode
  neotree
  nyan-mode
  volatile-highlights
  idle-highlight-mode

  # Undo
  undo-tree

  # View Mode
  view-lock-mode

  # Password
  password-store
  password-store-otp
  pass

  # Utility
  crux
  dogears
  embark
  htmlize
  minimap
  puni
  quickrun
  restclient
  smartparens
  smart-jump
  string-inflection
  sudo-edit
  topsy
  uuid

  # Window
  ace-window
  writeroom-mode
  zoom-window

  # Remote Access
  docker-tramp
  consult-tramp
]
