{ epkgs, pkgs, sources }:
let plugins = pkgs.callPackage ./plugins.nix { inherit sources epkgs; };
in with epkgs; [
  # check
  flycheck
  flycheck-elsa
  plugins.flycheck-projectile
  flycheck-cfn
  flycheck-phpstan

  # completion
  corfu
  cape
  prescient
  kind-icon

  # git
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

  # keyboard
  key-chord
  key-combo
  which-key
  dmacro
  god-mode

  # refactor
  emr

  # snippet
  yasnippet
  consult-yasnippet

  # narrowing
  fancy-narrow
  origami

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

  # Syntax
  syntax-subword

  # Undo
  undo-tree

  # View Mode
  plugins.view-lock-mode

  # Utility
  crux
  dogears
  embark
  embark-consult
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
]
