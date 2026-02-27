{
  epkgs,
  pkgs,
  nurPkgs,
}:
with epkgs;
[
  # check
  flycheck
  flycheck-elsa
  flycheck-projectile
  flycheck-cfn

  # completion
  corfu
  corfu-terminal
  cape
  prescient
  kind-icon

  # git
  magit
  git-gutter
  git-gutter-fringe
  git-timemachine
  gist
  blamer
  git-auto-commit-mode

  # keyboard
  key-chord
  key-combo
  dmacro
  god-mode
  which-key

  # refactor
  emr

  # snippet
  yasnippet
  consult-yasnippet

  # narrowing
  fancy-narrow

  # LSP
  eglot-booster
  lsp-mode
  lsp-sourcekit
  ccls
  consult-lsp
  dap-mode
  lsp-ui
  lsp-scheme
  lsp-dart
  # lsp-bridge

  # Syntax
  syntax-subword

  # Undo
  undo-tree

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
