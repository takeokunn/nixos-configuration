{
  epkgs,
  pkgs,
  sources,
}:
let
  packages = pkgs.callPackage ./packages.nix { inherit sources epkgs; };
in
with epkgs;
[
  # check
  flycheck
  flycheck-elsa
  packages.flycheck-projectile
  flycheck-cfn

  # completion
  corfu
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
  origami

  # LSP
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

  # View Mode
  packages.view-lock-mode

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
