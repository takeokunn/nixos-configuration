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
  # Basic
  org-journal
  org-generate
  org-pomodoro
  packages.org-view-mode
  org-random-todo
  org-projectile
  packages.org-dashboard
  packages.org-volume
  org-ql

  # Theme
  org-superstar

  # Content
  toc-org

  # Presentation
  org-tree-slide

  # Org Link
  org-link-beautify
  orgit

  # Org Agenda
  org-super-agenda
  org-hyperscheduler
  # org-timeblock

  # Org External Tools
  org-redmine
  org-ai

  # Org Babel
  ob-async
  packages.ob-fish
  ob-rust
  ob-go
  ob-translate
  ob-typescript
  ob-php
  packages.ob-phpstan
  ob-http
  ob-mermaid
  ob-graphql
  ob-swift
  ob-elixir
  ob-dart
  ob-fsharp
  packages.ob-treesitter
  packages.ob-racket
  ob-base64
  org-nix-shell

  # Org Publish
  ox-gfm
  ox-zenn
  packages.ox-hatena
  ox-qmd
  ox-hugo
  packages.ox-typst

  # Org Roam
  org-roam
  consult-org-roam
  org-roam-ui
  org-roam-timestamps
  org-roam-ql
]
