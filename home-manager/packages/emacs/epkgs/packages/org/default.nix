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
  packages.org-dashboard
  packages.org-volume
  org-ql
  org-sliced-images
  org-preview-html
  org-table-highlight

  # Theme
  org-superstar

  # Content
  toc-org

  # Presentation
  org-tree-slide

  # Org Link
  orgit

  # Org Agenda
  org-super-agenda
  org-hyperscheduler

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
  ob-deno
  ob-fsharp
  packages.ob-treesitter
  packages.ob-racket
  ob-base64
  ob-prolog
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
  org-roam-ql-ql
]
