{ epkgs, pkgs, sources }:
let plugins = pkgs.callPackage ./plugins.nix { inherit sources epkgs; };
in with epkgs; [
  # Basic
  org-journal
  org-generate
  org-pomodoro
  plugins.org-view-mode
  org-random-todo
  org-projectile
  plugins.org-dashboard
  plugins.org-volume
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
  org-timeblock

  # Org External Tools
  org-redmine
  org-ai

  # Org Babel
  ob-async
  # ob-fish
  ob-rust
  ob-go
  ob-translate
  ob-typescript
  ob-php
  plugins.ob-phpstan
  ob-http
  ob-mermaid
  ob-graphql
  ob-swift
  ob-elixir
  ob-dart
  ob-fsharp
  plugins.ob-treesitter
  ob-base64
  org-nix-shell

  # Org Publish
  ox-gfm
  ox-zenn
  plugins.ox-hatena
  ox-qmd
  ox-hugo

  # Org Roam
  org-roam
  consult-org-roam
  org-roam-ui
  org-roam-timestamps
  org-roam-ql
]
