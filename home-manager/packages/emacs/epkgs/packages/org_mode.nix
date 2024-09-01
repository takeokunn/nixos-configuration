{ epkgs, pkgs }:
let
  org-view-mode =
    pkgs.callPackage ./nixpkgs/org-view-mode.nix { inherit epkgs; };
  org-dashboard =
    pkgs.callPackage ./nixpkgs/org-dashboard.nix { inherit epkgs; };
  org-volume = pkgs.callPackage ./nixpkgs/org-volume.nix { inherit epkgs; };
  ob-phpstan = pkgs.callPackage ./nixpkgs/ob-phpstan.nix { inherit epkgs; };
  ob-treesitter =
    pkgs.callPackage ./nixpkgs/ob-treesitter.nix { inherit epkgs; };
  ox-hatena = pkgs.callPackage ./nixpkgs/ox-hatena.nix { inherit epkgs; };
in with epkgs; [
  # Basic
  org-journal
  org-generate
  org-pomodoro
  org-view-mode
  org-random-todo
  org-projectile
  org-dashboard
  org-volume
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
  ob-phpstan
  ob-http
  ob-mermaid
  ob-graphql
  ob-swift
  ob-elixir
  ob-dart
  ob-fsharp
  ob-treesitter
  ob-base64
  org-nix-shell

  # Org Publish
  ox-gfm
  ox-zenn
  ox-hatena
  ox-qmd
  ox-hugo

  # Org Roam
  org-roam
  consult-org-roam
  org-roam-ui
  org-roam-timestamps
  org-roam-ql
]
