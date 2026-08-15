{ epkgs, nurPkgs }:
let
  org-view-mode = epkgs.org-view-mode;
  org-dashboard = epkgs.org-dashboard;
  ox-typst = epkgs.ox-typst;
  doclive = nurPkgs.emacs-doclive;
  org-volume = nurPkgs.emacs-org-volume;
  ob-phpstan = nurPkgs.emacs-ob-phpstan;
  ob-racket = nurPkgs.emacs-ob-racket;
  ob-treesitter = nurPkgs.emacs-ob-treesitter;
  ob-fish = nurPkgs.emacs-ob-fish;
  ox-hatena = nurPkgs.emacs-ox-hatena;
in
with epkgs;
[
  org-journal
  org-generate
  org-pomodoro
  org-view-mode
  org-random-todo
  org-dashboard
  org-volume
  org-ql
  org-sliced-images
  org-preview-html
  doclive

  org-superstar

  toc-org

  org-tree-slide

  orgit

  org-super-agenda
  org-hyperscheduler

  org-redmine
  org-ai

  ob-async
  ob-fish
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
  ob-deno
  ob-fsharp
  ob-treesitter
  ob-racket
  ob-base64
  ob-gleam
  ob-prolog
  org-nix-shell

  ox-gfm
  ox-zenn
  ox-hatena
  ox-qmd
  ox-hugo
  ox-typst

  org-roam
  consult-org-roam
  org-roam-ui
  org-roam-timestamps
  org-roam-ql
  org-roam-ql-ql

  citar
  citar-embark
  citar-org-roam
]
