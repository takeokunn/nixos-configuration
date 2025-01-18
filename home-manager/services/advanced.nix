{ pkgs, emacsPkg }:
let
  git-sync = import ./git-sync;
  emacs = import ./emacs { inherit emacsPkg; };
  mako = import ./mako { inherit pkgs; };
  ollama = import ./ollama { inherit pkgs; };
  swayidle = import ./swayidle { inherit pkgs; };
  swaync = import ./swaync { inherit pkgs; };
  swayosd = import ./swayosd { inherit pkgs; };
in
[
  git-sync
  emacs
  mako
  ollama
  swayidle
  swaync
  swayosd
]
