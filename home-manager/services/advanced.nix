{ pkgs, emacsPkg }:
let
  autorandr = import ./autorandr { inherit pkgs; };
  emacs = import ./emacs { inherit pkgs emacsPkg; };
  mako = import ./mako { inherit pkgs; };
  ollama = import ./ollama { inherit pkgs; };
  gpg-agent = import ./gpg-agent;
  swayidle = import ./swayidle { inherit pkgs; };
in
[
  autorandr
  emacs
  mako
  ollama
  gpg-agent
  swayidle
]
