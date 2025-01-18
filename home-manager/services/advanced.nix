{ pkgs, emacsPkg }:
let
  emacs = import ./emacs { inherit emacsPkg; };
  mako = import ./mako { inherit pkgs; };
  ollama = import ./ollama { inherit pkgs; };
  gpg-agent = import ./gpg-agent;
  swayidle = import ./swayidle { inherit pkgs; };
  swaync = import ./swaync { inherit pkgs; };
  swayosd = import ./swayosd { inherit pkgs; };
in
[
  emacs
  mako
  ollama
  gpg-agent
  swayidle
  swaync
  swayosd
]
