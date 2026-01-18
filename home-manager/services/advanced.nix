{ pkgs, emacsPkg }:
let
  emacs = import ./emacs { inherit pkgs emacsPkg; };
  ollama = import ./ollama { inherit pkgs; };
  gpg-agent = import ./gpg-agent { inherit pkgs; };

  # Modern services (niri ecosystem)
  hypridle = import ./hypridle { inherit pkgs; };
  hyprlock = import ./hyprlock { inherit pkgs; };
  wob = import ./wob { inherit pkgs; };
  wlsunset = import ./wlsunset { inherit pkgs; };
  kanshi = import ./kanshi { inherit pkgs; };
  playerctld = import ./playerctld { inherit pkgs; };
in
[
  emacs
  ollama
  gpg-agent

  # Modern services (niri ecosystem)
  hypridle
  hyprlock
  wob
  wlsunset
  kanshi
  playerctld
]
