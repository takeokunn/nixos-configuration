{ pkgs, emacsPkg }:
let
  emacs = import ./emacs { inherit pkgs emacsPkg; };
  ollama = import ./ollama { inherit pkgs; };
  gpg-agent = import ./gpg-agent { inherit pkgs; };

  # Modern services (niri ecosystem)
  hypridle = import ./hypridle { inherit pkgs; };
  hyprlock = import ./hyprlock { inherit pkgs; };
  swayosd = import ./swayosd { inherit pkgs; };
  wlsunset = import ./wlsunset { inherit pkgs; };
  kanshi = import ./kanshi { inherit pkgs; };
  playerctld = import ./playerctld { inherit pkgs; };
  cliphist = import ./cliphist { inherit pkgs; };
  wl-clip-persist = import ./wl-clip-persist { inherit pkgs; };
  easyeffects = import ./easyeffects { inherit pkgs; };

  # Impermanence (Linux only)
  impermanence = import ./impermanence { inherit pkgs; };
in
[
  emacs
  ollama
  gpg-agent

  # Modern services (niri ecosystem)
  hypridle
  hyprlock
  swayosd
  wlsunset
  kanshi
  playerctld
  cliphist
  wl-clip-persist
  easyeffects
]
++ pkgs.lib.optionals pkgs.stdenv.isLinux [
  # Impermanence (Linux only)
  impermanence
]
