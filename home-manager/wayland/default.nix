{
  pkgs,
  nurPkgs,
  emacsLib,
}:
let
  lib = pkgs.lib;
  niri = import ./niri { inherit pkgs emacsLib; };
  fuzzel = import ./fuzzel { inherit pkgs; };
  networkmanager-dmenu = import ./networkmanager-dmenu { inherit pkgs; };
  yazi = import ./yazi { inherit pkgs; };
  clipse = import ./clipse { inherit pkgs; };
  swww = import ./swww { inherit pkgs nurPkgs; };
  waybar = import ./waybar { inherit pkgs; };
  gtk = import ./gtk { inherit pkgs; };
  qt = import ./qt { inherit pkgs; };
  hypridle = import ./hypridle { inherit pkgs; };
  hyprlock = import ./hyprlock { inherit pkgs; };
  swayosd = import ./swayosd { inherit pkgs; };
  wlsunset = import ./wlsunset { inherit pkgs; };
  kanshi = import ./kanshi { inherit pkgs; };
  playerctld = import ./playerctld { inherit pkgs; };
  cliphist = import ./cliphist { inherit pkgs; };
  wl-clip-persist = import ./wl-clip-persist { inherit pkgs; };
  easyeffects = import ./easyeffects { inherit pkgs lib; };
  mako = import ./mako { inherit pkgs; };
in
[
  niri
  fuzzel
  networkmanager-dmenu
  yazi
  clipse
  swww
  waybar
  gtk
  qt
  hypridle
  hyprlock
  swayosd
  wlsunset
  kanshi
  playerctld
  cliphist
  wl-clip-persist
  easyeffects
  mako
]
++ pkgs.lib.optionals pkgs.stdenv.isLinux [
  (import ./impermanence { inherit pkgs; })
  { home.packages = with pkgs; [ networkmanagerapplet networkmanager_dmenu ]; }
]
