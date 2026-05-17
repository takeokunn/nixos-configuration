{
  pkgs,
  nurPkgs,
  emacsLib,
}:
pkgs.lib.optionals pkgs.stdenv.isLinux [
  (import ./niri { inherit pkgs emacsLib; })
  (import ./fuzzel)
  (import ./networkmanager-dmenu)
  (import ./yazi { inherit pkgs; })
  (import ./clipse { inherit pkgs; })
  (import ./swww { inherit pkgs nurPkgs; })
  (import ./waybar)
  (import ./gtk { inherit pkgs; })
  (import ./qt)
  (import ./hypridle)
  (import ./hyprlock)
  (import ./swayosd { inherit pkgs; })
  (import ./wlsunset)
  (import ./kanshi)
  (import ./playerctld)
  (import ./cliphist)
  (import ./wl-clip-persist)
  (import ./easyeffects)
  (import ./mako)
  (import ./impermanence)
  {
    home.packages = with pkgs; [
      networkmanagerapplet
      networkmanager_dmenu
    ];
  }
]
