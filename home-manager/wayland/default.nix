# Linux-only. Imported conditionally from home-manager/advanced.nix so the
# module system never evaluates it (and its Linux-only options) on Darwin.
{ pkgs, ... }:
{
  imports = [
    ./niri
    ./fuzzel
    ./networkmanager-dmenu
    ./yazi
    ./clipse
    ./swww
    ./waybar
    ./gtk
    ./qt
    ./hypridle
    ./hyprlock
    ./swayosd
    ./wlsunset
    ./kanshi
    ./playerctld
    ./cliphist
    ./wl-clip-persist
    ./easyeffects
    ./mako
    ./impermanence
  ];

  home.packages = with pkgs; [
    networkmanagerapplet
    networkmanager_dmenu
  ];
}
