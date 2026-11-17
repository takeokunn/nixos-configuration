{ pkgs, ... }:
{
  imports = [
    ./niri
    ./fuzzel
    ./networkmanager-dmenu
    ./vicinae
    ./yazi
    ./awww
    ./waybar
    ./gtk
    ./qt
    ./hypridle
    ./hyprlock
    ./swayosd
    ./wlsunset
    ./kanshi
    ./playerctld
    ./wl-clip-persist
    ./easyeffects
    ./mako
    ./zathura
    ./imv
    ./mpv
    ./impermanence
  ];

  home.packages = with pkgs; [
    networkmanagerapplet
    networkmanager_dmenu
    bluetui
  ];
}
