{ pkgs, ... }:
{
  programs = {
    fish.enable = true;
    noisetorch.enable = true;
    nix-ld.enable = true;

    # Modern Wayland compositor (niri)
    niri = {
      enable = true;
    };
  };

  # XWayland support via xwayland-satellite
  environment.systemPackages = with pkgs; [
    xwayland-satellite
  ];

  # Enable xdg-desktop-portal for niri
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-gnome
    ];
    config.common.default = [ "gtk" ];
  };

  # Security / Polkit
  security.polkit.enable = true;

  # Japanese input method
  i18n.inputMethod = {
    type = "fcitx5";
    fcitx5 = {
      waylandFrontend = true;
      addons = with pkgs; [
        fcitx5-mozc
        fcitx5-gtk
      ];
    };
  };
}
