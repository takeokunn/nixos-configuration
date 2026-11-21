{ pkgs, ... }:
{
  programs.niri.enable = true;

  environment.systemPackages = with pkgs; [
    xwayland-satellite
  ];

  xdg.portal.enable = true;
  xdg.portal.extraPortals = with pkgs; [
    xdg-desktop-portal-gtk
    xdg-desktop-portal-gnome
  ];
  xdg.portal.config.common.default = [ "gtk" ];

  security.polkit.enable = true;
  # GTK4 polkit agent, so Stylix's GTK theme applies to its dialogs. It starts with
  # graphical-session.target, which niri-session brings up.
  security.soteria.enable = true;

  # Nautilus needs gvfs for the trash and for network locations.
  services.gvfs.enable = true;
}
