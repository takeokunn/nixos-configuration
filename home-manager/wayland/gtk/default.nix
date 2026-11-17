{ pkgs, ... }:
{
  gtk.enable = true;
  gtk.iconTheme.name = "Dracula";
  gtk.iconTheme.package = pkgs.dracula-icon-theme;
  stylix.targets.gtk.enable = true;

  # Without this the portal reports no preference, and Chrome and Electron apps render light.
  dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";

  # One cursor for niri, GTK, Qt and XWayland clients.
  home.pointerCursor = {
    enable = true;
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };
}
