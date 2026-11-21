{
  gtk.enable = true;
  stylix.targets.gtk.enable = true;

  # Without this the portal reports no preference, and Chrome and Electron apps render light.
  dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";

  # stylix.cursor (nixos/config/stylix.nix) sets the theme; these hand it to GTK and XWayland
  # clients as well, so niri, GTK, Qt and XWayland show one cursor.
  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
  };
}
