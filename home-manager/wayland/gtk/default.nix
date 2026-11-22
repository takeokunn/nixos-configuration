{ config, ... }:
let
  inherit (config.stylix) fonts;
in
{
  gtk.enable = true;
  stylix.targets.gtk.enable = true;

  # Without this the portal reports no preference, and Chrome and Electron apps render light.
  dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
  # Stylix sets the interface font only; GTK apps that ask for a monospace font read this key.
  dconf.settings."org/gnome/desktop/interface".monospace-font-name =
    "${fonts.monospace.name} ${toString fonts.sizes.applications}";

  # stylix.cursor (nixos/config/stylix.nix) sets the theme; these hand it to GTK and XWayland
  # clients as well, so niri, GTK, Qt and XWayland show one cursor.
  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
  };
}
