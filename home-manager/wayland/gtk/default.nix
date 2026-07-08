{ pkgs, ... }:
let
  draculaTheme = {
    name = "Dracula";
    package = pkgs.dracula-theme;
  };
in
{
  gtk.enable = true;
  gtk.theme = draculaTheme;
  gtk.iconTheme.name = "Dracula";
  gtk.iconTheme.package = pkgs.dracula-icon-theme;
  gtk.gtk4.theme = draculaTheme;
}
