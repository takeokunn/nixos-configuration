{ pkgs, ... }:
{
  gtk.enable = true;
  gtk.iconTheme.name = "Dracula";
  gtk.iconTheme.package = pkgs.dracula-icon-theme;
  stylix.targets.gtk.enable = true;
}
