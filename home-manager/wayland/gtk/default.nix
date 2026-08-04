{ pkgs, ... }:
let
  draculaTheme = {
    name = "Dracula";
    # `dracula-theme` was removed from nixpkgs: it depended on
    # `gtk-engine-murrine`, which was dropped as unmaintained upstream and
    # GTK 2-only. Keep the theme *name* so the setting stays declared, but stop
    # referencing the removed package.
    # package = pkgs.dracula-theme;
  };
in
{
  gtk.enable = true;
  gtk.theme = draculaTheme;
  gtk.iconTheme.name = "Dracula";
  gtk.iconTheme.package = pkgs.dracula-icon-theme;
  gtk.gtk4.theme = draculaTheme;
}
