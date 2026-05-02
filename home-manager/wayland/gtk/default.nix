{ pkgs, ... }:
let
  draculaTheme = {
    name = "Dracula";
    package = pkgs.dracula-theme;
  };
in
{
  gtk = {
    enable = pkgs.stdenv.isLinux;
    theme = draculaTheme;
    iconTheme = {
      name = "Dracula";
      package = pkgs.dracula-icon-theme;
    };
    gtk4.theme = draculaTheme;
  };
}
