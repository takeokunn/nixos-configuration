{ pkgs, ... }:
{
  gtk = {
    enable = pkgs.stdenv.isLinux;
    theme = {
      name = "Dracula";
      package = pkgs.dracula-theme;
    };
    iconTheme = {
      name = "Dracula";
      package = pkgs.dracula-icon-theme;
    };
  };
}
