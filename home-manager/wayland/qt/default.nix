{ pkgs, ... }:
{
  qt = {
    enable = pkgs.stdenv.isLinux;
    platformTheme.name = "gtk";
    style.name = "gtk2";
  };
}
