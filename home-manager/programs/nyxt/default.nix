{ pkgs }:
{
  programs.nyxt.enable = pkgs.stdenv.isLinux;

  xdg.configFile."nyxt/config.lisp".source = ./config.lisp;
}
