{ pkgs, ... }:
{
  programs.firefox.enable = pkgs.stdenv.hostPlatform.isLinux;
}
