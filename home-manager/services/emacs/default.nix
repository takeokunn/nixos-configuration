{ pkgs, emacsPkg, ... }:
{
  services.emacs = {
    enable = pkgs.stdenv.isLinux;
    package = emacsPkg;
    client.enable = true;
  };
}
