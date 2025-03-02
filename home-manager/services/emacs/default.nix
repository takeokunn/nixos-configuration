{ pkgs, emacsPkg, ... }:
{
  services.emacs = {
    enable = pkgs.stdenv.isLinux;
    package = emacsPkg;
    client.enable = true;
    defaultEditor = true;
  };

  programs.fish = {
    interactiveShellInit = ''
      set -x EDITOR 'emacsclient -nw --alternate-editor=""'
    '';
  };
}
