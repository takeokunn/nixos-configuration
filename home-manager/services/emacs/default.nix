{ emacsPkg, ... }:
{
  services.emacs = {
    enable = true;
    package = emacsPkg;
    client.enable = true;
  };

  programs.fish = {
    interactiveShellInit = ''
      set -x EDITOR 'emacsclient -nw --alternate-editor=""'
    '';
  };
}
