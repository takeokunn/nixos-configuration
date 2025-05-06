{ pkgs }:
{
  services.mako = {
    enable = pkgs.stdenv.isLinux;
    settings = {
      backgroundColor = "#282a36";
      textColor = "#44475a";
      borderColor = "#282a36";
    };
    # settings = ''
    #   [urgency=low]
    #   border-color=#282a36

    #   [urgency=normal]
    #   border-color=#f1fa8c

    #   [urgency=high]
    #   border-color=#ff5555
    # '';
  };
}
