{ pkgs }:
{
  services.mako = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;

    # Appearance
    font = "HackGen Console NF 11";
    width = 350;
    height = 150;
    margin = "10";
    padding = "12";
    borderSize = 2;
    borderRadius = 8;

    # Dracula colors
    backgroundColor = "#282a36";
    textColor = "#f8f8f2";
    borderColor = "#bd93f9";
    progressColor = "over #44475a";

    # Behavior
    defaultTimeout = 5000;
    layer = "overlay";
    anchor = "top-right";
    sort = "-time";
    maxVisible = 5;
    icons = true;
    maxIconSize = 48;
    markup = true;
    actions = true;

    extraConfig = ''
      [urgency=low]
      border-color=#6272a4
      default-timeout=3000

      [urgency=normal]
      border-color=#bd93f9
      default-timeout=5000

      [urgency=critical]
      border-color=#ff5555
      default-timeout=0
      ignore-timeout=1

      [app-name=spotify]
      border-color=#50fa7b

      [app-name=discord]
      border-color=#8be9fd
    '';
  };
}
