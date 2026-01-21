{ pkgs }:
{
  services.mako = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;

    settings = {
      # Appearance
      font = "HackGen Console NF 14";
      width = 350;
      height = 150;
      margin = "10";
      padding = "12";
      border-size = 2;
      border-radius = 8;

      # Dracula colors with transparency
      background-color = "#282a36ee";
      text-color = "#f8f8f2";
      border-color = "#bd93f9";
      progress-color = "over #44475a";

      # Behavior
      default-timeout = 5000;
      layer = "overlay";
      anchor = "top-right";
      sort = "-time";
      max-visible = 5;
      icons = true;
      max-icon-size = 48;
      markup = true;
      actions = true;
    };

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
