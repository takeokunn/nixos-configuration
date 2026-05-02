{ pkgs }:
{
  xdg.configFile."networkmanager-dmenu/config.ini" = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    text = ''
      [dmenu]
      dmenu_command = fuzzel
      wifi_chars = ▂▄▆█
      format = {name} {sec} {bars}

      [editor]
      terminal = kitty
      gui_if_available = True
      gui = nm-connection-editor
    '';
  };
}
