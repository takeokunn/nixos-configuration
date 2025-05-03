{ pkgs }:
{
  programs.lnav = {
    enable = true;
    package = pkgs.lnav;
    config = {
      ui.theme = "dracula";
      format-repos = [
        "https://github.com/hagfelsh/lnav_formats.git"
        "https://github.com/PaulWay/lnav-formats.git"
        "https://github.com/penntaylor/lnav-ruby-logger-format.git"
        "https://github.com/aspiers/lnav-formats.git"
      ];
    };
  };

  xdg.configFile."lnav/formats/installed/" = {
    source = ./formats;
    recursive = true;
  };
}
