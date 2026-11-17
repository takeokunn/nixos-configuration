{
  programs.lnav.enable = true;
  programs.lnav.config.ui.theme = "dracula";
  xdg.configFile."lnav/formats/installed/".source = ./formats;
  xdg.configFile."lnav/formats/installed/".recursive = true;
}
