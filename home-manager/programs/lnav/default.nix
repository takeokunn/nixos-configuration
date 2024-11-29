{
  programs.lnav = {
    enable = false;
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

  programs.fish = {
    interactiveShellInit = ''
      set -x MANPAGER "lnav -q"
    '';
  };
}
