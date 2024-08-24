{
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "uninstall";
    };
    taps = [ "dracula/install" "nikitabobko/tap" ];
    casks = [
      "aerospace"
      "aquaskk"
      "dracula-xcode"
      "google-chrome"
      "orbstack"
      "sequel-ace"
      "sublime-text"
    ];
    masApps = {
      LINE = 539883307;
      Xcode = 497799835;
    };
  };
}
