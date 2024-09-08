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
      "keycastr"
      "orbstack"
      "raycast"
      "sequel-ace"
      "sf-symbols"
      "sublime-text"
    ];
    masApps = {
      LINE = 539883307;
      Xcode = 497799835;
    };
  };
}
