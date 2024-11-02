{
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "uninstall";
    };
    taps = [
      "dracula/install"
      "nikitabobko/tap"
    ];
    brews = [
      "pinentry-mac"
      "terminal-notifier"
    ];
    casks = [
      "aerospace"
      "android-studio"
      "aquaskk"
      "dracula-xcode"
      "flutter"
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
      # Xcode = 497799835;
    };
  };
}
