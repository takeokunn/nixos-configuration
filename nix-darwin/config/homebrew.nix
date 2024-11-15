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
      "FelixKratz/formulae"
    ];
    brews = [
      "pinentry-mac"
      "terminal-notifier"
      "sketchybar"
    ];
    casks = [
      "aerospace"
      "android-studio"
      "aquaskk"
      "chatgpt"
      "dracula-xcode"
      "flutter"
      "flutterflow"
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
    };
  };
}
