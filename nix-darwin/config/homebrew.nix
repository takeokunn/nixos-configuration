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
      "FelixKratz/formulae"
      "nikitabobko/tap"
    ];
    brews = [
      "pinentry-mac"
      "sketchybar"
      "terminal-notifier"
    ];
    casks = [
      "aerospace"
      "android-studio"
      "aquaskk"
      "chatgpt"
      "dracula-xcode"
      "flutter"
      "flutterflow"
      "font-sketchybar-app-font"
      "google-chrome"
      "keycastr"
      "obs"
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
