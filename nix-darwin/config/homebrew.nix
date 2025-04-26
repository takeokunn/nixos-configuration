{
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "uninstall";
    };
    brews = [
      "pinentry-mac"
      "sketchybar"
      "terminal-notifier"
    ];
    casks = [
      "aquaskk"
      "chatgpt"
      "claude"
      "cleanshot"
      "excalidraw"
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
    # masApps = {
    #   LINE = 539883307;
    #   amazon-kindle = 302584613;
    # };
  };
}
