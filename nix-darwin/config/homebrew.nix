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
      # "sketchybar"
      "terminal-notifier"
    ];
    casks = [
      "aquaskk"
      "chatgpt"
      "claude"
      "cleanshot"
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
 };
}
