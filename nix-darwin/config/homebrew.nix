{
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "uninstall";
    };
    brews = [
      "pdfpc"
      "pinentry-mac"
      "terminal-notifier"
    ];
    casks = [
      "aquaskk"
      "chatgpt"
      "claude"
      "cleanshot"
      "font-hack-nerd-font"
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
