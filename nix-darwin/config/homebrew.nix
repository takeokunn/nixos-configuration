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
      "cleanshot"
      "clickup"
      "font-hack-nerd-font"
      "font-sketchybar-app-font"
      "google-chrome"
      "keycastr"
      "obs"
      "postman"
      "orbstack"
      "postico"
      "raycast"
      "sequel-ace"
      "sf-symbols"
      "sublime-text"
    ];
  };
}
