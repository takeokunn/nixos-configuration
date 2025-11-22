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
      "docker"
      "font-hack-nerd-font"
      "font-sketchybar-app-font"
      "google-chrome"
      "keycastr"
      "postman"
      "postico"
      "raycast"
      "orbstack"
      "sequel-ace"
      "sf-symbols"
      "sublime-text"
      "ngrok"
    ];
  };
}
