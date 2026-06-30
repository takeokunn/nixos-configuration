{
  homebrew.enable = true;
  homebrew.onActivation.autoUpdate = true;
  homebrew.onActivation.upgrade = true;
  homebrew.onActivation.cleanup = "uninstall";
  homebrew.onActivation.extraFlags = [ "--force-cleanup" ];
  homebrew.brews = [
    "pdfpc"
    "docker-credential-helper"
  ];
  homebrew.casks = [
    "aquaskk"
    "cleanshot"
    "claude"
    "clickup"
    "discord"
    "docker-desktop"
    "element"
    "font-sketchybar-app-font"
    "google-chrome"
    "microsoft-excel"
    "keycastr"
    "postico"
    "postman"
    "raycast"
    "sequel-ace"
    "sf-symbols"
    "slack"
    "slite"
    "sublime-text"
  ];
}
