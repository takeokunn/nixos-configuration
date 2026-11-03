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
    "arto-app/tap/arto"
    "claude"
    "cleanshot"
    "clickup"
    "docker-desktop"
    "font-sketchybar-app-font"
    "google-chrome"
    "microsoft-excel"
    "raycast"
    "sequel-ace"
    "sf-symbols"
    "slack"
    "slite"
    "sublime-text"
  ];
}
