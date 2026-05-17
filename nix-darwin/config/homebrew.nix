{
  homebrew.enable = true;
  homebrew.onActivation.autoUpdate = true;
  homebrew.onActivation.upgrade = true;
  homebrew.onActivation.cleanup = "uninstall";
  homebrew.brews = [
    "pdfpc"
    "docker-credential-helper"
  ];
  homebrew.casks = [
    "aquaskk"
    "cleanshot"
    "claude"
    "font-sketchybar-app-font"
    "google-chrome"
    "sf-symbols"
  ];
}
