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
      "docker-credential-helper"
    ];
    casks = [
      "aquaskk"
      "cleanshot"
      "font-sketchybar-app-font"
      "google-chrome"
      "sf-symbols"
    ];
  };
}
