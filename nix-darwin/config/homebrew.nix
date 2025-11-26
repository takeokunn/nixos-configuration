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
    ];
    casks = [
      "font-sketchybar-app-font"
      "google-chrome"
      "sf-symbols"
    ];
  };
}
