{
  enable = true;
  onActivation = {
    autoUpdate = true;
    upgrade = true;
    cleanup = "uninstall";
  };
  taps = [ "dracula/install" "nikitabobko/tap" ];
  casks = [
    "aquaskk"
    "orbstack"
    "sequel-ace"
    "google-chrome"
    "sublime-text"
    "dracula-xcode"
    "aerospace"
  ];
  masApps = {
    LINE = 539883307;
    Xcode = 497799835;
  };
}
