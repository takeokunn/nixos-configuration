{
  enable = true;
  onActivation = {
    autoUpdate = true;
    upgrade = true;
    cleanup = "uninstall";
  };
  taps = [ "dracula/install" ];
  casks = [
    "aquaskk"
    "orbstack"
    "sequel-ace"
    "google-chrome"
    "sublime-text"
    "dracula-xcode"
  ];
  masApps = {
    LINE = 539883307;
    Xcode = 497799835;
  };
}
