{
  services = {
    nix-daemon.enable = true;
    sketchybar.enable = false;

    offlineimap = {
      enable = true;
      startInterval = 600;
    };
  };
}
