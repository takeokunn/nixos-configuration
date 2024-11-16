{
  services = {
    nix-daemon.enable = true;
    sketchybar.enable = true;

    offlineimap = {
      enable = true;
      startInterval = 600;
    };
  };
}
