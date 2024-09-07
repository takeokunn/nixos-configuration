{
  services = {
    nix-daemon.enable = true;

    offlineimap = {
      enable = true;
      startInterval = 600;
    };
  };
}
