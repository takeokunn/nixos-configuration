{
  services = {
    nix-daemon.enable = true;
    cachix-agent.enable = true;

    offlineimap = {
      enable = true;
      startInterval = 600;
    };
  };
}
