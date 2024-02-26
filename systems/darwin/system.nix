{ lib, ... }: {
  services.nix-daemon.enable = true;
  nix.settings.experimental-features = "nix-command flakes";

  nixpkgs.system = "aarch64-darwin";

  users.users.obara.home = "/Users/obara";
  system = {
    stateVersion = 4;
    defaults = {
      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;
      LaunchServices.LSQuarantine = false;
      NSGlobalDomain.AppleShowAllExtensions = true;
      finder = {
        AppleShowAllFiles = true;
        AppleShowAllExtensions = true;
        _FXShowPosixPathInTitle = true;
      };
      dock = {
        autohide = true;
        show-recents = false;
        launchanim = false;
        orientation = "bottom";
      };
    };
  };
}
