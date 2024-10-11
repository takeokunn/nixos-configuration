{ pkgs }: {
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
      universalaccess = {
        reduceMotion = true;
        reduceTransparency = true;
      };
      dock = {
        autohide = true;
        show-recents = false;
        launchanim = false;
        orientation = "bottom";
      };
      trackpad = {
        Clicking = true;
        Dragging = true;
      };
    };

    environment.shells = with pkgs; [ fish ];

    activationScripts.extraActivation.text = ''
      softwareupdate --all --install
    '';
  };
}
