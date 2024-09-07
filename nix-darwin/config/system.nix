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

    activationScripts.extraActivation.text = ''
      chsh -s ${pkgs.fish}/bin/fish
      softwareupdate --all --install
    '';
  };
}
