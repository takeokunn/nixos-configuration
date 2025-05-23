{ pkgs, username }:
{
  system = {
    primaryUser = username;
    stateVersion = 5;
    defaults = {
      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;
      LaunchServices.LSQuarantine = false;
      NSGlobalDomain = {
        _HIHideMenuBar = true;
        AppleShowAllExtensions = true;
      };
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
      softwareupdate --all --install
    '';
  };
}
