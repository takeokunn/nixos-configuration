{ username }:
{
  time.timeZone = "Asia/Tokyo";

  power = {
    restartAfterFreeze = true;
    sleep.allowSleepByPowerButton = true;
  };

  system = {
    primaryUser = username;
    stateVersion = 5;

    startup.chime = false;

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };

    defaults = {
      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;
      LaunchServices.LSQuarantine = false;

      NSGlobalDomain = {
        _HIHideMenuBar = true;
        AppleShowAllExtensions = true;
        AppleInterfaceStyle = "Dark";
        ApplePressAndHoldEnabled = false;
        InitialKeyRepeat = 15;
        KeyRepeat = 2;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
        NSNavPanelExpandedStateForSaveMode = true;
        NSNavPanelExpandedStateForSaveMode2 = true;
      };

      finder = {
        AppleShowAllFiles = true;
        AppleShowAllExtensions = true;
        _FXShowPosixPathInTitle = true;
        ShowPathbar = true;
        ShowStatusBar = true;
        FXEnableExtensionChangeWarning = false;
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
        TrackpadThreeFingerDrag = true;
      };

      controlcenter.BatteryShowPercentage = true;

      WindowManager.EnableStandardClickToShowDesktop = false;
    };

    activationScripts.extraActivation.text = ''
      softwareupdate --all --install
    '';
  };
}
