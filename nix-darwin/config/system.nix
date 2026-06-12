{ username }:
{
  launchd.daemons.sysctl-max-files = {
    serviceConfig = {
      Label = "org.nixos.sysctl-max-files";
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/usr/sbin/sysctl -w kern.maxfiles=1048576 kern.maxfilesperproc=524288"
      ];
      RunAtLoad = true;
    };
  };

  time.timeZone = "Asia/Tokyo";

  power.restartAfterFreeze = true;
  power.sleep.allowSleepByPowerButton = true;

  system.primaryUser = username;
  system.stateVersion = 5;

  system.startup.chime = false;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  system.defaults.SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;
  system.defaults.LaunchServices.LSQuarantine = false;

  system.defaults.NSGlobalDomain = {
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

  system.defaults.finder = {
    AppleShowAllFiles = true;
    AppleShowAllExtensions = true;
    _FXShowPosixPathInTitle = true;
    ShowPathbar = true;
    ShowStatusBar = true;
    FXEnableExtensionChangeWarning = false;
  };

  system.defaults.dock = {
    autohide = true;
    show-recents = false;
    launchanim = false;
  };

  system.defaults.trackpad = {
    Clicking = true;
    Dragging = true;
    TrackpadThreeFingerDrag = false;
    TrackpadThreeFingerHorizSwipeGesture = 2;
    TrackpadThreeFingerVertSwipeGesture = 2;
  };

  system.defaults.controlcenter.BatteryShowPercentage = true;

  system.defaults.WindowManager.EnableStandardClickToShowDesktop = false;
}
