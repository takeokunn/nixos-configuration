{ username }:
{
  # File descriptor exhaustion ("too many open files") fix.
  # Real-world trigger here: ~10 AI agents run in parallel on a huge project. Each agent
  # subtree (node + MCP servers + language servers + ripgrep + browser) opens many FDs, so
  # both macOS FD axes bite, and they are independent:
  #   1. kern.maxfiles (system aggregate) -> total open files across ALL processes. The
  #      10-agent fleet collectively approaches this ceiling -> raise it high (1048576).
  #   2. kern.maxfilesperproc              -> hard cap a single process can ever reach.
  #   3. launchctl limit maxfiles          -> default soft/hard rlimit launchd hands every
  #      process it spawns, incl. the terminal and therefore every agent child. Defaults to
  #      256 (soft), so raising only the sysctl ceiling does nothing for launchd children.
  # Use a generous-but-finite soft (65536, ~600x measured per-process usage) with a high
  # hard (524288): keeps headroom for the agent fleet while avoiding the known footgun of an
  # enormous *soft* limit (programs that walk/allocate over RLIMIT_NOFILE degrade or break).
  # sysctl must run before launchctl limit, so both live in one ordered daemon. Changing this
  # command re-triggers RunAtLoad on the next darwin-rebuild, so the values reapply reliably.
  launchd.daemons.sysctl-max-files = {
    serviceConfig = {
      Label = "org.nixos.sysctl-max-files";
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/usr/sbin/sysctl -w kern.maxfiles=1048576 kern.maxfilesperproc=524288 && /bin/launchctl limit maxfiles 65536 524288"
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
