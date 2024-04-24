{ pkgs, ... }: {
  nix = {
    gc = {
      automatic = true;
      user = "obara";
      interval = {
        Hour = 9;
        Minute = 0;
      };
      options = "--delete-older-than 7d";
    };
    settings.experimental-features = "nix-command flakes";
  };
  services.nix-daemon.enable = true;

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      noto-fonts
      noto-fonts-lgc-plus
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      noto-fonts-color-emoji
      noto-fonts-emoji-blob-bin
      noto-fonts-monochrome-emoji
      hackgen-font
      hackgen-nf-font
    ];
  };

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

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "uninstall";
    };
    casks = [
      "aquaskk"
      "drawio"
      "docker"
      "google-chrome"
      "keycastr"
      "raycast"
      "sequel-ace"
      "sublime-text"
      "wireshark"
      "discord"
      "raycast"
      "slack"
    ];
    masApps = {
      Xcode = 497799835;
      LINE = 539883307;
    };
  };

  networking = {
    knownNetworkServices = [ "Wi-Fi" "Ethernet Adaptor" ];
    dns = [ "8.8.8.8" "8.8.4.4" "2001:4860:4860::8888" "2001:4860:4860::8844" ];
  };

  security.pam.enableSudoTouchIdAuth = true;

  # launchd.user.agents.ollama = {
  #   serviceConfig = {
  #     ProgramArguments = [ "${pkgs.ollama}/bin/ollama" "serve" ];
  #     KeepAlive = true;
  #     RunAtLoad = true;
  #   };
  # };
}
