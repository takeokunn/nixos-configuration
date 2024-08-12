{ pkgs, username, ... }: {
  environment.shells = with pkgs; [ fish ];

  nix = {
    gc = {
      automatic = true;
      interval = {
        Hour = 9;
        Minute = 0;
      };
      options = "--delete-older-than 3d";
    };
    optimise.automatic = true;
    settings = {
      experimental-features = "nix-command flakes";
      max-jobs = 8;
    };

    extraOptions = ''
      extra-substituters = https://devenv.cachix.org
      extra-trusted-public-keys = devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=
    '';
  };

  services = {
    nix-daemon.enable = true;

    offlineimap = {
      enable = true;
      startInterval = 600;
    };

    yabai = {
      enable = false;
      config = {
        debug_output = "on";
        external_bar = "all:30:0";
      };
    };

    skhd = {
      enable = false;
      skhdConfig = ''
        # focus window
        shift + alt - x : yabai -m window --focus recent || yabai -m display --focus recent
        alt - h : yabai -m window --focus west || yabai -m window --focus stack.prev || yabai -m display --focus west
        alt - j : yabai -m window --focus south || yabai -m display --focus south
        alt - k : yabai -m window --focus north || yabai -m display --focus north
        alt - l : yabai -m window --focus east || yabai -m window --focus stack.next || yabai -m display --focus east
        alt - z : yabai -m window --focus stack.prev
        alt - c : yabai -m window --focus stack.next
      '';
    };
  };

  fonts = {
    packages = with pkgs; [
      noto-fonts
      noto-fonts-lgc-plus
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      noto-fonts-color-emoji
      noto-fonts-emoji-blob-bin
      noto-fonts-monochrome-emoji
      hackgen-font
      hackgen-nf-font
      nerdfonts
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
      trackpad = {
        Clicking = true;
        Dragging = true;
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
    casks =
      [ "aquaskk" "orbstack" "sequel-ace" "google-chrome" "sublime-text" ];
    masApps.LINE = 539883307;
  };

  networking = {
    knownNetworkServices =
      [ "Wi-Fi" "Ethernet Adaptor" "Thunderbolt Ethernet" ];
    dns = [
      "8.8.8.8"
      "8.8.4.4"
      "1.1.1.1"
      "1.0.0.1"
      "2001:4860:4860::8888"
      "2001:4860:4860::8844"
      "2606:4700:4700::1111"
      "2606:4700:4700::1001"
    ];
  };

  security = {
    sudo.extraConfig = ''
      ${username} ALL=NOPASSWD: ALL
    '';
    pam.enableSudoTouchIdAuth = true;
  };

  launchd.agents.ollama = {
    serviceConfig = {
      ProgramArguments = [ "${pkgs.ollama}/bin/ollama" "serve" ];
      KeepAlive = true;
      RunAtLoad = true;
    };
  };
}
