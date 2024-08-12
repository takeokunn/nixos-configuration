{ pkgs, username, ... }: {
  environment.shells = with pkgs; [ fish ];

  nix = {
    gc = {
      automatic = true;
      interval = {
        Hour = 11;
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
        # global settings
        debug_output = "on";
        external_bar = "all:30:0";
        menubar_opacity = "1.0";
        mouse_follows_focus = "on";
        focus_follows_mouse = "off";
        display_arrangement_order = "default";
        window_origin_display = "default";
        window_placement = "second_child";
        window_zoom_persist = "on";
        window_shadow = "float";
        window_opacity = "on";
        window_opacity_duration = "0.0";
        active_window_opacity = "1.0";
        normal_window_opacity = "0.90";
        window_animation_duration = "0.0";
        window_animation_easing = "ease_out_circ";
        insert_feedback_color = "0xffd75f5f";
        split_ratio = "0.50";
        split_type = "auto";
        mouse_modifier = "fn";
        mouse_action1 = "move";
        mouse_action2 = "resize";
        mouse_drop_action = "swap";

        # space settings
        layout = "bsp";
        top_padding = "10";
        bottom_padding = "10";
        left_padding = "10";
        right_padding = "10";
        window_gap = "2";
        auto_balance = "on";
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

        # workspace focus
        alt - 1 : yabai -m space --focus 1
        alt - 2 : yabai -m space --focus 2
        alt - 3 : yabai -m space --focus 3
        alt - 4 : yabai -m space --focus 4
        alt - 5 : yabai -m space --focus 5
        alt - 6 : yabai -m space --focus 6
        alt - 7 : yabai -m space --focus 7
        alt - 8 : yabai -m space --focus 8
        alt - 9 : yabai -m space --focus prev
        alt - 0 : yabai -m space --focus next

        # move window
        shift + alt - h : yabai -m window --warp west
        shift + alt - j : yabai -m window --warp south
        shift + alt - k : yabai -m window --warp north
        shift + alt - l : yabai -m window --warp east

        shift + alt - 1 : yabai -m window --space 1
        shift + alt - 2 : yabai -m window --space 2
        shift + alt - 3 : yabai -m window --space 3
        shift + alt - 4 : yabai -m window --space 4
        shift + alt - 5 : yabai -m window --space 5
        shift + alt - 6 : yabai -m window --space 6
        shift + alt - 7 : yabai -m window --space 7
        shift + alt - 8 : yabai -m window --space 8
        shift + alt - 9 : yabai -m window --space prev
        shift + alt - 0 : yabai -m window --space next

        # fullscreen / floating
        # toggle window native fullscreen
        shift + alt - f : yabai -m window --toggle native-fullscreen
        # toggle window fullscreen zoom
        alt - f : yabai -m window --toggle zoom-fullscreen
        # float / unfloat window and restore position
        shift + alt - space : yabai -m window --toggle float && yabai -m window --grid 4:4:1:1:2:2

        # toggle window split type
        alt - e : yabai -m window --toggle split

        ctrl + alt - a : yabai -m space --layout bsp
        ctrl + alt - s : yabai -m space --layout stack
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
    taps = [ "dracula/install" ];
    casks = [
      "aquaskk"
      "orbstack"
      "sequel-ace"
      "google-chrome"
      "sublime-text"
      "dracula-xcode"
    ];
    masApps = {
      LINE = 539883307;
      Xcode = 497799835;
    };
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
