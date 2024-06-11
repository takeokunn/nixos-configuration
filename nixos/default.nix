{ config, pkgs, ... }: {
  boot = {
    hardwareScan = true;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
    enableIPv6 = true;
    wireless.enable = true;
  };

  time = { timeZone = "Asia/Tokyo"; };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "ja_JP.UTF-8";
      LC_IDENTIFICATION = "ja_JP.UTF-8";
      LC_MEASUREMENT = "ja_JP.UTF-8";
      LC_MONETARY = "ja_JP.UTF-8";
      LC_NAME = "ja_JP.UTF-8";
      LC_NUMERIC = "ja_JP.UTF-8";
      LC_PAPER = "ja_JP.UTF-8";
      LC_TELEPHONE = "ja_JP.UTF-8";
      LC_TIME = "ja_JP.UTF-8";
    };
  };

  services = {
    xserver = {
      enable = true;
      displayManager = {
        gdm.enable = true;
        gnome.enable = true;
      };

      xkb = {
        layout = "jp";
        variant = "";
        options = "ctrl:swapcaps";
      };

    };

    tlp.enable = true;
    printing.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
      media-session.enable = true;
    };
  };

  console = { keyMap = "jp106"; };

  sound = { enable = true; };

  hardware = {
    opengl.enable = true;
    bluetooth.enable = true;
  };

  security = {
    rtkit.enable = true;
    tpm2.enable = true;
  };

  users.users = {
    take = {
      isNormalUser = true;
      extraGroups = [ "networkmanager" "wheel" ];
      shell = pkgs.fish;
      useDefaultShell = true;
      packages = with pkgs; [ firefox google-chrome fcitx5-skk ];
    };
  };

  nix = {
    gc.automatic = true;
    gc.options = "--delete-older-than 3d";
  };
  nixpkgs = { config.allowUnfree = true; };

  programs = {
    fish.enable = true;

    gnupg = {
      enable = true;
      agent = { enable = true; };
    };

    google-chrome = {
      enable = true;
      commandLineArgs =
        [ "--enable-features=UseOzonePlatfor" "--ozone-platform=x11" ];
    };

    # sway = {
    #   enable = true;
    #   xwayland.enable = true;
    # };
    # waybar.enable = true;
  };

  system = {
    stateVersion = "24.11";

    # MEMO: 有効にしたい
    autoUpgrade.enable = false;
  };

  fonts = {
    fontDir.enable = true;
    packages = with pkgs; [
      dejavu_fonts
      freefont_ttf
      gyre-fonts
      hackgen-font
      hackgen-nf-font
      liberation_ttf
      migu
      nerdfonts
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      noto-fonts-color-emoji
      noto-fonts-emoji
      noto-fonts-emoji-blob-bin
      noto-fonts-lgc-plus
      noto-fonts-monochrome-emoji
      twemoji-color-font
      unifont
    ];
  };
}
