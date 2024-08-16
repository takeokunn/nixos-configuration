{ username }: {
  xserver = {
    enable = true;
    xkb.layout = "jp";
    displayManager.gdm.enable = true;
  };

  displayManager.autoLogin = {
    enable = true;
    user = username;
  };

  pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
    jack.enable = true;
  };

  xremap = {
    userName = username;
    serviceMode = "system";
    config = {
      modmap = [{
        name = "CapsLock to Ctrl";
        remap = { CapsLock = "Ctrl_L"; };
      }];
    };
  };

  ntp.enable = true;

  ollama = {
    enable = true;
    models = "gemma2:27b";
  };

  tlp = {
    enable = true;
    settings = {
      AHCI_RUNTIME_PM_ON_AC = "auto";
      AHCI_RUNTIME_PM_ON_BAT = "auto";
      RUNTIME_PM_ON_AC = "auto";
      RUNTIME_PM_ON_BAT = "auto";
    };
  };

  offlineimap.enable = true;
}
