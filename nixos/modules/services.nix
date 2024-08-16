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
}
