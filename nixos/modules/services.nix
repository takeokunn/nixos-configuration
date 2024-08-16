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
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    pulse.enable = true;
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
