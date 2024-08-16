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
