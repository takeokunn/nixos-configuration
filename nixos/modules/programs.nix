{ pkgs }: {
  fish.enable = true;
  firefox.enable = true;
  noisetorch.enable = true;

  chromium = {
    enable = true;
    extensions = [
      "gppongmhjkpfnbhagpmjfkannfbllamg" # Wappalyzer
      "kfdibhbheajeacnkkakomaliggbgndcf" # Chromemacs
      "gighmmpiobklfepjocnamgkkbiglidom" # Adblock
      "hkgfoiooedgoejojocmhlaklaeopbecg" # Picture-in-Picture
    ];
  };

  gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  sway = {
    enable = true;
    xwayland.enable = true;
    wrapperFeatures.gtk = true;
  };
  swaylock.enable = true;
  waybar.enable = true;
  rofi.enable = true;
}
