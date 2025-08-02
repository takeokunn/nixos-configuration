{
  programs = {
    fish.enable = true;
    firefox.enable = true;
    noisetorch.enable = true;
    nix-ld.enable = true;

    chromium = {
      enable = true;
      extensions = [
        "gppongmhjkpfnbhagpmjfkannfbllamg" # Wappalyzer
        "kfdibhbheajeacnkkakomaliggbgndcf" # Chromemacs
        "gighmmpiobklfepjocnamgkkbiglidom" # Adblock
        "hkgfoiooedgoejojocmhlaklaeopbecg" # Picture-in-Picture
        "gfapcejdoghpoidkfodoiiffaaibpaem" # Dracula Chrome Theme
        "cejijldbedfmdehondfmoadlkhgjcmkd" # Sheets Row Highlighter
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
  };
}
