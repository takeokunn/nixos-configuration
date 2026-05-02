{ pkgs }:
{
  programs.chromium = {
    enable = pkgs.stdenv.isLinux;
    extensions = [
      { id = "gppongmhjkpfnbhagpmjfkannfbllamg"; } # Wappalyzer
      { id = "kfdibhbheajeacnkkakomaliggbgndcf"; } # Chromemacs
      { id = "gighmmpiobklfepjocnamgkkbiglidom"; } # Adblock
      { id = "hkgfoiooedgoejojocmhlaklaeopbecg"; } # Picture-in-Picture
      { id = "gfapcejdoghpoidkfodoiiffaaibpaem"; } # Dracula Chrome Theme
      { id = "cejijldbedfmdehondfmoadlkhgjcmkd"; } # Sheets Row Highlighter
    ];
  };
}
