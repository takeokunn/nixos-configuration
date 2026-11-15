{
  # Writes managed policies for both Chromium and Google Chrome (/etc/opt/chrome). Chrome on Linux
  # only installs extensions from these system policies, not from home-manager.
  programs.chromium = {
    enable = true;
    extensions = [
      "gppongmhjkpfnbhagpmjfkannfbllamg" # Wappalyzer
      "kfdibhbheajeacnkkakomaliggbgndcf" # Chromemacs
      "gighmmpiobklfepjocnamgkkbiglidom" # Adblock
      "hkgfoiooedgoejojocmhlaklaeopbecg" # Picture-in-Picture
      "gfapcejdoghpoidkfodoiiffaaibpaem" # Dracula Chrome Theme
      "cejijldbedfmdehondfmoadlkhgjcmkd" # Sheets Row Highlighter
    ];
    extraOpts = {
      MetricsReportingEnabled = false;
      UrlKeyedAnonymizedDataCollectionEnabled = false;
      BrowserSignin = 1;
    };
  };
}
