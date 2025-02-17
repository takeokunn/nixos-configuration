{
  programs.offlineimap = {
    enable = true;
    extraConfig.general = {
      accounts = "Gmail";
      maxsyncaccounts = 1;
    };
  };

  accounts.email.accounts = {
    Gmail = {
      offlineimap = {
        enable = true;
        extraConfig.remote = {
          maxconnections = 10;
        };
      };
      passwordCommand = [
        "pass"
        "show"
        "private/develop/google/offlineimap"
      ];
      imap.tls.enable = true;
    };
  };
}
