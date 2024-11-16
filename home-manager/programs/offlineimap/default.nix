{
  programs.offlineimap = {
    enable = true;
    extraConfig = {
      general = {
        accounts = "Gmail";
        maxsyncaccounts = 1;
      };
    };
  };

  accounts.email.accounts = {
    Gmail = {
      primary = true;
      offlineimap = {
        enable = true;
        extraConfig.remote = {
          maxconnections = 2;
        };
      };
      flavor = "gmail.com";
      realName = "takeo obara";
      address = "bararararatty@gmail.com";
      passwordCommand = [
        "pass"
        "show"
        "private/develop/google/offlineimap"
      ];
      imap = {
        host = "imap.gmail.com";
        port = 993;
        tls.enable = true;
      };
    };
  };
}
