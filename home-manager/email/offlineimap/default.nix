{
  programs.offlineimap.enable = true;
  programs.offlineimap.extraConfig.general.accounts = "Gmail";
  programs.offlineimap.extraConfig.general.maxsyncaccounts = 1;

  accounts.email.accounts.Gmail.offlineimap.enable = true;
  accounts.email.accounts.Gmail.offlineimap.extraConfig.remote.maxconnections = 10;
  accounts.email.accounts.Gmail.passwordCommand = [
    "pass"
    "show"
    "private/develop/google/offlineimap"
  ];
  accounts.email.accounts.Gmail.imap.tls.enable = true;
}
