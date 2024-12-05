{ pkgs }: {
  programs.msmtp = {
    enable = true;
    extraConfig = ''
      defaults
      auth on
      tls on
    '';
  };

  accounts.email.accounts = {
    SmtpGmail = {
      primary = true;
      msmtp.enable = true;
      realName = "takeo obara";
      userName = "takeo obara";
      address = "bararararatty@gmail.com";
      passwordCommand = [
        "pass"
        "show"
        "private/develop/google/smtp"
      ];
      smtp = {
        host = "smtp.gmail.com";
        tls.enable = true;
      };
    };
  };
}
