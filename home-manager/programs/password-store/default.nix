{ pkgs }: {
  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
    settings = {
      PASSWORD_STORE_DIR = "~/ghq/github.com/takeokunn/password-store";
      PASSWORD_STORE_ENABLE_EXTENSIONS = "true";
    };
  };
}
