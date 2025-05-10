{ pkgs }:
{
  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
    settings = {
      PASSWORD_STORE_DIR = "$HOME/ghq/github.com/takeokunn/private/password-store/current";
      PASSWORD_STORE_ENABLE_EXTENSIONS = "true";
      PASSWORD_STORE_CLIP_TIME = "60";
    };
  };
}
