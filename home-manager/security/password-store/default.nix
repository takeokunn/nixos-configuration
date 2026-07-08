{ pkgs, ... }:
{
  programs.password-store.enable = true;
  programs.password-store.package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
  programs.password-store.settings.PASSWORD_STORE_DIR = "$HOME/ghq/github.com/takeokunn/private/password-store/current";
  programs.password-store.settings.PASSWORD_STORE_ENABLE_EXTENSIONS = "true";
  programs.password-store.settings.PASSWORD_STORE_CLIP_TIME = "60";
}
