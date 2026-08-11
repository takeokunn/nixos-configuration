{ pkgs, ... }:
{
  programs.password-store.enable = true;
  programs.password-store.package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
  # The repository is bare, so its working files live in a worktree. This one is
  # named `main' rather than being timestamped precisely so a static setting can
  # name it; nix has no way to resolve the path at evaluation time.
  programs.password-store.settings.PASSWORD_STORE_DIR = "$HOME/ghq/github.com/takeokunn/private.git/.worktrees/main/password-store/current";
  programs.password-store.settings.PASSWORD_STORE_ENABLE_EXTENSIONS = "true";
  programs.password-store.settings.PASSWORD_STORE_CLIP_TIME = "60";
}
