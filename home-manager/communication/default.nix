{ pkgs, ... }:
{
  programs.discord = {
    enable = true;
    settings = {
      SKIP_HOST_UPDATE = true;
      OPEN_ON_STARTUP = false;
    };
  };

  home.packages = pkgs.lib.optionals (!pkgs.stdenv.isDarwin) (
    with pkgs;
    [
      slack
    ]
  );
}
