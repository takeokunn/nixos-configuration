{ pkgs, ... }:
{
  programs.firefox.enable = pkgs.stdenv.hostPlatform.isLinux;
  # Stylix declares programs.firefox.profiles.default from this list and themes it.
  stylix.targets.firefox = {
    enable = true;
    profileNames = [ "default" ];
  };
  programs.firefox.policies = {
    DisableTelemetry = true;
    DisableFirefoxStudies = true;
    DisablePocket = true;
    DontCheckDefaultBrowser = true;
  };
}
