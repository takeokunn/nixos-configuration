{ pkgs, ... }:
{
  programs.firefox.enable = pkgs.stdenv.hostPlatform.isLinux;
  programs.firefox.policies = {
    DisableTelemetry = true;
    DisableFirefoxStudies = true;
    DisablePocket = true;
    DontCheckDefaultBrowser = true;
  };
}
