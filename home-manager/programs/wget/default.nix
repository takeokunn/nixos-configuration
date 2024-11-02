{ pkgs }:
{
  programs.wget = {
    enable = true;
    checkCertificate = "off";
    timestamping = "on";
    noParent = "on";
    timeout = 60;
    tries = 3;
    retryConnrefused = "on";
    trustServerNames = "on";
    followFtp = "on";
    adjustExtension = "on";
    localEncoding = "UTF-8";
    robots = "off";
    serverResponse = "on";
  };
}
