{ pkgs }:
{
  services.swayidle = {
    enable = pkgs.stdenv.isLinux;
    timeouts = [
      {
        timeout = 900;
        command = "${pkgs.swaylock}/bin/swaylock -fF";
      }
    ];
  };
}
