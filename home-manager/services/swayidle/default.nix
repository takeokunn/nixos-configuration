{ pkgs }:
{
  services.swayidle = {
    enable = pkgs.stdenv.isLinux;
    timeouts = [
      {
        timeout = 60;
        command = "${pkgs.swaylock}/bin/swaylock -fF";
      }
    ];
  };
}
