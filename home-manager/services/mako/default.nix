{ pkgs }:
{
  services.mako = {
    enable = pkgs.stdenv.isLinux;
    extraConfig = ''
      background-color=#282a36
      text-color=#f8f8f2
      border-color=#282a36

      [urgency=low]
      border-color=#282a36

      [urgency=normal]
      border-color=#f1fa8c

      [urgency=high]
      border-color=#ff5555
    '';
  };
}
