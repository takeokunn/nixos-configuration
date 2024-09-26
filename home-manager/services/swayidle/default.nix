{ pkgs }: {
  services.swayidle = {
    # enable = pkgs.stdenv.isLinux;
    enable = false;
    timeouts = [
      {
        timeout = 60;
        command = "${pkgs.swaylock}/bin/swaylock -fF";
      }
      {
        timeout = 90;
        command = "${pkgs.systemd}/bin/systemctl suspend";
      }
    ];
  };
}
