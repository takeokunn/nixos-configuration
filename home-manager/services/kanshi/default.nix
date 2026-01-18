{ pkgs }:
{
  services.kanshi = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    systemdTarget = "graphical-session.target";

    settings = [
      # Laptop only (ThinkPad X13 Gen2)
      {
        profile.name = "laptop";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "enable";
            mode = "1920x1200@60Hz";
            position = "0,0";
            scale = 1.0;
          }
        ];
      }

      # External monitor only (docked, lid closed)
      {
        profile.name = "docked";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = "*";
            status = "enable";
            position = "0,0";
            scale = 1.0;
          }
        ];
      }

      # Laptop + External monitor (extended)
      {
        profile.name = "dual";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "enable";
            mode = "1920x1200@60Hz";
            position = "0,0";
            scale = 1.0;
          }
          {
            criteria = "*";
            status = "enable";
            position = "1920,0";
            scale = 1.0;
          }
        ];
      }

      # Laptop + External monitor (mirror)
      {
        profile.name = "mirror";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "enable";
            mode = "1920x1080@60Hz";
            position = "0,0";
            scale = 1.0;
          }
          {
            criteria = "*";
            status = "enable";
            mode = "1920x1080@60Hz";
            position = "0,0";
            scale = 1.0;
          }
        ];
      }
    ];
  };
}
