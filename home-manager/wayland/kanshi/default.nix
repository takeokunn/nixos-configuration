{
  services.kanshi.enable = true;
  services.kanshi.settings = [
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
}
