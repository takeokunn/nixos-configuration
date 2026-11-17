{
  services.kanshi.enable = true;
  # Every profile with an external output matches the same set of outputs, and kanshi applies the
  # first match, so "dual" comes before "docked". Switch by hand with `kanshictl switch <name>`.
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
  ];
}
