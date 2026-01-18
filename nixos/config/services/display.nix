{ username }:
{
  services = {
    xserver = {
      enable = true;
      xkb.layout = "jp";
    };

    displayManager = {
      gdm.enable = true;
      autoLogin = {
        enable = true;
        user = username;
      };
    };
  };
}
