{ username }:
{
  services.xserver.enable = true;
  services.xserver.xkb.layout = "jp";

  services.displayManager.gdm.enable = true;
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = username;
}
