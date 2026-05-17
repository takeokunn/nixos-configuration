{ username }:
{
  services.xremap.enable = true;
  services.xremap.userName = username;
  services.xremap.serviceMode = "system";
  services.xremap.config.modmap = [
    {
      name = "CapsLock to Ctrl";
      remap.CapsLock = "Ctrl_L";
    }
  ];
}
