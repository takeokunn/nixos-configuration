{ pkgs, username }:
{
  services.xremap = {
    enable = true;
    userName = username;
    serviceMode = "system";
    config = {
      modmap = [
        {
          name = "CapsLock to Ctrl";
          remap = {
            CapsLock = "Ctrl_L";
          };
        }
      ];
    };
  };

}
