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

  i18n.inputMethod = {
    type = "fcitx5";
    fcitx5 = {
      waylandFrontend = true;
      addons = with pkgs; [
        fcitx5-mozc
        fcitx5-gtk
      ];
    };
  };
}
