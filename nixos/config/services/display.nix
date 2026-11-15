{ config, pkgs, ... }:
{
  services.greetd = {
    enable = true;
    settings.default_session = {
      command = "${pkgs.tuigreet}/bin/tuigreet --time --remember --asterisks --cmd ${config.programs.niri.package}/bin/niri-session";
      user = "greeter";
    };
  };

  # Read by the console (TTY and the initrd PIN prompt); niri carries its own copy of the same layout.
  services.xserver.xkb.layout = "jp";
  services.xserver.xkb.options = "ctrl:nocaps";
}
