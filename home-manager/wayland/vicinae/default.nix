{ config, lib, ... }:
let
  inherit (import ../../../shared/design.nix config.stylix.fonts) size radius;
in
{
  # Raycast-style launcher and clipboard history (Mod+D, Mod+V in niri).
  programs.vicinae = {
    enable = true;
    systemd = {
      enable = true;
      autoStart = true;
    };
    # Stylix sets the family and colors. The rest matches fuzzel and mako: the popup size and the
    # window radius, fontconfig rendering instead of Qt's distance fields, and the rail's 24-hour clock.
    settings = {
      font = {
        rendering = "native";
        normal.size = lib.mkForce size.popup;
      };
      launcher_window = {
        rounding = radius.window;
        clock.format = "HH:mm";
      };
    };
  };
  stylix.targets.vicinae.enable = true;
}
