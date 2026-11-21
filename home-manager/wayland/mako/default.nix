{ config, ... }:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  services.mako.enable = true;
  # Stylix sets the font, the base colors and the [urgency=low|critical] sections; extraConfig is
  # written after them, so its criteria below win.
  stylix.targets.mako.enable = true;
  services.mako.settings = {
    width = 350;
    height = 150;
    margin = "10";
    padding = "12";
    border-size = 2;
    border-radius = 8;

    default-timeout = 5000;
    layer = "overlay";
    anchor = "top-right";
    sort = "-time";
    max-visible = 5;
    icons = true;
    max-icon-size = 48;
    markup = true;
    actions = true;
  };

  services.mako.extraConfig = ''
    [urgency=low]
    border-color=${colors.base03}
    default-timeout=3000

    [urgency=normal]
    border-color=${colors.base0F}
    default-timeout=5000

    [urgency=critical]
    border-color=${colors.base08}
    default-timeout=0
    ignore-timeout=1

    [app-name=spotify]
    border-color=${colors.base0B}

    [app-name=discord]
    border-color=${colors.base0C}

    [mode=do-not-disturb]
    invisible=1
  '';
}
