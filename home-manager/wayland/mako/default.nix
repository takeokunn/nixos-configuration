{ config, lib, ... }:
let
  palette = config.lib.stylix.colors;
  colors = palette.withHashtag;
  # Same alpha Stylix gives the base background-color.
  opacity = lib.toHexString (builtins.ceil (config.stylix.opacity.popups * 255));
  # base00 with 12% of the app's color mixed in.
  tinted =
    base:
    let
      channel =
        c:
        lib.fixedWidthString 2 "0" (
          lib.toHexString (
            builtins.floor (
              lib.toInt palette."base00-rgb-${c}" * 0.88 + lib.toInt palette."${base}-rgb-${c}" * 0.12 + 0.5
            )
          )
        );
    in
    "#${channel "r"}${channel "g"}${channel "b"}${opacity}";
  # mako fills the whole notification with the progress, so it stays translucent over the text.
  appSection = app: base: ''
    [app-name=${app}]
    border-color=${colors.${base}}
    background-color=${tinted base}
    progress-color=over ${colors.${base}}40
  '';
in
{
  services.mako.enable = true;
  # Stylix sets the font, the base colors and the [urgency=low|critical] sections; extraConfig is
  # written after them, so its criteria below win.
  stylix.targets.mako.enable = true;
  services.mako.settings = {
    width = 356;
    height = 150;
    # niri's layer shadow covers the whole surface, so the gap to the screen edge is a
    # layer-shell margin rather than transparent space inside the surface.
    margin = "0";
    outer-margin = "10";
    padding = "14,16";
    border-size = 2;
    border-radius = 14;

    default-timeout = 5000;
    layer = "overlay";
    anchor = "top-right";
    sort = "-time";
    max-visible = 5;
    icons = true;
    max-icon-size = 40;
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

    ${appSection "discord" "base0C"}
    ${appSection "google-chrome" "base0B"}
    ${appSection "spotify" "base0B"}
    ${appSection "slack" "base0E"}
    # Repeated after the app sections so a critical notification keeps its red border.
    [urgency=critical]
    border-color=${colors.base08}

    [mode=do-not-disturb]
    invisible=1
  '';
}
