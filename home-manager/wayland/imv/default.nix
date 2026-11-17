{ config, lib, ... }:
let
  colors = config.lib.stylix.colors;
in
{
  programs.imv.enable = true;
  # Stylix has no imv target.
  programs.imv.settings.options = {
    background = colors.base00;
    overlay_text_color = colors.base05;
    overlay_background_color = colors.base02;
  };

  xdg.mimeApps.defaultApplications = lib.genAttrs [
    "image/png"
    "image/jpeg"
    "image/gif"
    "image/webp"
    "image/bmp"
    "image/tiff"
    "image/svg+xml"
  ] (_: [ "imv.desktop" ]);
}
