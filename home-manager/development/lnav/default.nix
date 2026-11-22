{
  config,
  lib,
  options,
  ...
}:
let
  hasStylix = options ? stylix;
in
{
  programs.lnav.enable = true;
  # Without Stylix (Android, or this module used on its own) lnav keeps its built-in Dracula theme.
  programs.lnav.config.ui = {
    theme = if hasStylix then "stylix" else "dracula";
  }
  // lib.optionalAttrs hasStylix {
    theme-defs.stylix = import ./stylix-theme.nix config.lib.stylix.colors.withHashtag;
  };
  xdg.configFile."lnav/formats/installed/".source = ./formats;
  xdg.configFile."lnav/formats/installed/".recursive = true;
}
