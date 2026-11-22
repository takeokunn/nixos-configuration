{ config, pkgs, ... }:
let
  stylixFonts = config.stylix.fonts;
in
{
  fonts.fontDir.enable = true;
  fonts.packages = import ../../shared/fonts.nix pkgs ++ [ pkgs.nerd-fonts.symbols-only ];

  fonts.fontconfig = {
    # Grayscale antialiasing without hinting, closest to macOS rendering.
    antialias = true;
    hinting.enable = false;
    subpixel.rgba = "none";
    subpixel.lcdfilter = "none";
    # Japanese first, so shared Han characters never fall back to Chinese glyph shapes.
    defaultFonts = {
      # The Stylix fonts first, so every app resolves the same families Stylix configures.
      sansSerif = [
        stylixFonts.sansSerif.name
        "Noto Sans CJK JP"
      ];
      serif = [ stylixFonts.serif.name ];
      monospace = [
        stylixFonts.monospace.name
        "Noto Sans Mono CJK JP"
      ];
      emoji = [ stylixFonts.emoji.name ];
    };
  };
}
