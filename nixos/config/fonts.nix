{ pkgs, ... }:
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
      sansSerif = [
        "Geist"
        "Noto Sans CJK JP"
      ];
      serif = [
        "Noto Serif"
        "Noto Serif CJK JP"
      ];
      monospace = [
        "UDEV Gothic NF"
        "Noto Sans Mono CJK JP"
      ];
      emoji = [ "Noto Color Emoji" ];
    };
  };
}
