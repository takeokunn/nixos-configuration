{ pkgs, ... }:
{
  fonts.fontDir.enable = true;
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-lgc-plus
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    hackgen-font
    hackgen-nf-font
    nerd-fonts.fira-code
    nerd-fonts.symbols-only
    emacs-all-the-icons-fonts
    font-awesome
    font-awesome_5
  ];

  fonts.fontconfig = {
    # Grayscale antialiasing without hinting, closest to macOS rendering.
    antialias = true;
    hinting.enable = false;
    subpixel.rgba = "none";
    subpixel.lcdfilter = "none";
    # Japanese first, so shared Han characters never fall back to Chinese glyph shapes.
    defaultFonts = {
      sansSerif = [
        "Noto Sans"
        "Noto Sans CJK JP"
      ];
      serif = [
        "Noto Serif"
        "Noto Serif CJK JP"
      ];
      monospace = [
        "HackGen Console NF"
        "Noto Sans Mono CJK JP"
      ];
      emoji = [ "Noto Color Emoji" ];
    };
  };
}
