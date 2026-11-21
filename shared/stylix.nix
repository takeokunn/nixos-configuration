# Palette and fonts shared by the NixOS and nix-darwin Stylix modules. Targets stay opt-in
# (autoEnable = false) so Stylix only themes apps whose hand-written colors it replaced.
{ pkgs }:
{
  enable = true;
  autoEnable = false;
  polarity = "dark";
  # Inline (from base16-schemes dracula.yaml) rather than a store path: reading a file out of a
  # package during evaluation is import-from-derivation, which CI forbids.
  base16Scheme = {
    scheme = "Dracula";
    author = "Jamy Golden, based on Dracula Theme";
    base00 = "282a36";
    base01 = "363447";
    base02 = "44475a";
    base03 = "6272a4";
    base04 = "9ea8c7";
    base05 = "f8f8f2";
    base06 = "f0f1f4";
    base07 = "ffffff";
    base08 = "ff5555";
    base09 = "ffb86c";
    base0A = "f1fa8c";
    base0B = "50fa7b";
    base0C = "8be9fd";
    base0D = "80bfff";
    base0E = "ff79c6";
    base0F = "bd93f9";
  };
  fonts = {
    sizes = {
      applications = 11;
      desktop = 11;
      popups = 12;
      terminal = 13;
    };
    monospace = {
      package = pkgs.udev-gothic-nf;
      name = "UDEV Gothic NF";
    };
    sansSerif = {
      package = pkgs.geist-font;
      name = "Geist";
    };
    serif = {
      package = pkgs.noto-fonts-cjk-serif;
      name = "Noto Serif CJK JP";
    };
    emoji = {
      package = pkgs.noto-fonts-color-emoji;
      name = "Noto Color Emoji";
    };
  };
}
