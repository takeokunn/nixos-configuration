{ pkgs }:
{
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-lgc-plus
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    noto-fonts-emoji-blob-bin
    noto-fonts-monochrome-emoji
    hackgen-font
    hackgen-nf-font
    fira-code-nerdfont
    emacs-all-the-icons-fonts
    font-awesome
    font-awesome_5
    iosevka
    (iosevka-bin.override { variant = "Aile"; })
    (iosevka-bin.override { variant = "Etoile"; })
  ];
}
