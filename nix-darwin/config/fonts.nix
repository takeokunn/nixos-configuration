{ pkgs }:
{
  fonts.packages = with pkgs; [
    monaspace
    noto-fonts
    noto-fonts-lgc-plus
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    noto-fonts-emoji-blob-bin
    noto-fonts-monochrome-emoji
    hackgen-font
    hackgen-nf-font
    nerd-fonts.fira-code
    emacs-all-the-icons-fonts
    font-awesome
    font-awesome_5
    iosevka
    (iosevka-bin.override { variant = "Aile"; })
    (iosevka-bin.override { variant = "Etoile"; })
  ];
}
