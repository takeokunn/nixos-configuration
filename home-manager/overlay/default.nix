{ emacs-overlay }:
let
  lnav-overlay = (self: super: {
    lnav = super.lnav.overrideAttrs (oldAttrs: {
      buildInputs = with self; [
        bzip2
        ncurses
        pcre2
        readline
        sqlite
        curl
        libarchive
      ];
    });
  });

in
[
  (import emacs-overlay)
  lnav-overlay
]
