{ nurPkgs, ... }:
{
  imports = [
    ./emacs
    ./emacs-service
    ./emacs-scratchpad
  ];

  home.packages = [ nurPkgs.kakehashi ];
}
