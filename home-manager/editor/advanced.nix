{ nurPkgs, ... }:
{
  imports = [
    ./emacs
    ./emacs-service
    ./emacs-scratchpad
    ./copilot-language-server
  ];

  home.packages = [ nurPkgs.kakehashi ];
}
