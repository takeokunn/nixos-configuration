{ nurPkgs, ... }:
{
  imports = [
    ./emacs
    ./emacs-service
    ./copilot-language-server
  ];

  home.packages = [ nurPkgs.kakehashi ];
}
