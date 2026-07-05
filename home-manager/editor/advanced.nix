{
  lib,
  pkgs,
  emacsPkg,
  org-babel,
  llmAgentsPkgs,
  nurPkgs,
}:
let
  emacs = import ./emacs {
    inherit
      lib
      pkgs
      emacsPkg
      org-babel
      ;
  };
  emacs-service = import ./emacs-service { inherit pkgs emacsPkg nurPkgs; };
  copilot-language-server = import ./copilot-language-server { inherit llmAgentsPkgs; };
in
[
  emacs
  emacs-service
  copilot-language-server
  { home.packages = [ nurPkgs.kakehashi ]; }
]
