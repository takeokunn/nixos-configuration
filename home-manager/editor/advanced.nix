{
  lib,
  pkgs,
  emacsPkg,
  org-babel,
  llmAgentsPkgs,
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
  emacs-service = import ./emacs-service { inherit pkgs emacsPkg; };
  copilot-language-server = import ./copilot-language-server { inherit pkgs llmAgentsPkgs; };
in
[
  emacs
  emacs-service
  copilot-language-server
]
