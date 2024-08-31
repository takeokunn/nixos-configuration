{ pkgs, emacsPkgs, org-babel }:
let tangle = org-babel.lib.tangleOrgBabel { languages = [ "emacs-lisp" ]; };
in {
  programs.emacs = {
    enable = true;
    package = emacsPkgs;
  };

  home.file = {
    ".emacs.d/init.el".text =
      tangle (builtins.readFile ../../packages/elisp/init.org);
    ".emacs.d/early-init.el".text =
      tangle (builtins.readFile ../../packages/elisp/early-init.org);
  };

  home.packages = with pkgs; [ emacs-lsp-booster pinentry-emacs ];
}
