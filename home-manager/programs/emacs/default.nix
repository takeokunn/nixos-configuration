{ pkgs, org-babel }:
let tangle = org-babel.lib.tangleOrgBabel { languages = [ "emacs-lisp" ]; };
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./elisp/init.org;
      defaultInitFile = true;
      package = pkgs.emacs-git;
      alwaysTangle = true;
      extraEmacsPackages = import ./epkgs { inherit pkgs; };
    };
  };

  home.file = {
    ".config/emacs/init.el".text = tangle (builtins.readFile ./elisp/init.org);
    ".config/emacs/early-init.el".text =
      tangle (builtins.readFile ./elisp/early-init.org);
    ".config/emacs/yasnippet.org".source = ./yasnippet.org;
  };

  home.packages = with pkgs; [ emacs-lsp-booster pinentry-emacs cmigemo ];
}
