{ pkgs, org-babel, sources }:
let tangle = org-babel.lib.tangleOrgBabel { languages = [ "emacs-lisp" ]; };
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./elisp/init.org;
      defaultInitFile = true;
      package = pkgs.emacs-git;
      alwaysTangle = true;
      extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
    };
  };

  home = {
    file = {
      ".emacs.d/init.el".text = tangle (builtins.readFile ./elisp/init.org);
      ".emacs.d/early-init.el".text =
        tangle (builtins.readFile ./elisp/early-init.org);
      ".emacs.d/misc/yasnippet.org".source = ./misc/yasnippet.org;
    };

    packages = with pkgs; [ emacs-lsp-booster pinentry-emacs ];

    sessionVariables = {
      EDITOR = "emacsclient -nw";
      HOMEBREW_EDITOR = "emacsclient -nw";
    };
  };
}
