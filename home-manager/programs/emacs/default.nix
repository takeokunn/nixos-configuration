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
    ".emacs.d/init.el".text = tangle (builtins.readFile ./elisp/init.org);
    ".emacs.d/early-init.el".text =
      tangle (builtins.readFile ./elisp/early-init.org);
  };

  home.packages = with pkgs; [ emacs-lsp-booster pinentry-emacs ];

  programs.fish = {
    # shellAliases = { emacs = "emacs -nw"; };

    interactiveShellInit = ''
      # set -gx LSP_USE_PLISTS true
      # set -gx EDITOR "emacs -nw"
      # set -gx HOMEBREW_EDITOR "emacs -nw"
    '';
  };
}
