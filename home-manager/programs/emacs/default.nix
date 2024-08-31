{ pkgs }: {
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
    ".emacs.d/init.el".source = ./elisp/init.el;
    ".emacs.d/early-init.el".source = ./elisp/early-init.el;
  };

  home.packages = with pkgs; [ emacs-lsp-booster pinentry-emacs ];

  programs.fish = {
    shellAliases = { emacs = "emacs -nw"; };

    interactiveShellInit = ''
      # set -gx LSP_USE_PLISTS true
      set -gx EDITOR "emacs -nw"
      set -gx HOMEBREW_EDITOR "emacs -nw"
    '';
  };
}
