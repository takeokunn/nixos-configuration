{
  pkgs,
  org-babel,
  sources,
}:
let
  tangle = org-babel.lib.tangleOrgBabel { languages = [ "emacs-lisp" ]; };
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = builtins.toFile "empty.el" "";
      package = pkgs.emacs-git;
      extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
    };
  };

  home = {
    file = {
      ".emacs.d/init.el".text = tangle (builtins.readFile ./elisp/init.org);
      ".emacs.d/early-init.el".text = tangle (builtins.readFile ./elisp/early-init.org);
      ".emacs.d/misc/yasnippet.org".source = ./misc/yasnippet.org;
    };

    packages = with pkgs; [
      emacs-lsp-booster
      pinentry-emacs
    ];
  };
}
