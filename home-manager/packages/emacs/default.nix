{ pkgs }:
pkgs.emacsWithPackagesFromUsePackage {
  config = ./elisp/init.org;
  defaultInitFile = true;
  package = pkgs.emacs-git;
  alwaysTangle = true;
  extraEmacsPackages = import ./epkgs { inherit pkgs; };
}
