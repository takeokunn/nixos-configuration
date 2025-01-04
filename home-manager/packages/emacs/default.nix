{
  pkgs,
  sources,
}:
pkgs.emacsWithPackagesFromUsePackage {
  config = builtins.toFile "empty.el" "";
  package = pkgs.emacs-git;
  extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
}
