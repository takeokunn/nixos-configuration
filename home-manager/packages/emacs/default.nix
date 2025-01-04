{
  pkgs,
  sources,
}:
{
  emacs-git = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-git;
    extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
  };

  emacs-unstable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-unstable;
    extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
  };

  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs;
    extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
  };
}
