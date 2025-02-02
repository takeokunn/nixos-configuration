{
  pkgs,
  sources,
}:
let
  override = final: prev: {
    dap-mode = prev.melpaPackages.dap-mode.overrideAttrs (old: {
      preBuild = null;
    });
  };
in
{
  emacs-git = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-git;
    extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
    override = override;
  };

  emacs-unstable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-unstable;
    extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
    override = override;
  };

  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs;
    extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
    override = override;
  };
}
