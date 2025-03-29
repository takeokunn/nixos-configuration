{
  lib,
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
    package = pkgs.emacs-git.overrideAttrs (old: {
      buildInputs =
        old.buildInputs
        ++ lib.optional pkgs.stdenv.isDarwin [ pkgs.darwin.apple_sdk.frameworks.WebKit ];
      configureFlags = old.configureFlags ++ [ "--with-xwidgets" ];
    });
    extraEmacsPackages = import ./epkgs { inherit pkgs sources; };
    override = override;
  };

  emacs-unstable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-unstable.overrideAttrs (old: {
      buildInputs =
        old.buildInputs
        ++ lib.optional pkgs.stdenv.isDarwin [ pkgs.darwin.apple_sdk.frameworks.WebKit ];
      configureFlags = old.configureFlags ++ [ "--with-xwidgets" ];
    });
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
