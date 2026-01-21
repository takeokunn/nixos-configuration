{
  lib,
  pkgs,
  nurPkgs,
}:
let
  override = _: _: {
  };

  parallelBuildAttrs = {
    enableParallelBuilding = true;
    env = {
      NATIVE_COMP_JOBS = "0";
    };
  };
in
{
  emacs-git = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-git.overrideAttrs (
      old:
      parallelBuildAttrs
      // {
        buildInputs = old.buildInputs ++ lib.optional pkgs.stdenv.isDarwin [ pkgs.apple-sdk ];
        configureFlags = old.configureFlags ++ [ "--with-xwidgets" ];
        env = (old.env or { }) // parallelBuildAttrs.env;
      }
    );
    extraEmacsPackages = import ./epkgs { inherit pkgs nurPkgs; };
    override = override;
  };

  emacs-unstable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-unstable.overrideAttrs (
      old:
      parallelBuildAttrs
      // {
        env = (old.env or { }) // parallelBuildAttrs.env;
      }
    );
    extraEmacsPackages = import ./epkgs { inherit pkgs nurPkgs; };
    override = override;
  };

  emacs-unstable-with-widgets = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-unstable.overrideAttrs (
      old:
      parallelBuildAttrs
      // {
        buildInputs = old.buildInputs ++ lib.optional pkgs.stdenv.isDarwin [ pkgs.apple-sdk ];
        configureFlags = old.configureFlags ++ [ "--with-xwidgets" ];
        env = (old.env or { }) // parallelBuildAttrs.env;
      }
    );
    extraEmacsPackages = import ./epkgs { inherit pkgs nurPkgs; };
    override = override;
  };

  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs.overrideAttrs (
      old:
      parallelBuildAttrs
      // {
        env = (old.env or { }) // parallelBuildAttrs.env;
      }
    );
    extraEmacsPackages = import ./epkgs { inherit pkgs nurPkgs; };
    override = override;
  };

  emacs-unstable-pgtk = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-unstable-pgtk.overrideAttrs (
      old:
      parallelBuildAttrs
      // {
        env = (old.env or { }) // parallelBuildAttrs.env;
      }
    );
    extraEmacsPackages = import ./epkgs { inherit pkgs nurPkgs; };
    override = override;
  };
}
