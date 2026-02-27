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

  emacs-unstable =
    let
      base = pkgs.emacsWithPackagesFromUsePackage {
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
    in
    pkgs.symlinkJoin {
      name = base.name;
      paths = [ base ];
      passthru = base.passthru // { withPackages = base; };
      postBuild = lib.optionalString pkgs.stdenv.isDarwin ''
        EXEC="$out/Applications/Emacs.app/Contents/MacOS/Emacs"
        if [ -e "$EXEC" ] || [ -L "$EXEC" ]; then
          rm -f "$EXEC"
          printf '#!/bin/bash\nexec ${base}/bin/emacsclient -c -n "$@"\n' > "$EXEC"
          chmod +x "$EXEC"
        else
          echo "warning: Emacs.app not found in $out, skipping emacsclient wrapper" >&2
        fi
      '';
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
