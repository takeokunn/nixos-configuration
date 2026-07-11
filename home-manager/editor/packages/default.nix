{
  lib,
  pkgs,
  nurPkgs,
}:
let
  override = _: super: {
    # copilot-0.5.0 bundles an old copilot-chat.el that shadows the
    # standalone copilot-chat package in load-path, causing autoloads like
    # copilot-chat-insert-commit-message to fail at runtime.
    copilot = super.copilot.overrideAttrs (old: {
      postInstall = (old.postInstall or "") + ''
        rm -f $out/share/emacs/site-lisp/elpa/copilot-*/copilot-chat.el
        rm -f $out/share/emacs/site-lisp/elpa/copilot-*/copilot-chat.elc
      '';
    });
    # projectile ≥ 20260627 requires consult at byte-compile time but the
    # generated nixpkgs derivation does not yet declare it as a dependency.
    projectile = super.projectile.overrideAttrs (old: {
      propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [ super.consult ];
    });
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
        buildInputs = old.buildInputs ++ lib.optionals pkgs.stdenv.isDarwin [ pkgs.apple-sdk ];
        configureFlags = old.configureFlags ++ [ "--with-xwidgets" ];
        env = (old.env or { }) // parallelBuildAttrs.env;
      }
    );
    extraEmacsPackages = import ./epkgs { inherit nurPkgs; };
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
            buildInputs = old.buildInputs ++ lib.optionals pkgs.stdenv.isDarwin [ pkgs.apple-sdk ];
            # On Darwin 25.x (macOS 16), ObjC files (.m) fail to compile because
            # the compiler defaults to a pre-C11 standard where `bool` and `alignof`
            # are unknown. Force gnu11 standard and include stdbool.h explicitly.
            env = (old.env or { }) // parallelBuildAttrs.env // lib.optionalAttrs pkgs.stdenv.isDarwin {
              NIX_CFLAGS_COMPILE = "${(old.env or { }).NIX_CFLAGS_COMPILE or ""} -std=gnu11 -include stdbool.h";
            };
          }
        );
        extraEmacsPackages = import ./epkgs { inherit nurPkgs; };
        override = override;
      };
    in
    pkgs.symlinkJoin {
      name = base.name;
      paths = [ base ];
      passthru = base.passthru // {
        withPackages = base;
      };
      postBuild = lib.optionalString pkgs.stdenv.isDarwin ''
        EXEC="$out/Applications/Emacs.app/Contents/MacOS/Emacs"
        if [ -e "$EXEC" ] || [ -L "$EXEC" ]; then
          rm -f "$EXEC"
          printf '#!/bin/bash\nexec ${base}/bin/emacsclient -c -n -s "/tmp/emacs$(id -u)/server" "$@"\n' > "$EXEC"
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
        buildInputs = old.buildInputs ++ lib.optionals pkgs.stdenv.isDarwin [ pkgs.apple-sdk ];
        configureFlags = old.configureFlags ++ [ "--with-xwidgets" ];
        env = (old.env or { }) // parallelBuildAttrs.env;
      }
    );
    extraEmacsPackages = import ./epkgs { inherit nurPkgs; };
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
    extraEmacsPackages = import ./epkgs { inherit nurPkgs; };
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
    extraEmacsPackages = import ./epkgs { inherit nurPkgs; };
    override = override;
  };
}
