{ emacs-overlay }:
[
  (import emacs-overlay)
  (final: prev: {
    # direnv 2.37.1 GNUmakefile passes -linkmode=external but CGO is not enabled in the nix sandbox
    # Removing -linkmode=external falls back to internal linking which works fine on darwin
    direnv = prev.direnv.overrideAttrs (old: {
      postPatch = (old.postPatch or "") + ''
        substituteInPlace GNUmakefile --replace-fail '-linkmode=external' ""
      '';
    });

    # ECL 24.5.10 doesn't build with C23/clang - use develop branch with full C23 fixes
    # See: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/775
    # and: https://bugs.debian.org/1115924
    ecl = prev.ecl.overrideAttrs (_: {
      version = "24.5.10-unstable-2026-01-03";
      src = prev.fetchFromGitLab {
        owner = "embeddable-common-lisp";
        repo = "ecl";
        rev = "95566d1380b1ee9af07ce0f63061643f2b424e44";
        hash = "sha256-ZeztkSdCkWq3PfHj2pnFjJSIK42m4w8DJigZiyWKdas=";
      };
      # Remove patches that are already included in develop branch
      patches = [ ];
    });
    # Override SBCL to use the fixed ECL for bootstrapping
    sbcl = prev.sbcl.override { ecl = final.ecl; };
  })
]
