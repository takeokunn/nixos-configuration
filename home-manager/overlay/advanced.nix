{ emacs-overlay }:
[
  (import emacs-overlay)
  (final: prev: {
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
    pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
      (_: python-prev: {
        mcp = python-prev.mcp.overrideAttrs (_: {
          # Skip the broken postPatch from nixpkgs - upstream mcp v1.25.0 has
          # removed time.sleep(0.1) from several test files that nixpkgs tries to patch
          postPatch = "";
        });
      })
    ];
    arto = prev.stdenv.mkDerivation rec {
      pname = "arto";
      version = "0.3.3";
      src = prev.fetchurl {
        url = "https://github.com/lambdalisue/rs-arto/releases/download/v${version}/Arto_${version}_aarch64.dmg";
        sha256 = "0wsla36r96viafqp97i98pwfkn9jjfmm3r3pi1xcz73faj6r2ac8";
      };
      nativeBuildInputs = [ prev.undmg ];
      sourceRoot = ".";
      installPhase = ''
        mkdir -p $out/Applications
        cp -r Arto.app $out/Applications/
      '';
      meta = {
        description = "The Art of Reading Markdown";
        homepage = "https://github.com/lambdalisue/rs-arto";
        platforms = [ "aarch64-darwin" ];
      };
    };
  })
]
