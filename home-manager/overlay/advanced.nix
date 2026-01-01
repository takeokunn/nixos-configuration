{ emacs-overlay }:
[
  (import emacs-overlay)
  (final: prev: {
    pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
      (python-final: python-prev: {
        mcp = python-prev.mcp.overrideAttrs (oldAttrs: {
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
