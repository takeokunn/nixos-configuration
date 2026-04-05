{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        emacsPkg = pkgs.emacs.override {
          withNativeCompilation = false;
        };
        readthezeroSetup = pkgs.fetchurl {
          url = "https://takeokunn.github.io/readthezero/readthezero-dracula.setup";
          hash = "sha256-XJJXWu8YnYorhvb4iIoqztMOWpCjKk1HnqNaDNgXl4A=";
        };
      in
      {
        packages = {
          emacs-takeokunn-org = pkgs.stdenv.mkDerivation {
            name = "emacs-takeokunn-org";
            src = ../.;
            nativeBuildInputs = with pkgs; [
              (emacsPkg.pkgs.withPackages (epkgs: with epkgs; [ htmlize ]))
              perl
            ];
            buildPhase = ''
              perl -pi -e "s|https://takeokunn.github.io/readthezero/readthezero-dracula.setup|${readthezeroSetup}|g" \
                home-manager/programs/emacs/elisp/init.org \
                home-manager/programs/emacs/elisp/early-init.org
              emacs --batch --load deploy/script.el --funcall export-org-files
            '';
            installPhase = ''
              mv public/init.html public/index.html
              cp -r public/ $out/
            '';
          };
        };
      }
    );
}
