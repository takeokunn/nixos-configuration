{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nur-packages = {
      url = "github:takeokunn/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      nur-packages,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        emacsPkg = pkgs.emacs.override {
          withNativeCompilation = false;
        };
        readthezero = nur-packages.packages.${system}.readthezero;
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
              perl -pi -e "s|https://takeokunn.github.io/readthezero/readthezero-dracula.setup|${readthezero}/readthezero-dracula.setup|g" \
                home-manager/programs/emacs/elisp/init.org \
                home-manager/programs/emacs/elisp/early-init.org
              emacs --batch --load deploy/script.el --funcall export-org-files
              cp ${readthezero}/readthezero-base.css public/
              cp ${readthezero}/readthezero-theme-dracula.css public/
              cp ${readthezero}/readthezero.js public/
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
