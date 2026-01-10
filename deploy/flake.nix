{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
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
      in
      {
        packages = {
          emacs-takeokunn-org = pkgs.stdenv.mkDerivation {
            name = "emacs-takeokunn-org";
            src = ../.;
            nativeBuildInputs = with pkgs; [
              (emacsPkg.pkgs.withPackages (epkgs: with epkgs; [ htmlize ]))
            ];
            buildPhase = ''
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
