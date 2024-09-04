{ pkgs }:
let
  ddu = pkgs.callPackage ./nixpkgs/ddu.nix;
  ddu-ui-ff = pkgs.callPackage ./nixpkgs/ddu-ui-ff.nix;
  ddu-ui-filer = pkgs.callPackage ./nixpkgs/ddu-ui-filer.nix;
  ddu-kind-file = pkgs.callPackage ./nixpkgs/ddu-kind-file.nix;
  ddu-kind-word = pkgs.callPackage ./nixpkgs/ddu-kind-word.nix;
  ddu-source-file = pkgs.callPackage ./nixpkgs/ddu-source-file.nix;
  ddu-source-file_rec = pkgs.callPackage ./nixpkgs/ddu-source-file_rec.nix;
  ddu-source-line = pkgs.callPackage ./nixpkgs/ddu-source-line.nix;
  ddu-filter-matcher_substring =
    pkgs.callPackage ./nixpkgs/ddu-filter-matcher_substring.nix;
  ddu-filter-matcher_hidden =
    pkgs.callPackage ./nixpkgs/ddu-filter-matcher_hidden.nix;
  ddu-filter-matcher_relative =
    pkgs.callPackage ./nixpkgs/ddu-filter-matcher_relative.nix;
  ddu-source-path_history =
    pkgs.callPackage ./nixpkgs/ddu-source-path_history.nix;
in [
  ddu
  ddu-ui-ff
  ddu-ui-filer
  ddu-kind-file
  ddu-kind-word
  ddu-source-file
  ddu-source-file_rec
  ddu-source-line
  ddu-filter-matcher_substring
  ddu-filter-matcher_hidden
  ddu-filter-matcher_relative
  ddu-source-path_history
]
