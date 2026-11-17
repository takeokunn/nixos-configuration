# The Emacs build and its helper library. home-manager installs it (advanced.nix), and the nix-darwin
# host needs the same build for the AeroSpace scratchpad binding.
{ pkgs, nur-packages }:
let
  nurPkgs = import "${nur-packages}" {
    inherit pkgs;
    emacsPackages = pkgs.emacsPackagesFor pkgs.emacs-unstable;
  };
  emacsPkgSet = import ./packages {
    inherit (pkgs) lib;
    inherit pkgs nurPkgs;
  };
  emacsPkg =
    if pkgs.stdenv.hostPlatform.isDarwin then
      emacsPkgSet.emacs-unstable
    else
      emacsPkgSet.emacs-unstable-pgtk;
in
{
  inherit nurPkgs emacsPkg;
  emacsLib = import ./lib/emacs.nix { inherit pkgs emacsPkg; };
}
