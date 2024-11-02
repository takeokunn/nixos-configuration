{ inputs }:
let
  inherit (inputs) nixpkgs nix-on-droid;
  system = "aarch64-linux";
in
nix-on-droid.lib.nixOnDroidConfiguration {
  modules = [ ../../nix-on-droid ];
  pkgs = import nixpkgs { inherit system; };
  extraSpecialArgs = {
    inherit nixpkgs system;
  };
}
