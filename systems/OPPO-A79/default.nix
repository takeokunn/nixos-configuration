{ self, nixpkgs, home-manager, emacs-overlay, nix-on-droid }:
let system = "aarch64-linux";
in {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    modules = [ ../../nix-on-droid ];
    pkgs = import nixpkgs { inherit system; };
    extraSpecialArgs = { inherit nixpkgs emacs-overlay; };
  };
}
