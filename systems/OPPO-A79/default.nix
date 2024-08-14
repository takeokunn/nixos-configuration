{ self, nixpkgs, emacs-overlay, nix-on-droid }:
let system = "aarch64-linux";
in {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    modules = [ ../../nix-on-droid ];
    pkgs = import nixpkgs { inherit system; };
    extraSpecialArgs = { inherit nixpkgs system emacs-overlay; };
  };
}
