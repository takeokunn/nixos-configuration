{ self, nixpkgs, nix-on-droid, emacs-overlay }:
let system = "x86_64";
in {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    modules = [ ../../nix-on-droid ];
    extraSpecialArgs = { inherit system nixpkgs emacs-overlay; };
  };
}
