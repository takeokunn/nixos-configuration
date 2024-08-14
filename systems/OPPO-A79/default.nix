{ self, nixpkgs, nix-on-droid }: {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    modules = [ ../../nix-on-droid ];
    pkgs = import nixpkgs;
  };
}
