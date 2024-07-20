{ self, nixpkgs, nix-on-droid }: {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    pkgs = import nixpkgs { system = "x86_64"; };
    modules = [ ../../nix-on-droid ];
  };
}
