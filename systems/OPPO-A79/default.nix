{ self, nixpkgs, nix-on-droid }: {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    pkgs = import nixpkgs { system = "aarch64_linux"; };
    modules = [ ../../nix-on-droid ];
  };
}
