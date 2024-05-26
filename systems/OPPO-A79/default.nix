{ self, nixpkgs, home-manager, nix-on-droid }: {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    modules = [ ./nix-on-droid.nix ];
  };
}
