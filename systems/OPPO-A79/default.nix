{ self, nixpkgs, home-manager, emacs-overlay, nix-on-droid }: {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    modules = [ ../../nix-on-droid ];
    pkgs = import nixpkgs {
      system = "aarch64-linux";
      overlays = [ nix-on-droid.overlays.default ];
    };
    home-manager-path = home-manager.outPath;
  };
}
