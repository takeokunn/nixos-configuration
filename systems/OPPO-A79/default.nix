{ self, nixpkgs, nix-on-droid, emacs-overlay }: {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    modules = [ ../../nix-on-droid ];
    extraSpecialArgs = { emacs-overlay };
  };
}
