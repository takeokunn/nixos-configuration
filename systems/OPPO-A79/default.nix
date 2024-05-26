{ self, nixpkgs, home-manager, emacs-overlay, nix-on-droid }: {
  OPPO-A79 = nix-on-droid.lib.nixOnDroidConfiguration {
    modules = [ ./nix-on-droid.nix ];

    # home-manager.config = { pkgs }:
    #   {

    #   };
  };
}
