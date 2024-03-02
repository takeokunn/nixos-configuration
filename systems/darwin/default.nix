{ self, nixpkgs, nix-darwin, home-manager, emacs-overlay, ... }:
let
  system = "aarch64-darwin";
  lib = nixpkgs.lib;
  overlay = import emacs-overlay;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = [
      overlay
      (final: prev: {
        gotools = prev.gotools.overrideAttrs (old: {
          postPatch = ''
            # The gopls folder contains a Go submodule which causes a build failure
            # and lives in its own package named gopls.
            rm -r gopls
            # getgo is an experimental go installer which adds generic named server and client binaries to $out/bin
            rm -r cmd/getgo
            # remove bundle
            rm -r cmd/bundle
          '';
        });
      })
    ];
  };
in {
  OPL2212-2 = nix-darwin.lib.darwinSystem {
    inherit system lib;
    modules = [
      ./system.nix
      home-manager.darwinModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users.obara = import ./home.nix { inherit pkgs; };
      }
    ];
  };
}
