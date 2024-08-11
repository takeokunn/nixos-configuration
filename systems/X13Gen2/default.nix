{ self, nixpkgs, home-manager, emacs-overlay, nixos-hardware }:
let
  username = "take";
  system = "x86_64-linux";
in {
  X13Gen2 = nixpkgs.lib.nixosSystem {
    inherit system;
    modules = [ ../../nixos ./hardware-configuration.nix ];
  };
}
