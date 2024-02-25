{ self, nix-darwin, ... }:

{
  OPL2212-2 = nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    modules = [
      ./system.nix
    ];
  };
}
