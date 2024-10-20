{ inputs }:
let
  inherit (inputs) nix-darwin home-manager;
  system = "aarch64-darwin";
  username = "obara";
  configuration = { ... }: {
    users.users.${username}.home = "/Users/${username}";
  };
in {
  OPL2212-2 = nix-darwin.lib.darwinSystem {
    inherit system;
    inherit (inputs.nixpkgs) lib;
    specialArgs = { inherit username; };
    modules = [
      configuration
      ../../nix-darwin
      home-manager.darwinModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users."${username}" =
          import ../../home-manager/advanced.nix {
            inherit system;
            inherit (inputs)
              nixpkgs emacs-overlay neovim-nightly-overlay org-babel;
          };
      }
    ];
  };
}
