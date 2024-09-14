{ self, nixpkgs, nix-darwin, home-manager, org-babel, emacs-overlay
, wezterm-flake, neovim-nightly-overlay }:
let
  system = "aarch64-darwin";
  username = "obara";
  configuration = { pkgs, ... }: {
    users.users.${username}.home = "/Users/${username}";
  };
  lib = nixpkgs.lib;
in {
  OPL2212-2 = nix-darwin.lib.darwinSystem {
    inherit system lib;
    specialArgs = { inherit username; };
    modules = [
      configuration
      ../../nix-darwin
      home-manager.darwinModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users."${username}" =
          import ../../home-manager/advanced.nix {
            inherit system nixpkgs emacs-overlay wezterm-flake
              neovim-nightly-overlay org-babel;
          };
      }
    ];
  };
}
