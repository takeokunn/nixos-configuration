{ self, nixpkgs, private-nixpkgs, nix-darwin, home-manager, emacs-overlay, ...
}:
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
    modules = [
      configuration
      ./system.nix
      home-manager.darwinModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users."${username}" = import ../../home-manager {
          inherit system nixpkgs private-nixpkgs emacs-overlay;
        };
      }
    ];
  };
}
