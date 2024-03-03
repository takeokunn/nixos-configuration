{ self, nixpkgs, private-nixpkgs, nix-darwin, home-manager, emacs-overlay, ...
}:
let
  system = "aarch64-darwin";
  username = "obara";
  configuration = { pkgs, ... }: {
    users.users.${username}.home = "/Users/${username}";
  };
  lib = nixpkgs.lib;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = import ./overlay.nix { inherit emacs-overlay; };
  };
  private-pkgs = import private-nixpkgs { inherit system; };
in {
  OPL2212-2 = nix-darwin.lib.darwinSystem {
    inherit system lib;
    modules = [
      configuration
      ./system.nix
      home-manager.darwinModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users."${username}" =
          import ./home.nix { inherit pkgs private-pkgs; };
      }
    ];
  };
}
