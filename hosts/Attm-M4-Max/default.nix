{ inputs }:
let
  inherit (inputs)
    nix-darwin
    home-manager
    nixvim
    brew-nix
    # mac-app-util temporarily disabled - ECL C23 build failure
    # See: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/775
    # mac-app-util
    emacs-overlay
    ;
  inherit (inputs) nixpkgs;

  username = "take";
  system = "aarch64-darwin";

  advancedOverlay = import ../../home-manager/overlay/advanced.nix { inherit emacs-overlay; };

  pkgs = import nixpkgs {
    inherit system;
    overlays = advancedOverlay ++ [ brew-nix.overlays.default ];
  };

  configuration =
    { ... }:
    {
      users.users.${username}.home = "/Users/${username}";
    };
in
nix-darwin.lib.darwinSystem {
  inherit pkgs;
  inherit (inputs.nixpkgs) lib;
  specialArgs = {
    inherit username pkgs;
  };
  modules = [
    configuration
    ../../nix-darwin
    brew-nix.darwinModules.default
    # mac-app-util.darwinModules.default  # Temporarily disabled
    home-manager.darwinModules.home-manager
    {
      home-manager = {
        useUserPackages = true;
        users."${username}" = import ../../home-manager/advanced.nix;
        sharedModules = [
          nixvim.homeModules.nixvim
          # mac-app-util.homeManagerModules.default  # Temporarily disabled
        ];
        extraSpecialArgs = {
          inherit system username;
          inherit (inputs) nixpkgs nixvim;
          inherit (inputs) mcp-servers-nix;
          inherit (inputs) emacs-overlay org-babel brew-nix;
        };
      };
    }
  ];
}
