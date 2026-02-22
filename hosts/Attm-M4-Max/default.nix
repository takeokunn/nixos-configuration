{ inputs }:
let
  inherit (inputs)
    nix-darwin
    home-manager
    nixvim
    brew-nix
    mac-app-util
    emacs-overlay
    nur-packages
    darwin-vz-nix
    ;
  inherit (inputs) nixpkgs;

  username = "take";
  system = "aarch64-darwin";

  advancedOverlay = import ../../home-manager/overlay/advanced.nix { inherit emacs-overlay; };

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = advancedOverlay ++ [ brew-nix.overlays.default ];
  };

  nurPkgs = nur-packages.packages.${system};

  # emacs package (for nix-darwin to use in aerospace)
  emacs = import ../../home-manager/packages/emacs {
    inherit (nixpkgs) lib;
    inherit pkgs nurPkgs;
  };
  emacsPkg = emacs.emacs-unstable;

  # emacs library (shared utilities for both nix-darwin and home-manager)
  emacsLib = import ../../home-manager/lib/emacs.nix {
    inherit (nixpkgs) lib;
    inherit pkgs emacsPkg;
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
    inherit
      inputs
      username
      pkgs
      emacsLib
      ;
  };
  modules = [
    configuration
    ../../nix-darwin
    darwin-vz-nix.darwinModules.default
    brew-nix.darwinModules.default
    mac-app-util.darwinModules.default
    home-manager.darwinModules.home-manager
    {
      home-manager = {
        useUserPackages = true;
        users."${username}" = import ../../home-manager/advanced.nix;
        sharedModules = [
          nixvim.homeModules.nixvim
          mac-app-util.homeManagerModules.default
          inputs.zen-browser.homeModules.twilight
          inputs.agent-skills.homeManagerModules.default
        ];
        extraSpecialArgs = {
          inherit inputs system username;
          inherit (inputs) nixpkgs nixvim;
          inherit (inputs) mcp-servers-nix;
          inherit (inputs) llm-agents;
          inherit (inputs) nur-packages;
          inherit (inputs)
            anthropic-skills
            cloudflare-skills
            hashicorp-agent-skills
            deno-skills
            aws-agent-skills
            microsoft-skills
            ;
          inherit (inputs)
            emacs-overlay
            org-babel
            brew-nix
            firefox-addons
            ;
        };
      };
    }
  ];
}
