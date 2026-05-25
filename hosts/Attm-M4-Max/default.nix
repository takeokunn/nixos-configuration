{ inputs }:
let
  inherit (inputs)
    nix-darwin
    home-manager
    nixpkgs
    nixvim
    mac-app-util
    emacs-overlay
    nur-packages
    darwin-vz-nix
    ;

  username = "take";
  system = "aarch64-darwin";
  lib = nixpkgs.lib;

  editorOverlay = import ../../home-manager/editor/overlay.nix { inherit emacs-overlay; };
  shellOverlay = import ../../home-manager/shell/overlay.nix;

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = editorOverlay ++ shellOverlay;
  };

  nurPkgs = nur-packages.packages.${system};

  emacs = import ../../home-manager/editor/packages {
    inherit lib pkgs nurPkgs;
  };
  emacsPkg = emacs.emacs-unstable;

  emacsLib = import ../../home-manager/editor/lib/emacs.nix {
    inherit lib pkgs emacsPkg;
  };

  configuration = _: {
    users.users.${username}.home = "/Users/${username}";
  };
in
nix-darwin.lib.darwinSystem {
  inherit pkgs lib;
  specialArgs = {
    inherit
      inputs
      username
      pkgs
      emacsLib
      nurPkgs
      ;
  };
  modules = [
    configuration
    ../../nix-darwin
    darwin-vz-nix.darwinModules.default
    mac-app-util.darwinModules.default
    home-manager.darwinModules.home-manager
    {
      home-manager.useUserPackages = true;
      home-manager.users."${username}" = import ../../home-manager/advanced.nix;
      home-manager.sharedModules = [
        nixvim.homeModules.nixvim
        mac-app-util.homeManagerModules.default
        # inputs.zen-browser.homeModules.twilight  # TODO: hash mismatch, re-enable after fix
        inputs.agent-skills.homeManagerModules.default
      ];
      home-manager.extraSpecialArgs = {
        inherit inputs system username;
        inherit (inputs)
          nixpkgs
          nixvim
          mcp-servers-nix
          llm-agents
          nur-packages
          anthropic-skills
          cloudflare-skills
          hashicorp-agent-skills
          deno-skills
          aws-agent-skills
          microsoft-skills
          scientific-skills
          context7-skills
          ast-grep-skill
          emacs-overlay
          org-babel
          firefox-addons
          ;
      };
    }
  ];
}
