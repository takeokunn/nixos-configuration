{
  config,
  system,
  nixpkgs,
  org-babel,
  emacs-overlay,
  mcp-servers-nix,
  ...
}:
let
  # nvfetcher
  sources = pkgs.callPackage ../_sources/generated.nix { };

  # packages
  basicOverlay = import ./overlay/basic.nix { inherit emacs-overlay; };
  advancedOverlay = import ./overlay/advanced.nix;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = basicOverlay ++ advancedOverlay;
  };
  nodePkgs = pkgs.callPackage ../node2nix { inherit pkgs; };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };
  advancedPkgs = import ./packages/advanced.nix { inherit pkgs nodePkgs; };

  # emacs package
  emacs = import ./packages/emacs {
    inherit (nixpkgs) lib;
    inherit pkgs sources;
  };
  emacsPkg = emacs.emacs-unstable-without-nativecomp;

  # misc
  misc = import ./misc;

  # modules
  modules = import ./modules;

  # programs
  basicPrograms = import ./programs/basic.nix {
    inherit pkgs sources;
    inherit org-babel emacsPkg;
  };
  advancedPrograms = import ./programs/advanced.nix {
    inherit (nixpkgs) lib;
    inherit pkgs nodePkgs sources;
  };

  # services
  basicServices = import ./services/basic.nix;
  advancedServices = import ./services/advanced.nix {
    inherit pkgs emacsPkg;
  };

  # sops
  sops = [
    (import ../sops {
      homeDirectory = config.home.homeDirectory;
    })
  ];

  # mcp servers
  mcpServers = import ./mcp-servers {
    inherit
      pkgs
      config
      mcp-servers-nix
      nodePkgs
      ;
  };
in
{
  imports =
    misc
    ++ modules
    ++ basicPrograms
    ++ advancedPrograms
    ++ basicServices
    ++ advancedServices
    ++ sops
    ++ mcpServers;

  home.stateVersion = "24.11";
  home.packages = basicPkgs ++ advancedPkgs;

  accounts.email.accounts = {
    Gmail = {
      primary = true;
      flavor = "gmail.com";
      realName = "takeo obara";
      address = "bararararatty@gmail.com";
    };
  };
}
