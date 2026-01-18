{
  system,
  nixpkgs,
  org-babel,
  emacs-overlay,
  mcp-servers-nix,
  llm-agents,
  brew-nix,
  arto,
  nur-packages,
  ...
}:
let
  isDarwin = builtins.match ".*-darwin" system != null;

  # nur-packages
  nurPkgs = nur-packages.packages.${system};

  # packages
  basicOverlay = import ./overlay/basic.nix;
  advancedOverlay = import ./overlay/advanced.nix { inherit emacs-overlay; };
  brewNixOverlay = if isDarwin then [ brew-nix.overlays.default ] else [ ];
  artoPkg = if isDarwin then arto.packages.${system}.default else null;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays =
      basicOverlay ++ advancedOverlay ++ [ mcp-servers-nix.overlays.default ] ++ brewNixOverlay;
  };
  llmAgentsPkgs = llm-agents.packages.${system};
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };
  advancedPkgs = import ./packages/advanced.nix { inherit pkgs llmAgentsPkgs artoPkg; };

  # emacs package
  emacs = import ./packages/emacs {
    inherit (nixpkgs) lib;
    inherit pkgs nurPkgs;
  };
  emacsPkg = emacs.emacs-unstable;

  # misc
  misc = import ./misc;

  # modules
  modules = import ./modules;

  # programs
  basicPrograms = import ./programs/basic.nix {
    inherit pkgs nurPkgs;
  };
  advancedPrograms = import ./programs/advanced.nix {
    inherit pkgs llmAgentsPkgs;
    inherit org-babel emacsPkg;
    inherit mcp-servers-nix;
  };

  # services
  basicServices = import ./services/basic.nix;
  advancedServices = import ./services/advanced.nix {
    inherit pkgs emacsPkg;
  };
in
{
  imports = misc ++ modules ++ basicPrograms ++ advancedPrograms ++ basicServices ++ advancedServices;

  nixpkgs.config.allowUnfree = true;

  home.stateVersion = "24.11";
  home.enableNixpkgsReleaseCheck = false;
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
