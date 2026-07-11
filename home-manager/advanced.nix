{
  system,
  nixpkgs,
  emacs-overlay,
  mcp-servers-nix,
  llm-agents,
  nur-packages,
  ...
}:
let
  lib = nixpkgs.lib;
  isDarwin = lib.hasSuffix "-darwin" system;

  editorOverlay = import ./editor/overlay.nix { inherit emacs-overlay; };
  sketchybarOverlay = import ./mac/sketchybar/overlay.nix;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = editorOverlay ++ [ mcp-servers-nix.overlays.default ];
  };

  # nurPkgs built against emacs-unstable, used only to build the Emacs package
  # set (the module-system `nurPkgs` from ./nur.nix uses the default emacs
  # packages and serves every other module).
  nurPkgsEmacs = import "${nur-packages}" {
    inherit pkgs;
    emacsPackages = pkgs.emacsPackagesFor pkgs.emacs-unstable;
  };
  llmAgentsPkgs = llm-agents.packages.${system};

  emacsPkgSet = import ./editor/packages {
    inherit lib pkgs;
    nurPkgs = nurPkgsEmacs;
  };
  emacsPkg = if isDarwin then emacsPkgSet.emacs-unstable else emacsPkgSet.emacs-unstable-pgtk;
  emacsLib = import ./editor/lib/emacs.nix {
    inherit lib pkgs emacsPkg;
  };
in
{
  imports = [
    ./nur.nix
    ./shell/basic.nix
    ./shell/advanced.nix
    ./editor/basic.nix
    ./editor/advanced.nix
    ./vcs/basic.nix
    ./vcs/advanced.nix
    ./security/basic.nix
    ./security/advanced.nix
    ./development/basic.nix
    ./development/advanced.nix
    ./browser
    ./email
    ./nix
    ./cloud
    ./communication
    ./ai-tools
  ]
  ++ lib.optionals isDarwin [ ./mac/sketchybar ]
  ++ lib.optionals (!isDarwin) [ ./wayland ];

  # Threaded to the Emacs/copilot modules that need pre-built derivations. These
  # are not among the host extraSpecialArgs, so setting them here is safe (a
  # specialArg cannot be shadowed by _module.args). org-babel, mcp-servers-nix
  # and the skill sources remain specialArgs supplied by the host.
  _module.args = {
    inherit emacsPkg emacsLib llmAgentsPkgs;
  };

  nixpkgs.config.allowUnfree = true;
  # sketchybarOverlay must be listed here (home-manager's own pkgs), not only in
  # the host overlays: without useGlobalPkgs, HM ignores the host's pkgs and
  # builds programs.sketchybar.package from this nixpkgs instance.
  nixpkgs.overlays = editorOverlay ++ sketchybarOverlay ++ [ mcp-servers-nix.overlays.default ];

  programs.nixvim.nixpkgs.source = nixpkgs;

  home.stateVersion = "25.11";
  home.enableNixpkgsReleaseCheck = false;

  targets.darwin.linkApps.enable = isDarwin;
  targets.darwin.copyApps.enable = false;

  accounts.email.accounts.Gmail.primary = true;
  accounts.email.accounts.Gmail.flavor = "gmail.com";
  accounts.email.accounts.Gmail.realName = "takeo obara";
  accounts.email.accounts.Gmail.address = "bararararatty@gmail.com";
}
