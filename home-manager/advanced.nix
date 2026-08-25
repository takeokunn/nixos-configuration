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
  shellOverlay = import ./shell/overlay.nix;
  sketchybarOverlay = import ./mac/sketchybar/overlay.nix;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = editorOverlay ++ [ mcp-servers-nix.overlays.default ];
  };

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

  _module.args = {
    inherit emacsPkg emacsLib llmAgentsPkgs;
  };

  nixpkgs.config.allowUnfree = true;
  # sketchybarOverlay and shellOverlay must be listed here (home-manager's own
  # pkgs), not only in the host overlays: without useGlobalPkgs, HM ignores the
  # host's pkgs and builds programs.sketchybar.package / programs.tmux.package
  # from this nixpkgs instance.
  nixpkgs.overlays =
    editorOverlay ++ shellOverlay ++ sketchybarOverlay ++ [ mcp-servers-nix.overlays.default ];

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
