{
  system,
  nixpkgs,
  org-babel,
  emacs-overlay,
  mcp-servers-nix,
  llm-agents,
  anthropic-skills,
  cloudflare-skills,
  hashicorp-agent-skills,
  deno-skills,
  aws-agent-skills,
  microsoft-skills,
  scientific-skills,
  context7-skills,
  ast-grep-skill,
  firefox-addons,
  nur-packages,
  ...
}:
let
  lib = nixpkgs.lib;
  isDarwin = lib.hasSuffix "-darwin" system;

  nurPkgs = import "${nur-packages}" {
    inherit pkgs;
    emacsPackages = pkgs.emacsPackagesFor pkgs.emacs-unstable;
  };

  editorOverlay = import ./editor/overlay.nix { inherit emacs-overlay; };
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = editorOverlay ++ [ mcp-servers-nix.overlays.default ];
  };
  llmAgentsPkgs = llm-agents.packages.${system};
  emacsPkgSet = import ./editor/packages {
    inherit lib pkgs nurPkgs;
  };
  emacsPkg = if isDarwin then emacsPkgSet.emacs-unstable else emacsPkgSet.emacs-unstable-pgtk;
  emacsLib = import ./editor/lib/emacs.nix {
    inherit lib pkgs emacsPkg;
  };

  shell = import ./shell/basic.nix { inherit pkgs nurPkgs; };
  editor = import ./editor/basic.nix { inherit pkgs nurPkgs; };
  vcs = import ./vcs/basic.nix { inherit pkgs nurPkgs; };
  security = import ./security/basic.nix { inherit pkgs; };
  development = import ./development/basic.nix { inherit pkgs nurPkgs; };

  shellAdvanced = import ./shell/advanced.nix { inherit pkgs; };
  editorAdvanced = import ./editor/advanced.nix {
    inherit lib;
    inherit
      pkgs
      emacsPkg
      org-babel
      llmAgentsPkgs
      nurPkgs
      ;
  };
  browser = import ./browser { inherit pkgs firefox-addons; };
  vcsAdvanced = import ./vcs/advanced.nix;
  securityAdvanced = import ./security/advanced.nix { inherit pkgs; };
  email = import ./email { inherit pkgs; };
  wayland = import ./wayland { inherit pkgs nurPkgs emacsLib; };
  nixTools = import ./nix;
  cloud = import ./cloud { inherit pkgs; };
  developmentAdvanced = import ./development/advanced.nix;
  sketchybar = if isDarwin then [ (import ./mac/sketchybar { inherit pkgs; }) ] else [ ];
  communication = import ./communication { inherit pkgs isDarwin; };
  aiTools = import ./ai-tools {
    inherit
      pkgs
      nurPkgs
      llmAgentsPkgs
      mcp-servers-nix
      ;
    inherit
      anthropic-skills
      cloudflare-skills
      hashicorp-agent-skills
      deno-skills
      aws-agent-skills
      microsoft-skills
      scientific-skills
      context7-skills
      ast-grep-skill
      ;
  };
in
{
  imports =
    shell
    ++ editor
    ++ vcs
    ++ security
    ++ development
    ++ shellAdvanced
    ++ editorAdvanced
    ++ browser
    ++ vcsAdvanced
    ++ securityAdvanced
    ++ email
    ++ wayland
    ++ nixTools
    ++ cloud
    ++ developmentAdvanced
    ++ aiTools
    ++ sketchybar
    ++ communication;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = editorOverlay ++ [ mcp-servers-nix.overlays.default ];

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
