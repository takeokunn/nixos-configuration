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
  brew-nix,
  firefox-addons,
  nur-packages,
  devenv,
  ...
}:
let
  isDarwin = builtins.match ".*-darwin" system != null;

  nurPkgs = nur-packages.packages.${system};
  devenvPkgs = devenv.packages.${system};

  basicOverlay = import ./overlay/basic.nix;
  advancedOverlay = import ./overlay/advanced.nix { inherit emacs-overlay; };
  brewNixOverlay = if isDarwin then [ brew-nix.overlays.default ] else [ ];
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays =
      basicOverlay ++ advancedOverlay ++ [ mcp-servers-nix.overlays.default ] ++ brewNixOverlay;
  };
  llmAgentsPkgs = llm-agents.packages.${system};
  basicPkgs = import ./packages/basic.nix { inherit pkgs nurPkgs devenvPkgs; };
  advancedPkgs = import ./packages/advanced.nix {
    inherit
      pkgs
      nurPkgs
      llmAgentsPkgs
      ;
  };

  emacsPkgSet = import ./editor/packages {
    inherit (nixpkgs) lib;
    inherit pkgs nurPkgs;
  };
  emacsPkg = if isDarwin then emacsPkgSet.emacs-unstable else emacsPkgSet.emacs-unstable-pgtk;
  emacsLib = import ./editor/lib/emacs.nix {
    inherit (nixpkgs) lib;
    inherit pkgs emacsPkg;
  };

  shell = import ./shell/basic.nix { inherit pkgs nurPkgs; };
  editor = import ./editor/basic.nix { inherit pkgs nurPkgs; };
  vcs = import ./vcs/basic.nix { inherit nurPkgs; };
  security = import ./security/basic.nix { inherit pkgs; };
  development = import ./development/basic.nix { inherit pkgs; };

  shellAdvanced = import ./shell/advanced.nix { inherit pkgs; };
  editorAdvanced = import ./editor/advanced.nix {
    inherit (nixpkgs) lib;
    inherit
      pkgs
      emacsPkg
      org-babel
      llmAgentsPkgs
      ;
  };
  browser = import ./browser { inherit pkgs firefox-addons; };
  vcsAdvanced = import ./vcs/advanced.nix;
  securityAdvanced = import ./security/advanced.nix { inherit pkgs; };
  email = import ./email { inherit pkgs; };
  wayland = import ./wayland { inherit pkgs nurPkgs emacsLib; };
  nixTools = import ./nix;
  cloud = import ./cloud;
  developmentAdvanced = import ./development/advanced.nix { inherit pkgs; };
  mac = if isDarwin then import ./mac { inherit pkgs; } else [ ];
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
    ++ mac;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays =
    basicOverlay ++ advancedOverlay ++ [ mcp-servers-nix.overlays.default ] ++ brewNixOverlay;

  home.stateVersion = "25.11";
  home.enableNixpkgsReleaseCheck = false;
  home.packages = basicPkgs ++ advancedPkgs;

  targets.darwin.linkApps.enable = isDarwin;
  targets.darwin.copyApps.enable = false;

  accounts.email.accounts = {
    Gmail = {
      primary = true;
      flavor = "gmail.com";
      realName = "takeo obara";
      address = "bararararatty@gmail.com";
    };
  };
}
