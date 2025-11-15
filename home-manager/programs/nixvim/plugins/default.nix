{ pkgs, sources, customPlugins }:
let
  coreEditing = import ./core/editing.nix { inherit pkgs customPlugins; };
  coreFileExplorer = import ./core/file-explorer.nix { inherit pkgs customPlugins; };

  uiColorscheme = import ./ui/colorscheme.nix { inherit pkgs; };
  uiStatusline = import ./ui/statusline.nix { inherit pkgs; };
  uiIcons = import ./ui/icons.nix { inherit pkgs; };
  uiDashboard = import ./ui/dashboard.nix { inherit pkgs; };

  languageLsp = import ./language/lsp.nix { inherit pkgs; };
  languageTreesitter = import ./language/treesitter.nix { inherit pkgs; };
  languageJapanese = import ./language/japanese.nix { inherit pkgs customPlugins; };
  languageMarkdown = import ./language/markdown.nix { inherit pkgs customPlugins; };

  toolsGit = import ./tools/git.nix { inherit pkgs customPlugins; };
  toolsTelescope = import ./tools/telescope.nix { inherit pkgs; };
  toolsMisc = import ./tools/misc.nix { inherit pkgs; };
in
  coreEditing
  // coreFileExplorer
  // uiColorscheme
  // uiStatusline
  // uiIcons
  // uiDashboard
  // languageLsp
  // languageTreesitter
  // languageJapanese
  // languageMarkdown
  // toolsGit
  // toolsTelescope
  // toolsMisc
