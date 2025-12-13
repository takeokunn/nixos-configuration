{
  pkgs,
  sources,
  customPackages,
}:
let
  coreEditing = import ./core/editing.nix { inherit pkgs; };
  coreFileExplorer = import ./core/file-explorer.nix { inherit pkgs customPackages; };

  languageLsp = import ./language/lsp.nix { inherit pkgs; };
  languageTreesitter = import ./language/treesitter.nix { inherit pkgs; };
  languageJapanese = import ./language/japanese.nix { inherit pkgs customPackages; };
  languageMarkdown = import ./language/markdown.nix { inherit pkgs customPackages; };
  languageRust = import ./language/rust.nix { inherit pkgs customPackages; };
  languageWeb = import ./language/web.nix { inherit pkgs customPackages; };

  toolsGit = import ./tools/git.nix { inherit pkgs customPackages; };
  toolsTelescope = import ./tools/telescope.nix;
  toolsHarpoon = import ./tools/harpoon.nix { inherit pkgs; };
  toolsMisc = import ./tools/misc.nix { inherit pkgs; };

  uiStatusline = import ./ui/statusline.nix { inherit pkgs; };
  uiIcons = import ./ui/icons.nix { inherit pkgs; };
  uiDashboard = import ./ui/dashboard.nix { inherit pkgs; };
  uiNavigation = import ./ui/navigation.nix { inherit pkgs; };
  uiNotifications = import ./ui/notifications.nix { inherit pkgs; };
  uiBufferline = import ./ui/bufferline.nix { inherit pkgs; };
  uiVisualAids = import ./ui/visual-aids.nix { inherit pkgs; };

  allModules = [
    coreEditing
    coreFileExplorer
    languageLsp
    languageTreesitter
    languageJapanese
    languageMarkdown
    languageRust
    languageWeb
    toolsGit
    toolsTelescope
    toolsHarpoon
    toolsMisc
    uiStatusline
    uiIcons
    uiDashboard
    uiNavigation
    uiNotifications
    uiBufferline
    uiVisualAids
  ];

  merged =
    builtins.foldl'
      (acc: m: {
        keymaps = acc.keymaps ++ (m.keymaps or [ ]);
        globals = acc.globals // (m.globals or { });
        userCommands = acc.userCommands // (m.userCommands or { });
        plugins = acc.plugins // (m.plugins or { });
        extraPlugins = acc.extraPlugins ++ (m.extraPlugins or [ ]);
      })
      {
        keymaps = [ ];
        globals = { };
        userCommands = { };
        plugins = { };
        extraPlugins = [ ];
      }
      allModules;
in
merged
