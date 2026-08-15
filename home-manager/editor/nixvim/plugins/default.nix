{
  pkgs,
  customPackages,
}:
let
  coreEditing = import ./core/editing.nix { inherit pkgs; };

  languageLsp = import ./language/lsp.nix;
  languageTreesitter = import ./language/treesitter.nix { inherit pkgs; };
  languageMarkdown = import ./language/markdown.nix { inherit pkgs customPackages; };
  languageRust = import ./language/rust.nix;
  languageWeb = import ./language/web.nix;
  languageJapanese = import ./language/japanese.nix { inherit pkgs customPackages; };

  toolsGit = import ./tools/git.nix;
  toolsMisc = import ./tools/misc.nix { inherit pkgs; };

  uiMini = import ./ui/mini;
  uiNavigation = import ./ui/navigation.nix;

  allModules = [
    coreEditing
    languageLsp
    languageTreesitter
    languageMarkdown
    languageRust
    languageWeb
    languageJapanese
    toolsGit
    toolsMisc
    uiMini
    uiNavigation
  ];

  merged =
    builtins.foldl'
      (acc: m: {
        keymaps = acc.keymaps ++ (m.keymaps or [ ]);
        globals = acc.globals // (m.globals or { });
        userCommands = acc.userCommands // (m.userCommands or { });
        plugins = acc.plugins // (m.plugins or { });
        extraPlugins = acc.extraPlugins ++ (m.extraPlugins or [ ]);
        extraConfigLua =
          let
            current = m.extraConfigLua or "";
          in
          if current == "" then acc.extraConfigLua else acc.extraConfigLua + "\n" + current;
      })
      {
        keymaps = [ ];
        globals = { };
        userCommands = { };
        plugins = { };
        extraPlugins = [ ];
        extraConfigLua = "";
      }
      allModules;
in
merged
