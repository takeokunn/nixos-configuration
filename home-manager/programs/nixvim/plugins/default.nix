# ════════════════════════════════════════════════════════════════════════════════
# NixVim Plugin Aggregation
# ════════════════════════════════════════════════════════════════════════════════
# This module aggregates all plugin configurations using a functional merge pattern.
# Each domain module exports: plugins, keymaps, globals, userCommands, extraConfigLua
#
# Keymap Allocation Reference:
# ────────────────────────────────────────────────────────────────────────────────
# <leader>f*  = Finder (mini.pick) - files, grep, buffers, help
# <leader>e   = Explorer (mini.files) - file tree at project root
# <leader>E   = Explorer (mini.files) - file tree at current file
# <leader>g*  = Git (neogit, diffview, mini.diff) - status, commits, hunks
# <leader>h*  = Harpoon (mini.visits) - quick file bookmarks
# <leader>a   = Aerial (symbols) - code outline
# <leader>m*  = Minimap (mini.map) - code minimap toggle/focus
# <leader>s*  = Sessions (mini.sessions) - save/load/delete
# <leader>t*  = Trailing (mini.trailspace) + TODO search
# <leader>u   = Undotree - undo history visualization
# <leader>z   = Zoom (mini.misc) - toggle window zoom
# <leader>c*  = Code actions (lsp)
# <leader>d*  = Diagnostics (trouble)
# <leader>b*  = Buffers - close, navigate
# <leader>1-4 = Visit files 1-4 (mini.visits/harpoon)
# ]h/[h       = Next/prev diff hunk
# gj          = Jump2d (character jump)
# gS          = Splitjoin toggle
# g=,gx,gm,gr,gs = Operators (evaluate, exchange, multiply, replace, sort)
# sa,sd,sf,sF,sh,sr,sn = Surround operations
# ════════════════════════════════════════════════════════════════════════════════

{
  pkgs,
  customPackages,
}:
let
  # ════════════════════════════════════════════════════════════════════════════
  # Core Plugins
  # ════════════════════════════════════════════════════════════════════════════
  # Foundational editing tools not handled by mini.nvim
  coreEditing = import ./core/editing.nix { inherit pkgs; };

  # ════════════════════════════════════════════════════════════════════════════
  # Language Plugins
  # ════════════════════════════════════════════════════════════════════════════
  # Language servers, treesitter, and language-specific tooling
  languageLsp = import ./language/lsp.nix { };
  languageTreesitter = import ./language/treesitter.nix { inherit pkgs; };
  languageJapanese = import ./language/japanese.nix { inherit pkgs customPackages; };
  languageMarkdown = import ./language/markdown.nix { inherit pkgs customPackages; };
  languageRust = import ./language/rust.nix { };
  languageWeb = import ./language/web.nix { };

  # ════════════════════════════════════════════════════════════════════════════
  # Tools Plugins
  # ════════════════════════════════════════════════════════════════════════════
  # Git workflows (neogit, diffview) and miscellaneous utilities
  toolsGit = import ./tools/git.nix { };
  toolsMisc = import ./tools/misc.nix { inherit pkgs; };

  # ════════════════════════════════════════════════════════════════════════════
  # UI Plugins
  # ════════════════════════════════════════════════════════════════════════════
  # mini.nvim ecosystem (26 modules) and complementary UI (trouble, aerial)
  uiMini = import ./ui/mini { };
  uiNavigation = import ./ui/navigation.nix { };

  # ════════════════════════════════════════════════════════════════════════════
  # Module Aggregation
  # ════════════════════════════════════════════════════════════════════════════
  allModules = [
    # Core
    coreEditing
    # Language
    languageLsp
    languageTreesitter
    languageJapanese
    languageMarkdown
    languageRust
    languageWeb
    # Tools
    toolsGit
    toolsMisc
    # UI
    uiMini
    uiNavigation
  ];

  # Merge all modules using functional fold pattern
  # Each module can export: keymaps, globals, userCommands, plugins, extraPlugins, extraConfigLua
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
