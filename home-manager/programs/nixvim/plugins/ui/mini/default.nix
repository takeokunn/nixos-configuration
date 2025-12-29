# ════════════════════════════════════════════════════════════════════════════════
# Mini.nvim Module Aggregator
# ════════════════════════════════════════════════════════════════════════════════
# This module aggregates all mini.nvim domain configurations into a single export.
# Split by functional domain for better maintainability:
#   - ui.nix:         Icons, statusline, tabline, starter, notify, indentscope, clue
#   - editing.nix:    Pairs, surround, jump, ai, move, splitjoin, operators, align, comment, animate
#   - navigation.nix: Pick, extra, fuzzy, files, visits, sessions
#   - git.nix:        Diff, git
#   - completion.nix: Completion, hipatterns, cursorword, trailspace, bufremove, bracketed, map, misc, snippets
# ════════════════════════════════════════════════════════════════════════════════

{ pkgs }:
let
  # Import all domain modules
  ui = import ./ui.nix { inherit pkgs; };
  editing = import ./editing.nix { inherit pkgs; };
  navigation = import ./navigation.nix { inherit pkgs; };
  git = import ./git.nix { inherit pkgs; };
  completion = import ./completion.nix { inherit pkgs; };

  allModules = [
    ui
    editing
    navigation
    git
    completion
  ];

  # Deep merge for plugins.mini.modules
  mergeModules =
    modules: builtins.foldl' (acc: m: acc // (m.plugins.mini.modules or { })) { } modules;

  # Merge all modules using functional fold pattern
  merged =
    builtins.foldl'
      (acc: m: {
        keymaps = acc.keymaps ++ (m.keymaps or [ ]);
        plugins = {
          web-devicons = m.plugins.web-devicons or acc.plugins.web-devicons or { };
          mini = {
            enable = true;
            mockDevIcons = true;
            modules = acc.plugins.mini.modules // (m.plugins.mini.modules or { });
          };
        };
      })
      {
        keymaps = [ ];
        plugins = {
          web-devicons = { };
          mini = {
            enable = true;
            mockDevIcons = true;
            modules = { };
          };
        };
      }
      allModules;
in
merged
