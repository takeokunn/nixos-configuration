# ════════════════════════════════════════════════════════════════════════════════
# Mini.nvim Git Integration Modules
# ════════════════════════════════════════════════════════════════════════════════
# Git integration: diff (gitsigns replacement) and git (additional features).
# Note: neogit and diffview are kept separately for advanced git UI.
# ════════════════════════════════════════════════════════════════════════════════

{ pkgs }:
let
  utils = import ./utils.nix;
  inherit (utils) mkMiniKeymap;
in
{
  plugins.mini.modules = {
    # ══════════════════════════════════════════════════════════════════════════
    # Diff (replaces gitsigns)
    # ══════════════════════════════════════════════════════════════════════════
    # Shows git diff in sign column
    diff = {
      view = {
        style = "sign";
        signs = {
          add = "│";
          change = "│";
          delete = "_";
        };
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Git (additional git features)
    # ══════════════════════════════════════════════════════════════════════════
    git = { };
  };

  keymaps = [
    # mini.diff keymaps
    {
      mode = "n";
      key = "]h";
      action.__raw = "function() require('mini.diff').goto_hunk('next') end";
      options.desc = "Next diff hunk";
    }
    {
      mode = "n";
      key = "[h";
      action.__raw = "function() require('mini.diff').goto_hunk('prev') end";
      options.desc = "Previous diff hunk";
    }
    (mkMiniKeymap "n" "<leader>go" "diff" "toggle_overlay" "Toggle diff overlay")
  ];
}
