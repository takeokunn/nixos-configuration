# ════════════════════════════════════════════════════════════════════════════════
# Mini.nvim Shared Utilities
# ════════════════════════════════════════════════════════════════════════════════
# Helper functions used across mini.nvim domain modules for keymap generation.
# These reduce boilerplate when defining keymaps that call mini.nvim Lua functions.
# ════════════════════════════════════════════════════════════════════════════════

{
  # Helper for mini.pick keymaps (builtin pickers)
  # Usage: mkPickKeymap "f" "files({ tool = 'git' })" "Find Git Files"
  mkPickKeymap = key: picker: desc: {
    mode = "n";
    key = "<leader>f${key}";
    action.__raw = "function() require('mini.pick').builtin.${picker} end";
    options.desc = desc;
  };

  # Helper for mini.extra pickers (extended pickers)
  # Usage: mkExtraPickKeymap "r" "registers()" "Find Registers"
  mkExtraPickKeymap = key: picker: desc: {
    mode = "n";
    key = "<leader>f${key}";
    action.__raw = "function() require('mini.extra').pickers.${picker}() end";
    options.desc = desc;
  };

  # Helper for generic mini module actions
  # Usage: mkMiniKeymap "n" "<leader>mm" "map" "toggle" "Toggle minimap"
  mkMiniKeymap = mode: key: module: func: desc: {
    inherit mode key;
    action.__raw = "function() require('mini.${module}').${func}() end";
    options = {
      inherit desc;
      silent = true;
    };
  };
}
