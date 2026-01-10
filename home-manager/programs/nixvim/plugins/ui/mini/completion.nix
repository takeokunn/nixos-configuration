# ════════════════════════════════════════════════════════════════════════════════
# Mini.nvim Completion & Visual Aids Modules
# ════════════════════════════════════════════════════════════════════════════════
# Completion system and visual aids: completion (nvim-cmp replacement), hipatterns
# (TODO highlighting), cursorword, trailspace, bufremove, bracketed, map, misc, snippets.
# ════════════════════════════════════════════════════════════════════════════════

_:
let
  utils = import ./utils.nix;
  inherit (utils) mkMiniKeymap;
in
{
  plugins.mini.modules = {
    # ══════════════════════════════════════════════════════════════════════════
    # Completion (replaces nvim-cmp)
    # ══════════════════════════════════════════════════════════════════════════
    # LSP-integrated completion with debounced delays
    completion = {
      delay = {
        completion = 100;
        info = 100;
        signature = 50;
      };
      lsp_completion = {
        source_func = "completefunc";
        auto_setup = true;
      };
      mappings = {
        force_twostep = "<C-Space>";
        force_fallback = "<A-Space>";
      };
      window = {
        info = {
          border = "rounded";
        };
        signature = {
          border = "rounded";
        };
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Hipatterns (TODO highlighting, partial replacement for todo-comments)
    # ══════════════════════════════════════════════════════════════════════════
    # delay.text_change adds debounce to reduce CPU usage in large files
    hipatterns = {
      delay = {
        text_change = 200;
        scroll = 50;
      };
      highlighters.__raw = ''
        {
          fixme = { pattern = '%f[%w]()FIXME()%f[%W]', group = 'MiniHipatternsFixme' },
          hack  = { pattern = '%f[%w]()HACK()%f[%W]',  group = 'MiniHipatternsHack'  },
          todo  = { pattern = '%f[%w]()TODO()%f[%W]',  group = 'MiniHipatternsTodo'  },
          note  = { pattern = '%f[%w]()NOTE()%f[%W]',  group = 'MiniHipatternsNote'  },
          hex_color = require('mini.hipatterns').gen_highlighter.hex_color(),
        }
      '';
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Cursorword (highlight word under cursor)
    # ══════════════════════════════════════════════════════════════════════════
    cursorword = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Trailspace (highlight trailing whitespace)
    # ══════════════════════════════════════════════════════════════════════════
    trailspace = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Bufremove (for buffer deletion)
    # ══════════════════════════════════════════════════════════════════════════
    bufremove = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Bracketed (navigate by brackets)
    # ══════════════════════════════════════════════════════════════════════════
    bracketed = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Map (code minimap)
    # ══════════════════════════════════════════════════════════════════════════
    map = {
      integrations.__raw = ''
        {
          require('mini.map').gen_integration.builtin_search(),
          require('mini.map').gen_integration.diff(),
          require('mini.map').gen_integration.diagnostic(),
        }
      '';
      symbols = {
        encode = {
          __raw = "require('mini.map').gen_encode_symbols.dot('4x2')";
        };
      };
      window = {
        width = 10;
        winblend = 25;
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Misc (miscellaneous utilities)
    # ══════════════════════════════════════════════════════════════════════════
    misc = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Snippets (snippet engine - replaces luasnip)
    # ══════════════════════════════════════════════════════════════════════════
    snippets = {
      snippets.__raw = ''
        {
          require('mini.snippets').gen_loader.from_lang(),
        }
      '';
    };
  };

  keymaps = [
    # mini.bufremove
    {
      mode = "n";
      key = "<leader>bc";
      action.__raw = "function() require('mini.bufremove').delete() end";
      options = {
        desc = "Close current buffer";
        silent = true;
      };
    }

    # mini.map keymaps
    (mkMiniKeymap "n" "<leader>mm" "map" "toggle" "Toggle minimap")
    (mkMiniKeymap "n" "<leader>mf" "map" "toggle_focus" "Toggle minimap focus")
    (mkMiniKeymap "n" "<leader>mr" "map" "refresh" "Refresh minimap")

    # mini.misc utilities
    (mkMiniKeymap "n" "<leader>z" "misc" "zoom" "Toggle zoom current window")

    # mini.trailspace
    (mkMiniKeymap "n" "<leader>tw" "trailspace" "trim" "Trim trailing whitespace")
  ];
}
