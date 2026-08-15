let
  utils = import ./utils.nix;
  inherit (utils) mkMiniKeymap;
in
{
  plugins.mini.modules = {
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

    cursorword = { };

    trailspace = { };

    bufremove = { };

    bracketed = { };

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

    misc = { };

    snippets = {
      snippets.__raw = ''
        {
          require('mini.snippets').gen_loader.from_lang(),
        }
      '';
    };
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>bc";
      action.__raw = "function() require('mini.bufremove').delete() end";
      options.desc = "Close current buffer";
      options.silent = true;
    }

    (mkMiniKeymap "n" "<leader>mm" "map" "toggle" "Toggle minimap")
    (mkMiniKeymap "n" "<leader>mf" "map" "toggle_focus" "Toggle minimap focus")
    (mkMiniKeymap "n" "<leader>mr" "map" "refresh" "Refresh minimap")

    (mkMiniKeymap "n" "<leader>z" "misc" "zoom" "Toggle zoom current window")

    (mkMiniKeymap "n" "<leader>tw" "trailspace" "trim" "Trim trailing whitespace")
  ];
}
