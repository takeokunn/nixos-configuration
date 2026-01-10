# ════════════════════════════════════════════════════════════════════════════════
# Mini.nvim UI Enhancement Modules
# ════════════════════════════════════════════════════════════════════════════════
# Visual interface components: icons, statusline, tabline, starter, notify,
# indentscope, and clue (which-key replacement).
# ════════════════════════════════════════════════════════════════════════════════

_: {
  # Disable web-devicons in favor of mini.icons
  plugins.web-devicons.enable = false;

  plugins.mini.modules = {
    # ══════════════════════════════════════════════════════════════════════════
    # Icons (replaces web-devicons)
    # ══════════════════════════════════════════════════════════════════════════
    icons = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Statusline (replaces lualine)
    # ══════════════════════════════════════════════════════════════════════════
    statusline = {
      use_icons = true;
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Tabline (replaces bufferline)
    # ══════════════════════════════════════════════════════════════════════════
    tabline = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Starter (replaces dashboard)
    # ══════════════════════════════════════════════════════════════════════════
    starter = {
      header.__raw = ''
        function()
          return [[
        ███╗   ██╗██╗██╗  ██╗██╗   ██╗██╗███╗   ███╗
        ████╗  ██║██║╚██╗██╔╝██║   ██║██║████╗ ████║
        ██╔██╗ ██║██║ ╚███╔╝ ██║   ██║██║██╔████╔██║
        ██║╚██╗██║██║ ██╔██╗ ╚██╗ ██╔╝██║██║╚██╔╝██║
        ██║ ╚████║██║██╔╝ ██╗ ╚████╔╝ ██║██║ ╚═╝ ██║
        ╚═╝  ╚═══╝╚═╝╚═╝  ╚═╝  ╚═══╝  ╚═╝╚═╝     ╚═╝
        ]]
        end
      '';
      items.__raw = ''
        {
          require('mini.starter').sections.builtin_actions(),
          require('mini.starter').sections.recent_files(10, false),
          require('mini.starter').sections.recent_files(10, true),
        }
      '';
      footer = "Happy Coding with NixVim!";
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Notify (replaces noice/notify)
    # ══════════════════════════════════════════════════════════════════════════
    notify = {
      lsp_progress = {
        enable = false;
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Indentscope (replaces indent-blankline)
    # ══════════════════════════════════════════════════════════════════════════
    indentscope = {
      symbol = "│";
      options = {
        try_as_border = true;
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Clue (replaces which-key)
    # ══════════════════════════════════════════════════════════════════════════
    # Displays available keybindings in a popup after pressing leader/trigger keys
    clue = {
      triggers.__raw = ''
        {
          { mode = 'n', keys = '<Leader>' },
          { mode = 'x', keys = '<Leader>' },
          { mode = 'n', keys = 'g' },
          { mode = 'x', keys = 'g' },
          { mode = 'n', keys = "'" },
          { mode = 'n', keys = '`' },
          { mode = 'x', keys = "'" },
          { mode = 'x', keys = '`' },
          { mode = 'n', keys = '"' },
          { mode = 'x', keys = '"' },
          { mode = 'i', keys = '<C-r>' },
          { mode = 'c', keys = '<C-r>' },
          { mode = 'n', keys = '<C-w>' },
          { mode = 'n', keys = 'z' },
          { mode = 'x', keys = 'z' },
        }
      '';
      clues.__raw = ''
        {
          require('mini.clue').gen_clues.builtin_completion(),
          require('mini.clue').gen_clues.g(),
          require('mini.clue').gen_clues.marks(),
          require('mini.clue').gen_clues.registers(),
          require('mini.clue').gen_clues.windows(),
          require('mini.clue').gen_clues.z(),
        }
      '';
      window = {
        delay = 300;
        config = {
          width = "auto";
        };
      };
    };
  };

  keymaps = [
    # Buffer navigation (using mini.tabline)
    {
      mode = "n";
      key = "<Tab>";
      action = "<cmd>bnext<CR>";
      options = {
        desc = "Next buffer";
        silent = true;
      };
    }
    {
      mode = "n";
      key = "<S-Tab>";
      action = "<cmd>bprevious<CR>";
      options = {
        desc = "Previous buffer";
        silent = true;
      };
    }
  ];
}
