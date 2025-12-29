# ════════════════════════════════════════════════════════════════════════════════
# Mini.nvim Navigation & File Management Modules
# ════════════════════════════════════════════════════════════════════════════════
# File navigation and search: pick (telescope replacement), extra pickers,
# files (oil replacement), visits (harpoon replacement), and sessions.
# ════════════════════════════════════════════════════════════════════════════════

{ pkgs }:
let
  utils = import ./utils.nix;
  inherit (utils) mkPickKeymap mkExtraPickKeymap mkMiniKeymap;

  # Generate visit file keymaps (<leader>1-4) for quick harpoon-style file access
  visitKeymaps = builtins.genList (i: {
    mode = "n";
    key = "<leader>${toString (i + 1)}";
    action.__raw =
      if i == 0 then
        "function() require('mini.visits').iterate_paths('first', nil, { filter = 'harpoon' }) end"
      else
        "function() require('mini.visits').iterate_paths('forward', nil, { filter = 'harpoon', n_times = ${toString (i + 1)}, wrap = false }) end";
    options.desc = "Visit file ${toString (i + 1)}";
  }) 4;
in
{
  plugins.mini.modules = {
    # ══════════════════════════════════════════════════════════════════════════
    # Pick (replaces telescope)
    # ══════════════════════════════════════════════════════════════════════════
    # Fuzzy finder with centered floating window
    pick = {
      mappings = {
        choose_marked = "<C-q>";
        mark = "<C-x>";
        mark_all = "<C-a>";
      };
      options = {
        use_cache = true;
      };
      window = {
        config.__raw = ''
          function()
            local height = math.floor(0.618 * vim.o.lines)
            local width = math.floor(0.618 * vim.o.columns)
            return {
              anchor = 'NW',
              height = height,
              width = width,
              row = math.floor(0.5 * (vim.o.lines - height)),
              col = math.floor(0.5 * (vim.o.columns - width)),
            }
          end
        '';
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Extra (additional pickers for mini.pick)
    # ══════════════════════════════════════════════════════════════════════════
    extra = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Fuzzy (fuzzy matching for mini.pick)
    # ══════════════════════════════════════════════════════════════════════════
    fuzzy = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Files (replaces oil)
    # ══════════════════════════════════════════════════════════════════════════
    # File tree with vim-like navigation (h/l for out/in)
    files = {
      mappings = {
        close = "q";
        go_in = "l";
        go_in_plus = "<CR>";
        go_out = "h";
        go_out_plus = "H";
        reset = "<BS>";
        reveal_cwd = "@";
        show_help = "g?";
        synchronize = "=";
        trim_left = "<";
        trim_right = ">";
      };
      options = {
        use_as_default_explorer = true;
      };
      windows = {
        preview = true;
        width_focus = 50;
        width_nofocus = 15;
        width_preview = 40;
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Visits (replaces harpoon)
    # ══════════════════════════════════════════════════════════════════════════
    # File bookmarking system with auto-tracking
    visits = {
      store = {
        autowrite = true;
      };
      track = {
        event = "BufEnter";
        delay = 1000;
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Sessions (session management)
    # ══════════════════════════════════════════════════════════════════════════
    sessions = {
      autowrite = true;
      autoread = false;
      file = "";
      directory.__raw = "vim.fn.stdpath('data') .. '/sessions'";
    };
  };

  keymaps = [
    # mini.pick keymaps (replacing telescope)
    (mkPickKeymap "f" "files({ tool = 'git' })" "Find Git Files")
    (mkPickKeymap "o" "buf_lines()" "Find in Current Buffer")
    (mkPickKeymap "g" "grep_live()" "Live Grep")
    (mkPickKeymap "b" "buffers()" "Find Buffers")
    (mkPickKeymap "h" "help()" "Find Help Tags")
    (mkExtraPickKeymap "r" "registers" "Find Registers")
    {
      mode = "n";
      key = "<leader>ft";
      action.__raw = "function() require('mini.pick').builtin.grep_live({ pattern = 'TODO|FIXME|HACK|NOTE' }) end";
      options.desc = "Search TODO comments";
    }
    (mkPickKeymap "/" "resume()" "Resume last picker")

    # mini.files keymaps (replacing oil)
    {
      mode = "n";
      key = "<leader>e";
      action.__raw = "function() require('mini.files').open(vim.fn.getcwd(), true) end";
      options = {
        desc = "Toggle file explorer (project root)";
        silent = true;
      };
    }
    {
      mode = "n";
      key = "<leader>E";
      action.__raw = "function() require('mini.files').open(vim.api.nvim_buf_get_name(0), true) end";
      options = {
        desc = "Toggle file explorer (current file)";
        silent = true;
      };
    }

    # mini.visits keymaps (replacing harpoon)
    # ha/hh/hr keymaps for harpoon muscle memory
    {
      mode = "n";
      key = "<leader>ha";
      action.__raw = "function() require('mini.visits').add_label('harpoon') end";
      options.desc = "Harpoon: add file";
    }
    {
      mode = "n";
      key = "<leader>hh";
      action.__raw = "function() require('mini.extra').pickers.visit_labels({ filter = 'harpoon' }) end";
      options.desc = "Harpoon: toggle menu";
    }
    {
      mode = "n";
      key = "<leader>hr";
      action.__raw = "function() require('mini.visits').remove_label('harpoon') end";
      options.desc = "Harpoon: remove file";
    }

    # mini.sessions keymaps
    (mkMiniKeymap "n" "<leader>ss" "sessions" "write" "Save session")
    (mkMiniKeymap "n" "<leader>sl" "sessions" "select" "Load session")
    (mkMiniKeymap "n" "<leader>sd" "sessions" "delete" "Delete session")
  ]
  ++ visitKeymaps;
}
