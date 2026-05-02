# ════════════════════════════════════════════════════════════════════════════════
# Mini.nvim Editing Enhancement Modules
# ════════════════════════════════════════════════════════════════════════════════
# Text manipulation and editing utilities: pairs, surround, jump, ai textobjects,
# move, splitjoin, operators, align, comment, and animate.
# ════════════════════════════════════════════════════════════════════════════════

_: {
  plugins.mini.modules = {
    # ══════════════════════════════════════════════════════════════════════════
    # Pairs (replaces nvim-autopairs)
    # ══════════════════════════════════════════════════════════════════════════
    pairs = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Surround (replaces vim-sandwich)
    # ══════════════════════════════════════════════════════════════════════════
    # sa=add, sd=delete, sf=find, sF=find_left, sh=highlight, sr=replace, sn=update_n_lines
    surround = {
      mappings = {
        add = "sa";
        delete = "sd";
        find = "sf";
        find_left = "sF";
        highlight = "sh";
        replace = "sr";
        update_n_lines = "sn";
      };
      n_lines = 50;
      respect_selection_type = true;
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Jump (replaces hop for f/F/t/T)
    # ══════════════════════════════════════════════════════════════════════════
    # Enhanced single-character jump with repeat support
    jump = {
      mappings = {
        forward = "f";
        backward = "F";
        forward_till = "t";
        backward_till = "T";
        repeat_jump = ";";
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Jump2d (for 2-character jumps)
    # ══════════════════════════════════════════════════════════════════════════
    # EasyMotion-style word jumping with visual dimming
    jump2d = {
      spotter = {
        __raw = "require('mini.jump2d').builtin_opts.word_start.spotter";
      };
      view = {
        dim = true;
        n_steps_ahead = 2;
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # AI (textobjects for treesitter)
    # ══════════════════════════════════════════════════════════════════════════
    # Enhanced around/inside textobjects: o=block, f=function, c=class, t=tag, d=digit, e=word, u=function_call
    ai = {
      n_lines = 500;
      custom_textobjects.__raw = ''
        {
          o = require('mini.ai').gen_spec.treesitter({
            a = { '@block.outer', '@conditional.outer', '@loop.outer' },
            i = { '@block.inner', '@conditional.inner', '@loop.inner' },
          }, {}),
          f = require('mini.ai').gen_spec.treesitter({ a = '@function.outer', i = '@function.inner' }, {}),
          c = require('mini.ai').gen_spec.treesitter({ a = '@class.outer', i = '@class.inner' }, {}),
          t = { '<([%p%w]-)%f[^<%w][^<>]->.-</%1>' },
          d = { '%f[%d]%d+' },
          e = { '%u[%l%d]+%f[^%l%d]', '%f[%S][%l%d]+%f[^%l%d]', '%f[%P][%l%d]+%f[^%l%d]', '^[%l%d]+%f[^%l%d]' },
          u = require('mini.ai').gen_spec.function_call(),
          U = require('mini.ai').gen_spec.function_call({ name_pattern = '[%w_]' }),
        }
      '';
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Move (move lines and selections)
    # ══════════════════════════════════════════════════════════════════════════
    # Using Shift modifier to avoid conflict with LSP (Alt+j/k)
    move = {
      mappings = {
        left = "<M-h>";
        right = "<M-l>";
        down = "<M-S-j>";
        up = "<M-S-k>";
        line_left = "<M-h>";
        line_right = "<M-l>";
        line_down = "<M-S-j>";
        line_up = "<M-S-k>";
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Splitjoin (split/join arguments)
    # ══════════════════════════════════════════════════════════════════════════
    splitjoin = {
      mappings = {
        toggle = "gS";
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Operators (additional operators)
    # ══════════════════════════════════════════════════════════════════════════
    # g==evaluate, gx=exchange, gm=multiply, gr=replace, gs=sort
    operators = {
      evaluate = {
        prefix = "g=";
      };
      exchange = {
        prefix = "gx";
      };
      multiply = {
        prefix = "gm";
      };
      replace = {
        prefix = "gr";
      };
      sort = {
        prefix = "gs";
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Align (align text by delimiter)
    # ══════════════════════════════════════════════════════════════════════════
    align = { };

    # ══════════════════════════════════════════════════════════════════════════
    # Comment (code commenting)
    # ══════════════════════════════════════════════════════════════════════════
    comment = {
      mappings = {
        comment = "gc";
        comment_line = "gcc";
        comment_visual = "gc";
        textobject = "gc";
      };
    };

    # ══════════════════════════════════════════════════════════════════════════
    # Animate (smooth animations)
    # ══════════════════════════════════════════════════════════════════════════
    # cursor/resize disabled for performance (constant CPU overhead)
    animate = {
      cursor = {
        enable = false;
      };
      scroll = {
        enable = true;
        timing.__raw = "require('mini.animate').gen_timing.linear({ duration = 80, unit = 'total' })";
        subscroll.__raw = ''
          require('mini.animate').gen_subscroll.equal({
            predicate = function(total_scroll) return math.abs(total_scroll) > 5 end
          })
        '';
      };
      resize = {
        enable = false;
      };
      open = {
        enable = true;
        timing.__raw = "require('mini.animate').gen_timing.linear({ duration = 120, unit = 'total' })";
      };
      close = {
        enable = true;
        timing.__raw = "require('mini.animate').gen_timing.linear({ duration = 120, unit = 'total' })";
      };
    };
  };

  keymaps = [
    # mini.jump2d keymap - using gj to avoid conflict with window navigation (sh/sj/sk/sl)
    {
      mode = [
        "n"
        "x"
        "o"
      ];
      key = "gj";
      action.__raw = "function() require('mini.jump2d').start(require('mini.jump2d').builtin_opts.single_character) end";
      options.desc = "Jump to character (2d)";
    }
  ];
}
